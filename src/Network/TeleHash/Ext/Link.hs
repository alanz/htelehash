{-# LANGUAGE OverloadedStrings #-}
module Network.TeleHash.Ext.Link
  (
    ext_link
  ) where

import Control.Exception
import Control.Monad.State
import Data.Maybe
import System.Time

import Network.TeleHash.Dht
import Network.TeleHash.Hn
import Network.TeleHash.SwitchApi
import Network.TeleHash.SwitchUtils
import Network.TeleHash.Types
import Network.TeleHash.Utils
import Network.TeleHash.Ext.Path
import Network.TeleHash.Ext.Seek


import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

-- ---------------------------------------------------------------------

-- |Accept a DHT link
ext_link :: TChan -> TeleHash ()
ext_link c = do
  logT $ "ext_link entered for:" ++ showChan c

  let
    respFunc p = do
      logT $ "ext_link:respFunc:processing " ++ showJson (rtJs p)
      c2 <- getChan (chUid c)
      -- always respond/ack, except if there is an error or end
      let mlp = parseJs (rtJs p) :: Maybe LinkReply
      case mlp of
        Nothing -> do
          logT $ "ext_link:unexpected packet:" ++ show p
          return ()
        Just (LinkReplyErr _err) -> do
          logT $ "ext_link:got err:" ++ show p
          deleteFromDht (chTo c)
          chan_fail (chUid c) Nothing
          return ()
        Just LinkReplyEnd -> do
          logT $ "ext_link:link ended"
          deleteFromDht (chTo c)
          chan_fail (chUid c) Nothing
          return ()
        Just lrp -> do
          hc <- getHN (chTo c)
          now <- io $ getClockTime
          putHN $ hc { hLinkAge = Just now
                     , hLinkChan = Just (chUid c)
                     }
          putChanInHnIfNeeded (chTo c) (chUid c)

          -- send a response if this is a new incoming
          -- if(!chan.sentAt) packet.from.link();
          case chLast c2 of
            Just _ -> do
              -- always respond/ack
              send_keepalive (chUid c2)
            Nothing -> do
              logT $ "ext_link:creating new link to hn:" ++ show (hHashName hc)
              -- TODO: this will creat a new channel, is this what we want?
              void $ link_hn (hHashName hc) (Just $ chUid c2)

          process_link_seed (chUid c2) p lrp

          -- this is a new link, request a path
          logT $ "ext_link:request a path: " ++ show (chTo c2)
          path_send (chTo c2)


  util_chan_popall c (Just respFunc)

  c3 <- getChan (chUid c)
  putChan $ c3 { chHandler = Just link_handler }

-- ---------------------------------------------------------------------

-- |Process an incoming link ack, flagged as a seed, containing
-- possible "see" values
process_link_seed :: Uid -> RxTelex -> LinkReply -> TeleHash ()
process_link_seed cid p lrp = do
  let (isSeed,sees) = case lrp of
        LinkReplyKeepAlive is -> (is,[])
        LinkReplyNormal is sv -> (is,sv)
        _                     -> (False,[])
  c <- getChan cid
  -- Check if we have the current hn in our dht
  insertIntoDht (chTo c)

{-
-- js version does this

  // look for any see and check to see if we should create a link
  if(Array.isArray(packet.js.see)) packet.js.see.forEach(function(address){
    var hn = packet.from.sees(address);
    if(!hn || hn.linked) return;
    if(self.buckets[hn.bucket].length < defaults.link_k) hn.link();
  });

-}

  forM_ sees $ \see -> do
    logT $ "process_link_seed:see=" ++ show see
    sw <- get
    let fields = Text.splitOn "," (Text.pack see)
    case fields of
      (h:_) -> do
        let hn = (HN $ Text.unpack h)
        mhc <- getHNMaybe hn -- have we seen this hashname before?
        -- first make sure the switch has an entry for the hashname
        hc <- hn_get hn
        if isJust (hLinkAge hc)
          then return () -- nothing to do
          else do
            -- create a link to the new bucket
            (_distance,bucket) <- getBucketContentsForHn (hHashName hc)
            if not (Set.member (hHashName hc) bucket) &&
               Set.size bucket <= (swDhtK sw)
                   -- TODO: use the hinted IP:Port if provided
              -- then void $ link_hn (hHashName hc)
              then do
                logT $ "process_link_seed:attempting peer to: " ++ show (hHashName hc,fields)
                case mhc of
                  Nothing -> do -- Not seen before, try to peer
                    peer_send (hHashName hc) (map Text.unpack fields)
                  Just _ -> return ()
                void $ link_hn (hHashName hc) Nothing
              else return ()
      _     -> do
        logT $ "process_link_seed:got junk see:" ++ show see


  -- TODO: check for and process bridges
  return ()

-- ---------------------------------------------------------------------

-- |Set up during first message received on a link, will process all
-- subsequent messages.
link_handler :: Uid -> TeleHash ()
link_handler cid = do
  c <- getChan cid
  logT $ "link_handler:processing " ++ showChan c

  util_chan_popall c $ Just $ \p -> do
    if packet_has_key p "err"
      then do
        logR $ "LINKDOWN " ++ showChanShort c ++ ": " ++ packet_get_str_always p "err"
        deleteFromDht (chTo c)
        -- if this channel was ever active, try to re-start it
        void $ link_hn (chTo c) (Just $ chUid c)
      else return ()

    -- update seed status
    case packet_get p "seed" of
      Nothing -> return ()
      Just (Aeson.Bool isSeed) -> do
        hc <- getHN (chTo c)
        putHN $ hc { hIsSeed = isSeed }
      Just _ -> do
        logT $ "link_handler:got strange seed value for:" ++ show p

    -- only send a response if we've not sent one in a while
    now <- io $ getClockTime
    c2 <- getChan cid
    mSentAt <- case chLast c2 of
      Nothing -> return Nothing
      Just pj -> do
        path <- getPath (chTo c2) pj
        return $ pAtOut path
    if isTimeOut now mSentAt param_link_timeout_secs
      then do
        send_keepalive cid
        return ()
      else return ()

    -- TODO: move this into its own timer thread
    path_send (chTo c)

{-
function inMaintenance(err, packet, chan)
{
  // ignore if this isn't the main link
  if(!packet.from || !packet.from.linked || packet.from.linked != chan) return;
  var self = packet.from.self;
  if(err)
  {
    debug("LINKDOWN",packet.from.hashname,err);
    delete packet.from.linked;
    var index = self.buckets[packet.from.bucket].indexOf(packet.from);
    if(index > -1) self.buckets[packet.from.bucket].splice(index,1);
    // if this channel was ever active, try to re-start it
    if(chan.recvAt) packet.from.link();
    return;
  }

  // update seed status
  packet.from.seed = packet.js.seed;

  // only send a response if we've not sent one in a while
  if((Date.now() - chan.sentAt) > Math.ceil(defaults.link_timer/2)) chan.send({js:{seed:self.seed}});
}
-}
-- ---------------------------------------------------------------------

send_keepalive :: Uid -> TeleHash ()
send_keepalive cid = do
  c <- getChan cid
  mreply <- chan_packet (chUid c) True
  case mreply of
    Nothing -> do
      c3 <- getChan (chUid c)
      logT $ "ext_link:send_keepalive:could not create a channel packet for " ++ show c3
    Just reply -> do
      sw <- get
      let reply2 = packet_set reply "seed" (swIsSeed sw)
      chan_send (chUid c) reply2
