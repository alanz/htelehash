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
-- This should only be called for the first packet received on a given
-- link channel.
ext_link :: TChan -> TeleHash ()
ext_link c = do
  logR $ "ext_link entered for:" ++ showChan c
  let
    respFunc p = do
      logR $ "ext_link:respFunc:processing " ++ showJson (rtJs p)
      c2 <- getChan (chUid c)

      let mlp = parseJs (rtJs p) :: Maybe LinkReply
      case mlp of
        Nothing -> do
          logR $ "ext_link:unexpected packet:" ++ show p
          return ()
        Just (LinkReplyErr _err) -> do

          logR $ "ext_link:got err:" ++ show p
          deleteFromDht (chTo c)
          chan_fail (chUid c) Nothing
          return ()
        Just LinkReplyEnd -> do
          logR $ "ext_link:link ended"
          deleteFromDht (chTo c)
          chan_fail (chUid c) Nothing
          return ()
        Just lrp -> do

          -- add in this link
          hc <- getHN (chTo c)
          now <- io $ getClockTime
          putHN $ hc { hLinkAge = Just now
                     , hLinkChan = Just (chUid c)
                     }
          putChanInHnIfNeeded (chTo c) (chUid c)

          logT $ "ext_link:inserting into dht:" ++ show (chTo c)
          -- Check if we have the current hn in our dht
          insertIntoDht (chTo c)

          -- send a response if this is a new incoming
          -- if(!chan.sentAt) packet.from.link();
          case chLast c2 of
            Just _ -> do
              -- send_keepalive (chUid c2)
              logT $ "ext_link:c2=" ++ show c2
              assert False undefined
              return ()
            Nothing -> do
              logT $ "ext_link:creating new link to hn:" ++ show (hHashName hc)
              void $ link_hn (hHashName hc) (Just $ chUid c2)

          process_link_seed (chUid c2) p lrp

          -- this is a new link, request a path
          -- logT $ "ext_link:request a path: " ++ show (chTo c2)
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

{-
-- js version does this

  // look for any see and check to see if we should create a link
  if(Array.isArray(packet.js.see)) packet.js.see.forEach(function(address){
    var hn = packet.from.sees(address);
    if(!hn || hn.linked) return;
    if(self.buckets[hn.bucket].length < defaults.link_k) hn.link();
  });

-}

  -- let sees1 = []
  logT $ "process_link_seed:PROCESSING SEES:" ++ show sees
  forM_ sees $ \see -> do
    logT $ "process_link_seed:see=" ++ show see
    sw <- get
    let fields = Text.splitOn "," (Text.pack see)
    mhn <- hn_fromaddress (map Text.unpack fields) (chTo c)
    case mhn of
      Just hn -> do
        hc <- hn_get hn
        if isJust (hLinkAge hc)
          then do
            logT $ "process_link_seed:isJust hLinkAge for" ++ show (hHashName hc)
            return () -- nothing to do
          else do
            -- create a link to the new bucket
            (_distance,bucket) <- getBucketContentsForHn (hHashName hc)
            if not (Set.member (hHashName hc) bucket) &&
               Set.size bucket <= (swDhtK sw)
              then do
                logT $ "process_link_seed:creating new link to:" ++ show (hHashName hc)
                void $ link_hn (hHashName hc) Nothing
              else do
                logT $ "process_link_seed:NOT creating new link to:" ++ show (hHashName hc,_distance,bucket,swDhtK sw)
                return ()
      Nothing -> do
        logT $ "process_link_seed:got junk see:" ++ show see


  -- TODO: check for and process bridges
  return ()

-- ---------------------------------------------------------------------

{-
  // track other hashnames this one sees
  hn.sees = function(address)
  {
    if(typeof address != "string") warn("invalid see address",address,hn.hashname);
    if(typeof address != "string") return false;
    var parts = address.split(",");
    if(!self.isHashname(parts[0]) || parts[0] == self.hashname) return false;
    var see = self.whois(parts[0]);
    if(!see) return false;
    // save suggested path if given/valid
    if(parts.length >= 4 && parts[2].split(".").length == 4 && parseInt(parts[3]) > 0) see.pathGet({type:"ipv4",ip:parts[2],port:parseInt(parts[3])});
    if(!see.vias) see.vias = {};
    // save suggested csid if we don't know one yet
    see.vias[hn.hashname] = see.cisd || parts[1];
    return see;
  }

-}

-- ---------------------------------------------------------------------

-- |Set up during first message received on a link, will process all
-- subsequent messages.
link_handler :: Uid -> TeleHash ()
link_handler cid = do
  c <- getChan cid
  logT $ "link_handler:processing " ++ showChan c

  util_chan_popall c $ Just $ \p -> do
    -- ignore if this isn't the main link
    -- if(!packet.from || !packet.from.linked || packet.from.linked != chan) return;
    from <- getHN (chTo c)
    if not (isJust (hLinkChan from) && fromJust (hLinkChan from) == (chUid c))
      then do return ()
      else do
        if packet_has_key p "err"
          then do
            logR $ "LINKDOWN:" ++ showChanShort c ++ ": " ++ packet_get_str_always p "err"
            deleteFromDht (chTo c)
            -- if this HN was ever active, try to re-start it on a new
            -- channel
            hc <- getHN (chTo c)
            putHN $ hc { hLinkChan = Nothing }
            void $ link_hn (chTo c) Nothing
          else do
            -- update seed status
            case packet_get p "seed" of
              Nothing -> return ()
              Just (Aeson.Bool isSeed) -> do
                logT $ "link_handler:updating hIsSeed for " ++ (show (chTo c,isSeed))
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
