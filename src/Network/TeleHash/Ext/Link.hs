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


import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

-- ---------------------------------------------------------------------

ext_link :: TChan -> TeleHash ()
ext_link c = do
  logT $ "ext_link entered for:" ++ showChan c

  let
    respFunc p = do
      logT $ "ext_link:respFunc:processing " ++ showJson (rtJs p)
      c2 <- getChan (chUid c)
      -- always respond/ack, except if there is an error or end
      let merr = packet_get_str p "err"
          mend = packet_get_str p "end"
      if any isJust [merr,mend]
        then do
          chan_fail (chUid c) Nothing
          return ()
        else do
          let mlp = parseJs (rtJs p) :: Maybe LinkReply
          case mlp of
            Nothing -> do
              logT $ "ext_link:unexpected packet:" ++ show p
              return ()
            Just (LinkReplyErr _err) -> do
              logT $ "ext_link:got err:" ++ show p
              deleteFromDht (chTo c)
              return ()
            Just LinkReplyEnd -> do
              logT $ "ext_link:link ended"
              deleteFromDht (chTo c)
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
                  mreply <- chan_packet (chUid c2) True
                  case mreply of
                    Nothing -> do
                      c3 <- getChan (chUid c)
                      logT $ "ext_link:could not create a channel packet for " ++ show c3
                      -- assert False undefined
                    Just reply -> do
                      sw <- get
                      let reply2 = packet_set reply "seed" (swIsSeed sw)
                      chan_send (chUid c2) reply2
                Nothing -> do
                  logT $ "ext_link:creating link to now hn:" ++ show (hHashName hc)
                  -- TODO: this will creat a new channel, is this what we want?
                  void $ link_hn (hHashName hc) (Just $ chUid c2)

              process_link_seed (chUid c2) p lrp


              -- if this is a new link, request a path
              if packet_has_key p "type"
                then do
                  path_send (chTo c2)
                else return ()


  util_chan_popall c (Just respFunc)
{-
void ext_link(chan_t c)
{
  packet_t p;
  while((p = chan_pop(c)))
  {
    DEBUG_PRINTF("TODO link packet %.*s\n", p->json_len, p->json);
    packet_free(p);
  }
  // always respond/ack
  chan_send(c,chan_packet(c));
}
-}

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
