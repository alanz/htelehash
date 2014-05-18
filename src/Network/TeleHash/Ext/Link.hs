{-# LANGUAGE OverloadedStrings #-}
module Network.TeleHash.Ext.Link
  (
    ext_link
  , link_hn
  ) where

import Control.Exception
import Control.Monad.State
import Data.Maybe

import Network.TeleHash.Dht
import Network.TeleHash.Hn
import Network.TeleHash.SwitchApi
import Network.TeleHash.SwitchUtils
import Network.TeleHash.Types
import Network.TeleHash.Utils

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

-- ---------------------------------------------------------------------

ext_link :: TChan -> TeleHash ()
ext_link c = do
  logT $ "ext_link entered for:" ++ show (chId c, chUid c)

  let
    respFunc p = do
      -- always respond/ack, except if there is an error or end
      let merr = packet_get_str p "err"
          mend = packet_get_str p "end"
      if any isJust [merr,mend]
        then return ()
        else do
          let mlp = parseJs (rtJs p) :: Maybe LinkReply
          case mlp of
            Nothing -> do
              logT $ "ext_link:unexpected packet:" ++ show p
              return ()
            Just (LinkReplyErr _err) -> do
              logT $ "ext_link:got err:" ++ show p
              return ()
            Just LinkReplyEnd -> do
              logT $ "ext_link:link ended"
              return ()
            Just lrp -> do
              process_link_seed (chUid c) p lrp

          -- always respond/ack
          reply <- chan_packet (chUid c) True
          chan_send (chUid c) (gfromJust "ext_link" reply)

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
  if isSeed
    then insertIntoDht (chTo c)
    else deleteFromDht (chTo c)

  forM_ sees $ \see -> do
    sw <- get
    let fields = Text.splitOn "," (Text.pack see)
    case fields of
      (h:_) -> do
        -- first make sure the switch has an entry for the hashname
        hc <- hn_get (HN $ Text.unpack h)
        if hIsLinked hc
          then return () -- nothing to do
          else do
            -- create a link to the new bucket
            (_distance,bucket) <- getBucketContentsForHn (hHashName hc)
            if not (Set.member (hHashName hc) bucket) &&
               Set.size bucket <= (swDhtK sw)
                   -- TODO: use the hinted IP:Port if provided
              then void $ link_hn (hHashName hc)
              else return ()
      _     -> do
        logT $ "process_link_seed:got junk see:" ++ show see


  -- TODO: check for and process bridges
  return ()

-- ---------------------------------------------------------------------

link_get :: TeleHash Link
link_get = do
  sw <- get
  case swLink sw of
    Nothing -> do
      let l = Link
               { lMeshing = False
               , lMeshed  = Set.empty
               , lSeeding = False
               , lLinks   = Map.empty
               , lBuckets = []
               }
      put $ sw {swLink = Just l}
      return l
    Just l -> return l

{-
link_t link_get(switch_t s)
{
  link_t l;
  l = xht_get(s->index,"link");
  return l ? l : link_new(s);
}
-}

-- ---------------------------------------------------------------------

-- |create/fetch/maintain a link to this hn
link_hn :: HashName -> TeleHash (Maybe ChannelId)
link_hn hn = do
  l <- link_get
  c <- chan_new hn "link" Nothing
  mp <- chan_packet (chUid c) True
  case mp of
    Nothing -> return Nothing
    Just p -> do
      let p2 = if lSeeding l
                 then packet_set p "seed" True
                 else p
      chan_send (chUid c) p2
      return $ Just (chId c)

{-
// create/fetch/maintain a link to this hn
chan_t link_hn(switch_t s, hn_t h)
{
  chan_t c;
  packet_t p;
  link_t l = link_get(s);
  if(!s || !h) return NULL;

  c = chan_new(s, h, "link", 0);
  p = chan_packet(c);
  if(l->seeding) packet_set(p,"seed","true",4);
  chan_send(c, p);
  return c;
}
-}

-- ---------------------------------------------------------------------
