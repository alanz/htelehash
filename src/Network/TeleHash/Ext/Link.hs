module Network.TeleHash.Ext.Link
  (
    ext_link
  , link_hn
  ) where

import Control.Monad.State

import Network.TeleHash.Types
import Network.TeleHash.Utils
import Network.TeleHash.SwitchApi
import Network.TeleHash.SwitchUtils

import qualified Data.Map as Map
import qualified Data.Set as Set

-- ---------------------------------------------------------------------

ext_link :: TChan -> TeleHash ()
ext_link c = do
  logT $ "ext_link entered for:" ++ show (chId c, chUid c)

  let
    respFunc p = do
      -- always respond/ack, except if there is an error
      case packet_get_str p "err" of
        Just _ -> return ()
        Nothing -> do
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
