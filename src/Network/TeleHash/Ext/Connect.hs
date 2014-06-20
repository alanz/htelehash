module Network.TeleHash.Ext.Connect
  (
  ext_connect
  ) where

-- import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Prelude hiding (id, (.), head, either)

import Network.TeleHash.Hn
import Network.TeleHash.Types
import Network.TeleHash.Utils
import Network.TeleHash.SwitchApi
import Network.TeleHash.SwitchUtils


-- ---------------------------------------------------------------------

ext_connect :: Uid -> TeleHash ()
ext_connect cid = do
  c <- getChan cid
  logT $ "ext_connect entered for:" ++ show (chId c, chUid c)
  util_chan_popall c (Just (\p -> do
    logT $ "ext_connect paths:" ++ showJson (packet_get p "paths")
    mhn <- hn_fromjson p
    logT $ "ext_connect:connect " ++ show mhn
    case mhn of
      Nothing -> return ()
      Just hn -> switch_open hn Nothing
   ))

{-
//unhandled channel packet {"from":{"3a":"459e76744a5a1e7f5f59e97f57f6524a8a84731917fbbdf746bdbfd2c4e2b4e7","2a":"81b441c63f11f6591ea89467a562077c73ed33bd6095349456eaca3893bb3ef9","1a":"d4e703ff112afeed53f5800511a33f8088385098"},"paths":[{"type":"ipv4","ip":"127.0.0.1","port":58919}],"type":"connect","c":5}

void ext_connect(chan_t c)
{
  packet_t p;
  hn_t hn;
  while((p = chan_pop(c)))
  {
    DEBUG_PRINTF("paths %s",packet_get_str(p,"paths"));
    hn = hn_fromjson(c->s->index,p);
    packet_free(p);
    DEBUG_PRINTF("connect HN %s\n",hn?hn->hexname:"null");
    if(!hn) continue;
    switch_open(c->s, hn, NULL);
  }
}
-}

