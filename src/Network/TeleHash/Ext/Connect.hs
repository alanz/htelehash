module Network.TeleHash.Ext.Connect
  (
  ext_connect
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Crypto.Random
import Data.Aeson (object,(.=), (.:), (.:?) )
import Data.Aeson.Encode
import Data.Aeson.Types
import Data.Bits
import Data.Char
import Data.IP
import Data.List
import Data.Maybe
import Data.String.Utils
import Data.Text.Lazy.Builder
import Data.Typeable
import Data.Word
import Network.BSD
import Network.Socket
import Prelude hiding (id, (.), head, either, catch)
import System.IO
import System.Log.Handler.Simple
import System.Log.Logger
import System.Time

import Network.TeleHash.Convert
import Network.TeleHash.Crypt
import Network.TeleHash.Hn
import Network.TeleHash.Packet
import Network.TeleHash.Path
import Network.TeleHash.Paths
import Network.TeleHash.Types
import Network.TeleHash.Utils
import Network.TeleHash.SwitchApi
import Network.TeleHash.SwitchUtils

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.PubKey.DH as DH
import qualified Crypto.Types.PubKey.ECDSA as ECDSA
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as SB

-- ---------------------------------------------------------------------

ext_connect :: TChan -> TeleHash ()
ext_connect c = do
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

