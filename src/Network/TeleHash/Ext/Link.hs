module Network.TeleHash.Ext.Link
  (
    ext_link
  , link_hn
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

ext_link :: TChan -> TeleHash ()
ext_link c = do
  logT $ "ext_link entered for:" ++ show (chId c, chUid c)
  util_chan_popall c Nothing
  -- always respond/ack
  reply <- chan_packet c
  chan_send c (gfromJust "ext_link" reply)
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
               , lMeshed = Set.empty
               , lSeeding = False
               , lLinks = Map.empty
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
  mp <- chan_packet c
  case mp of
    Nothing -> return Nothing
    Just p -> do
      let p2 = if lSeeding l
                 then packet_set p "seed" True
                 else p
      chan_send c p2
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
