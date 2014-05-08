module Network.TeleHash.Ext.Seek
  (
  seek_auto
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

{-


typedef struct seek_struct
{
  hn_t id;
  int active;
  packet_t note;
} *seek_t;

typedef struct seeks_struct
{
  xht_t active;
} *seeks_t;
-}

-- ---------------------------------------------------------------------

seeks_get :: TeleHash (Map.Map HashName Seek)
seeks_get = do
  sw <- get
  return $ swIndexSeeks sw

{-
seeks_t seeks_get(switch_t s)
{
  seeks_t sks;
  sks = xht_get(s->index,"seeks");
  if(sks) return sks;

  sks = malloc(sizeof (struct seeks_struct));
  memset(sks,0,sizeof (struct seeks_struct));
  sks->active = xht_new(11);
  xht_set(s->index,"seeks",sks);
  return sks;
}
-}

-- ---------------------------------------------------------------------

seek_get :: HashName -> TeleHash Seek
seek_get hn = do
  sks <- seeks_get
  case Map.lookup hn sks of
    Just sk -> return sk
    Nothing -> do
      let sk = Seek { seekId = hn
                    , seekActive = True
                    , seekNote = Nothing
                    }
          sks2 = Map.insert hn sk sks
      sw <- get
      put $ sw { swIndexSeeks = sks2 }
      return sk

{-
seek_t seek_get(switch_t s, hn_t id)
{
  seek_t sk;
  seeks_t sks = seeks_get(s);
  sk = xht_get(sks->active,id->hexname);
  if(sk) return sk;

  sk = malloc(sizeof (struct seek_struct));
  memset(sk,0,sizeof (struct seek_struct));
  sk->id = id;
  xht_set(sks->active,id->hexname,sk);
  return sk;
}
-}

-- ---------------------------------------------------------------------

{-
void peer_handler(chan_t c)
{
  // remove the nat punch path if any
  if(c->arg)
  {
    path_free((path_t)c->arg);
    c->arg = NULL;
  }

  DEBUG_PRINTF("peer handler %s",c->to->hexname);
  // TODO process relay'd packets
}

// csid may be address format
void peer_send(switch_t s, hn_t to, char *address)
{
  char *csid, *ip = NULL, *port;
  packet_t punch = NULL;
  crypt_t cs;
  chan_t c;
  packet_t p;

  if(!address) return;
  if(!(csid = strchr(address,','))) return;
  *csid = 0;
  csid++;
  // optional address ,ip,port for punch
  if((ip = strchr(csid,',')))
  {
    *ip = 0;
    ip++;
  }
  if(!(cs = xht_get(s->index,csid))) return;

  // new peer channel
  c = chan_new(s, to, "peer", 0);
  c->handler = peer_handler;
  p = chan_packet(c);
  packet_set_str(p,"peer",address);
  packet_body(p,cs->key,cs->keylen);

  // send the nat punch packet if ip,port is given
  if(ip && (port = strchr(ip,',')))
  {
    *port = 0;
    port++;
    punch = packet_new();
    c->arg = punch->out = path_new("ipv4"); // free path w/ peer channel cleanup
    path_ip(punch->out,ip);
    path_port(punch->out,atoi(port));
    switch_sendingQ(s,punch);
  }

  chan_send(c, p);
}

void seek_handler(chan_t c)
{
  int i = 0;
  char *address;
  seek_t sk = (seek_t)c->arg;
  packet_t see, p = chan_pop(c);
  if(!sk || !p) return;
  DEBUG_PRINTF("seek response for %s of %.*s",sk->id->hexname,p->json_len,p->json);

  // process see array and end channel
  see = packet_get_packet(p,"see");
  while((address = packet_get_istr(see,i)))
  {
    i++;
    if(strncmp(address,sk->id->hexname,64) == 0) peer_send(c->s, c->to, address);
    // TODO maybe recurse others
  }
  packet_free(see);
  packet_free(p);
  // TODO sk->active-- and check to return note
}
-}

-- ---------------------------------------------------------------------

seek_send = assert False undefined

{-
void seek_send(switch_t s, seek_t sk, hn_t to)
{
  chan_t c;
  packet_t p;
  sk->active++;
  c = chan_new(s, to, "seek", 0);
  c->handler = seek_handler;
  c->arg = sk;
  p = chan_packet(c);
  packet_set_str(p,"seek",sk->id->hexname); // TODO make a prefix
  chan_send(c, p);
}
-}

-- ---------------------------------------------------------------------

-- create a seek to this hn and initiate connect
_seek_auto :: HashName -> TeleHash ()
_seek_auto hn = do
  sk <- seek_get hn
  logT $ "seek_auto:seek connecting " ++ show sk
  -- TODO get near from somewhere
  sw <- get
  let seed = ghead "seek_auto" $ Set.toList (swSeeds sw)

  seek_send sk seed


{-
// create a seek to this hn and initiate connect
void _seek_auto(switch_t s, hn_t hn)
{
  seek_t sk = seek_get(s,hn);
  DEBUG_PRINTF("seek connecting %s",sk->id->hexname);
  // TODO get near from somewhere
  seek_send(s, sk, bucket_get(s->seeds, 0));
}
-}

-- ---------------------------------------------------------------------

seek_auto :: TeleHash ()
seek_auto = do
  sw <- get
  put $ sw {swHandler = Just _seek_auto}

{-
void seek_auto(switch_t s)
{
  s->handler = _seek_auto;
}

void seek_free(switch_t s)
{
  seeks_t sks = seeks_get(s);
  // TODO xht_walk active and free each one
  free(sks);
}
-}

-- ---------------------------------------------------------------------

{-
// just call back note instead of auto-connect
void seek_note(switch_t s, hn_t h, packet_t note)
{

}
-}
