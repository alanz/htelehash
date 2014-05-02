module Network.TeleHash.SwitchUtils
  (
    util_loadjson
  , util_sendall
  , ipv4SendSocket
  , util_readone

  , util_chan_popall
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

import Network.TeleHash.Bucket
import Network.TeleHash.Crypt
import Network.TeleHash.Paths
import Network.TeleHash.Packet
import Network.TeleHash.Types
import Network.TeleHash.Switch
import Network.TeleHash.SwitchApi
import Network.TeleHash.Utils

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

-- | load id.json and seeds.json from the current directory
util_loadjson :: TeleHash ()
util_loadjson = do
  seeds <- bucket_load "./data/seeds.json.local"

  forM_ (Set.elems seeds) $ \seed -> do
    switch_seed seed

{-
int util_loadjson(switch_t s)
{
  bucket_t seeds;
  hn_t hn;
  int i = 0;

  if(switch_init(s, util_file2packet("id.json"))) return -1;

  seeds = bucket_load(s->index, "seeds.json");
  if(!seeds) return -1;
  while((hn = bucket_get(seeds, i)))
  {
    switch_seed(s,hn);
    i++;
  }
  bucket_free(seeds);
  return 0;
}
-}

-- ---------------------------------------------------------------------

-- | Send all queued packets from the switch
util_sendall :: Socket -> TeleHash ()
util_sendall sock = do
  mp <- switch_sending
  -- logT $ "switch_sending returned:" ++ show mp
  case mp of
    Nothing -> return ()
    Just p -> do
      case tChain p of
        Nothing -> do
          assert False undefined
        Just lp -> do
          case (tOut p) of
            (PIPv4 _) -> do
              logT $ "sendall:sending " ++ show (tOut p)
              ipv4Send (tOut p) lp Nothing
            _ -> do
              logT $ "sendall:not sending " ++ show (tOut p)
      util_sendall sock



{-
void util_sendall(switch_t s, int sock)
{
  struct	sockaddr_in sa;
  packet_t p;

  while((p = switch_sending(s)))
  {
    if(util_cmp(p->out->type,"ipv4")!=0)
    {
      packet_free(p);
      continue;
    }
    DEBUG_PRINTF("<<< %s packet %d %s",p->json_len?"open":"line",packet_len(p),path_json(p->out));
    path2sa(p->out, &sa);
    if(sendto(sock, packet_raw(p), packet_len(p), 0, (struct sockaddr *)&sa, sizeof(sa))==-1) printf("sendto failed\n");
    packet_free(p);
  }
}

-}

-- | Send the body of the packet in the telex. It is already encrypted
ipv4SendSocket :: Socket -> PathJson -> LinePacket -> Maybe HashName -> TeleHash ()
ipv4SendSocket sock path (LP msg) _ = do
  addr <- io (addrFromHostPort (show $ gfromJust "ipv4Send.1" $ pjsonIp path)
                               (show $ gfromJust "ipv4Send.2" $ pjsonPort path))

  io (sendDgram sock msg addr)
  return ()

-- ---------------------------------------------------------------------

util_readone :: Socket -> PathJson -> TeleHash Bool
util_readone sock inPath = do
  (msg,rinfo) <- io (SB.recvFrom sock 1000)
  recvTelex msg rinfo
  return True

{-
int util_readone(switch_t s, int sock, path_t in)
{
  unsigned char buf[2048];
  struct	sockaddr_in sa;
  int len, salen;
  packet_t p;

  salen = sizeof(sa);
  memset(&sa,0,salen);
  len = recvfrom(sock, buf, sizeof(buf), 0, (struct sockaddr *)&sa, (socklen_t *)&salen);

  if(len < 0 && errno != EAGAIN && errno != EWOULDBLOCK) return -1;
  if(len <= 0) return 0;

  sa2path(&sa,in); // inits ip/port from sa
  p = packet_parse(buf,len);
  DEBUG_PRINTF(">>> %s packet %d %s", p->json_len?"open":"line", len, path_json(in));
  switch_receive(s,p,in);
  return 0;
}
-}

-- ---------------------------------------------------------------------

util_chan_popall :: TChan -> Maybe (RxTelex -> TeleHash ()) -> TeleHash ()
util_chan_popall c mfn = do
  mp <- chan_pop (chUid c)
  case mp of
    Nothing -> return ()
    Just p -> do
      case mfn of
        Nothing -> do
          logT $ "util_chan_popall:unhandled channel packet:"  ++ showJson (rtJs p)
        Just fn -> fn p
      util_chan_popall c mfn
{-
      while((p = chan_pop(c)))
      {
        printf("unhandled channel packet %.*s\n", p->json_len, p->json);
        packet_free(p);
      }

-}
