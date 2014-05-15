module Network.TeleHash.SwitchUtils
  (
    util_loadjson
  , util_sendall
  , ipv4SendSocket
  , util_server
  , util_readone

  , util_chan_popall

  , util_chunk_out
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
  -- logT $ "util_sendall entered"
  mp <- switch_sending
  -- logT $ "util_sendall:switch_sending returned:" ++ show mp
  case mp of
    Nothing -> return ()
    Just p -> do
      case tLp p of
        Nothing -> do
          assert False undefined
        Just lp -> do
          case (tOut p) of
            (PIPv4 _) -> do
              logT $ "sendall:sending " ++ show (tOut p)
              ipv4Send (tOut p) lp Nothing
            _ -> do
              -- logT $ "sendall:not sending " ++ show (tOut p)
              return ()
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

-- ---------------------------------------------------------------------

util_server :: Int -> Int -> IO Socket
util_server port ms = do
  -- Establish a socket for communication
  --sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  sock <- NS.socket NS.AF_INET NS.Datagram defaultProtocol

  -- We want to listen on all interfaces (0.0.0.0)
  bindAddr <- NS.inet_addr "0.0.0.0"
  NS.bindSocket sock (NS.SockAddrInet (NS.PortNum $ fromIntegral port) bindAddr)

  -- setSocketOption sock RecvTimeOut ms

  socketName <- NS.getSocketName sock
  warningM "Controller" ("server listening " ++ (show socketName))
  return sock

{-
int util_server(int port, int ms)
{
  int sock;
  struct	sockaddr_in sad;
  struct timeval tv;

  // create a udp socket
  if( (sock = socket(PF_INET, SOCK_DGRAM, IPPROTO_UDP) ) < 0 )
  {
    printf("failed to create socket\n");
    return -1;
  }
  memset(&sad,0,sizeof(sad));
  sad.sin_family = AF_INET;
  sad.sin_port = htons(port);
  sad.sin_addr.s_addr = htonl(INADDR_ANY);
  if (bind (sock, (struct sockaddr *)&sad, sizeof(sad)) < 0)
  {
    perror("bind failed");
    return -1;
  }
  tv.tv_sec = ms/1000;
  tv.tv_usec = (ms%1000)*1000;
  if (setsockopt(sock, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv)) < 0)
  {
    perror("setsockopt");
    return -1;
  }
  return sock;
}

-}
 -- --------------------------------------------------------------------

-- |chunk the packet out
-- thtp uses "end" for the doneFlag, chat uses "done" to be able to
-- re-use the channel
util_chunk_out :: Uid -> TxTelex -> String -> TeleHash ()
util_chunk_out cid req doneFlag = do
  c <- getChan cid
  logT $ "util_chunk_out:sending " ++ showChan c ++ ":" ++ showJson (tJs req)
  lpraw <- packet_raw req
  let LP raw = lpraw
  logT $ "util_chunk_out:raw " ++ show raw

  -- send until everything is done

  let
    sendChunks toSend = do
      mchunk <- chan_packet cid
      case mchunk of
        Nothing -> do
          logT $ "thtp_send:could not make chan_packet"
          return () -- TODO: back pressure
        Just chunk -> do
          space <- packet_space chunk
          let len = BC.length toSend
          let space2 = if space > len
                         then len
                         else space
          let chunk2 = packet_body chunk (BC.take space2 toSend)
              rest = BC.drop space2 toSend
              restLen = BC.length rest
              chunk3 = if restLen > 0
                         then chunk2
                         else packet_set chunk2 doneFlag True
          logT $ "thtp_send:sending " ++ show chunk3
          chan_send cid chunk3
          if restLen > 0
            then sendChunks rest
            else return ()

  sendChunks raw
