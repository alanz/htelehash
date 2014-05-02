
-- based on ping.c in telehash-c



import Control.Applicative
import Control.Concurrent
import Control.Concurrent
import Control.Exception
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
import Prelude hiding (id, (.), head, either, catch)
import System.IO
import System.IO
import System.Log.Handler.Simple
import System.Log.Handler.Simple
import System.Log.Logger
import System.Log.Logger
import System.Time

import TeleHash.New.Bucket
import TeleHash.New.Crypt
import TeleHash.New.Packet
import TeleHash.New.Paths
import TeleHash.New.Switch
import TeleHash.New.SwitchApi
import TeleHash.New.Types
import TeleHash.New.Types
import TeleHash.New.Utils

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

main = do
  s <- streamHandler stdout DEBUG
  updateGlobalLogger rootLoggerName (setHandlers [s])

  sock <- openSocketIPv4

  -- (ch1,ch2,thread) <- startSwitchThread
  runApp (Just $ SocketHandle sock) app

  threadDelay 3000000


app :: TeleHash ()
app = do
  crypt_init

  switch_init testId

  seeds <- bucket_load "./data/seeds.json"
  -- seeds <- bucket_load "./data/seeds.json.local"
  logT $ "run:seeds=" ++ show seeds
  -- bucket_get seeds 0

  -- seeds = bucket_load(s->index, "seeds.json");
  -- if(!seeds || !bucket_get(seeds, 0))
  -- {
  --   printf("failed to load seeds.json: %s\n", crypt_err());
  --   return -1;
  -- }

  --  create/send a ping packet
  c <- chan_new (gfromJust "app" (bucket_get seeds 0)) "seek" Nothing
  p <- chan_packet c
  sw <- get
  let p2 = packet_set_str (gfromJust "run" p) "seek" (unHN $ swId sw)
  logT $ "run:p2=" ++ show p2
  chan_send c p2
  logT $ "run:chan_send done"

  -- TODO: make this a library utility function
  let sendall = do
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
            sendall

  sendall
  logT $ "run:sendall done"

  sw <- get
  let SocketHandle sock = gfromJust "app" (swH sw)
  (msg,rinfo) <- io (SB.recvFrom sock 1000)
  recvTelex msg rinfo

  sendall
  logT $ "run:sendall 2 done"

  -- io $ threadDelay (5 * onesec) -- give the replies a chance to come in

  let rxall = do
        mc <- switch_pop
        case mc of
          Nothing -> return ()
          Just c2 -> do
            if (chUid c2 == chUid c)
              then do
                logT $ "got pong state " ++ show (chState c2,chTo c2)
              else do
                assert False undefined
            rxall

  rxall
  logT $ "run:rxall done"

{-
  forever $ do
    logT $ "looping.."
    io $ threadDelay (1 * onesec)
    sendall
    rxall
-}


{-

int main(void)
{
  unsigned char buf[2048];
  switch_t s;
  bucket_t seeds;
  chan_t c, c2;
  packet_t p;
  path_t from;
  int sock, len, blen;
  struct        sockaddr_in sad, sa;

  crypt_init();
  s = switch_new(0);

  switch_init(s,util_file2packet("id.json"));
  if(!s->id)
  {
    printf("failed to load id.json: %s\n", crypt_err());
    return -1;
  }
  printf("loaded hashname %s\n",s->id->hexname);

  seeds = bucket_load(s->index, "seeds.json");
  if(!seeds || !bucket_get(seeds, 0))
  {
    printf("failed to load seeds.json: %s\n", crypt_err());
    return -1;
  }

  // create a udp socket
  if( (sock = socket(PF_INET, SOCK_DGRAM, IPPROTO_UDP) ) < 0 )
  {
          printf("failed to create socket\n");
          return -1;
  }
  memset((char *)&sad,0,sizeof(sad));
  memset((char *)&sa,0,sizeof(sa));
  sa.sin_family = sad.sin_family = AF_INET;
  sad.sin_port = htons(0);
  sad.sin_addr.s_addr = htonl(INADDR_ANY);
  if (bind (sock, (struct sockaddr *)&sad, sizeof(sad)) < 0)
  {
          printf("bind failed");
          return -1;
  }

  // create/send a ping packet
  c = chan_new(s, bucket_get(seeds, 0), "seek", 0);
  p = chan_packet(c);
  packet_set_str(p,"seek",s->id->hexname);
  chan_send(c, p);

  while((p = switch_sending(s)))
  {
    if(util_cmp(p->out->type,"ipv4")!=0)
    {
      packet_free(p);
      continue;
    }
    printf("sending %s packet %d %s\n",p->json_len?"open":"line",packet_len(p),path_json(p->out));
    path2sa(p->out, &sa);
    if(sendto(sock, packet_raw(p), packet_len(p), 0, (struct sockaddr *)&sa, sizeof(sa))==-1)
    {
          printf("sendto failed\n");
          return -1;
    }
    packet_free(p);
  }

  from = path_new("ipv4");
  len = sizeof(sa);
  if ((blen = recvfrom(sock, buf, sizeof(buf), 0, (struct sockaddr *)&sa, (socklen_t *)&len)) == -1)
  {
          printf("recvfrom failed\n");
          return -1;
  }
  sa2path(&sa, from); // inits ip/port from sa
  p = packet_parse(buf,blen);
  printf("received %s packet %d %s\n", p->json_len?"open":"line", blen, path_json(from));
  switch_receive(s,p,from);

  while((p = switch_sending(s)))
  {
    if(util_cmp(p->out->type,"ipv4")!=0)
    {
      packet_free(p);
      continue;
    }
    printf("Sending %s packet %d %s\n",p->json_len?"open":"line",packet_len(p),path_json(p->out));
    path2sa(p->out, &sa);
    if(sendto(sock, packet_raw(p), packet_len(p), 0, (struct sockaddr *)&sa, sizeof(sa))==-1)
    {
          printf("sendto failed\n");
          return -1;
    }
    packet_free(p);
  }

  from = path_new("ipv4");
  len = sizeof(sa);
  while((blen = recvfrom(sock, buf, sizeof(buf), 0, (struct sockaddr *)&sa, (socklen_t *)&len)) != -1)
  {
    sa2path(&sa, from); // inits ip/port from sa
    p = packet_parse(buf,blen);
    printf("Received %s packet %d %s\n", p->json_len?"open":"line", blen, path_json(from));
    switch_receive(s,p,from);

    while((c2 = switch_pop(s)))
    {
      if(c2 == c)
      {
        printf("got pong state %d from %s see %s\n",c->state,c->to->hexname,packet_get_str(chan_pop(c),"see"));
        return 0;
      }
      while((p = chan_pop(c)))
      {
        printf("unhandled channel packet %.*s\n", p->json_len, p->json);
        packet_free(p);
      }
    }
  }
  printf("recvfrom failed\n");
  return -1;



  return 0;
}

-}

