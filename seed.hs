
-- based on seeds.c in telehash-c

import Control.Concurrent
import Control.Exception
import Control.Monad.State
import System.IO
import System.Log.Handler.Simple
import System.Log.Logger

import Network.TeleHash.Bucket
import Network.TeleHash.Ext.Connect
import Network.TeleHash.Ext.Link
import Network.TeleHash.Ext.Path
import Network.TeleHash.Crypt
import Network.TeleHash.Paths
import Network.TeleHash.Periodic
import Network.TeleHash.Switch
import Network.TeleHash.SwitchApi
import Network.TeleHash.Types
import Network.TeleHash.Utils
import Network.TeleHash.SwitchUtils

import qualified Network.Socket.ByteString as SB
import qualified Data.Set as Set

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
  util_loadjson

  sw <- get
  let (SocketHandle sock) = gfromJust "app" $ swH sw

  logT $ "loaded hashname" ++ show (swId sw)

  --  create/send a ping packet
  c <- chan_new (ghead "app" $ Set.elems (swSeeds sw)) "link" Nothing
  p <- chan_packet (chUid c) True
  chan_send (chUid c) (gfromJust "app.2" p)
  util_sendall sock

  let inPath = PNone
  let loop = do
        void $ util_readone sock inPath
        switch_loop
        rx_loop
        util_sendall sock
        loop

      rx_loop = do
        mc <- switch_pop
        case mc of
          Nothing -> return ()
          Just cid -> do
            c <- getChan cid
            logT $ "channel active " ++ show (chState c,chUid c,chTo c)
            case chType c of
              "link" -> ext_link cid
              "path" -> ext_path cid
              "connect" -> ext_connect cid
              typ -> do
                logT $ "not processing channel type:" ++ typ
                util_chan_popall c Nothing
            if chState c == ChanEnded
              then chan_free c
              else return ()
            rx_loop

  -- Start the loop going
  loop

{-
int main(void)
{
  switch_t s;
  chan_t c;
  packet_t p;
  path_t in;
  int sock;

  crypt_init();
  s = switch_new(0);

  if(util_loadjson(s) != 0 || (sock = util_server(0,1000)) <= 0)
  {
    printf("failed to startup %s or %s\n", strerror(errno), crypt_err());
    return -1;
  }

  printf("loaded hashname %s\n",s->id->hexname);

  // create/send a ping packet
  c = chan_new(s, bucket_get(s->seeds, 0), "link", 0);
  p = chan_packet(c);
  chan_send(c, p);
  util_sendall(s,sock);

  in = path_new("ipv4");
  while(util_readone(s, sock, in) == 0)
  {
    switch_loop(s);

    while((c = switch_pop(s)))
    {
      printf("channel active %d %s %s\n",c->state,c->hexid,c->to->hexname);
      if(util_cmp(c->type,"connect") == 0) ext_connect(c);
      if(util_cmp(c->type,"link") == 0) ext_link(c);
      if(util_cmp(c->type,"path") == 0) ext_path(c);
      while((p = chan_pop(c)))
      {
        printf("unhandled channel packet %.*s\n", p->json_len, p->json);
        packet_free(p);
      }
      if(c->state == CHAN_ENDED) chan_free(c);
    }

    util_sendall(s,sock);
  }

  perror("exiting");
  return 0;
}

-}
