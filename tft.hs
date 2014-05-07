{-# LANGUAGE MultiWayIf #-}

-- based on tft.c in telehash-c

import Control.Concurrent
import Control.Exception
import Control.Monad.State
import Data.List
import Network.Socket
import System.IO
import System.Log.Handler.Simple
import System.Log.Logger

import Network.TeleHash.Bucket
import Network.TeleHash.Ext.Chat
import Network.TeleHash.Ext.Connect
import Network.TeleHash.Ext.Link
import Network.TeleHash.Ext.Peer
import Network.TeleHash.Ext.Seek
import Network.TeleHash.Ext.Thtp
import Network.TeleHash.Ext.Path
import Network.TeleHash.Crypt
import Network.TeleHash.Paths
import Network.TeleHash.Switch
import Network.TeleHash.SwitchApi
import Network.TeleHash.Types
import Network.TeleHash.Utils
import Network.TeleHash.SwitchUtils

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as Set
import qualified Network.Socket.ByteString as SB

-- ---------------------------------------------------------------------

data Input = IConsole String | IUdp BC.ByteString SockAddr
           deriving (Show)

-- ---------------------------------------------------------------------

-- TODO: use System.Console.ReadLine for this
readConsole :: (Chan Input) -> IO ()
readConsole ch = forever $ do
  s <- getLine
  putStrLn $ "readConsole: putting [" ++ s ++ "]"
  writeChan ch (IConsole s)

logg :: String -> String -> IO ()
logg nick str = do
  if str /= ""
    then putStrLn $ str
    else return ()
  putStr $ nick ++ "> "

{-
char nick[16];
void logg(char * format, ...)
{
    char buffer[1024];
    va_list args;
    va_start (args, format);
    vsnprintf (buffer, 1024, format, args);
    if(strlen(buffer))
    {
      printf("\n%s\n%s> ", buffer, nick);
    }else{
      printf("%s> ",nick);
    }
    va_end (args);
}

-}

recvUdpMsg :: (Chan Input) -> Socket -> IO ()
recvUdpMsg ch sock = forever $ do
  (msg,rinfo) <- (SB.recvFrom sock 1000)
  writeChan ch (IUdp msg rinfo)

-- ---------------------------------------------------------------------

main = do
  s <- streamHandler stdout DEBUG
  updateGlobalLogger rootLoggerName (setHandlers [s])

  sock <- util_server 0 100

  -- (ch1,ch2,thread) <- startSwitchThread
  runApp (Just $ SocketHandle sock)  app

  threadDelay 3000000


app :: TeleHash ()
app = do
  crypt_init

  switch_init testId

  seek_auto
  myId <- io myThreadId
  let nick = show myId
  logT $ "nick=" ++ nick

  -- make a dummy thtp response
  let p1 = packet_new (HN "null")
      p2 = packet_set_int p1 "status" 200
      p3 = packet_body p2 (BC.pack "bar\n")
      note = packet_new (HN "null")
      note2 = packet_link (Just note) p3
  thtp_path "/foo" note2

  util_loadjson

  sw <- get
  let (Just (SocketHandle sock)) = swH sw
  logT $ "about to launch threads"
  chInput <- io newChan
  threadConsole <- io $ forkIO (readConsole chInput)
  threadUdp     <- io $ forkIO (recvUdpMsg chInput sock)
  logT $ "threads launched"

  logT $ "loaded hashname " ++ show (swId sw)

  -- new chat, must be after-init
  mchat <- chat_get (Just "tft")
  let chat = (gfromJust "app" mchat)
  chat_add chat "*" "invited"
  mp <- chat_message chat
  let p1 = gfromJust "app" mp
      p2 = packet_set_str p1 "text" nick
  chat_join chat p2

  chat2 <- getChat (ecId chat)
  logT $ "created chat:" ++ show (chatIdToString $ ecId chat2,packet_get_str p2 "id",word32AsHexString $ ecRHash chat2)
  logT $ nick ++ ">"

  sw <- get
  logT $ "seeds:" ++ show (swSeeds sw)
  -- link_hn bucket_get(s->seeds, 0));
  link_hn $ head $ Set.toList (swSeeds sw)

  util_sendall sock

  -- create an admin channel for notes
  admin <- chan_new (swId sw) ".admin" Nothing


  let rx_loop = do
        mc <- switch_pop
        logT $ "rx_loop entered:mc=" ++ show mc
        case mc of
          Nothing -> return ()
          Just c -> do
            -- our internal testing stuff
            logT $ "TODO: put in internal testing stuff"

            logT $ "channel active " ++ show (chState c,chUid c,chTo c)
            case chHandler c of
              Just h -> do
                logT $ "rx_loop:calling handler"
                h (chId c)
              Nothing -> do
                logT $ "rx_loop:chType=" ++ (chType c)
                case chType c of
                  "connect" -> ext_connect c
                  "thtp"    -> ext_thtp (chId c)
                  "link"    -> ext_link c
                  "seek"    -> ext_link c -- is this correct?
                  "path"    -> ext_path c
                  "peer"    -> ext_peer c
                  "chat"    -> do
                    ext_chat c
                    msgs <-chat_pop_all chat
                    forM_ msgs $ \p -> do
                      logT $ "processing chat msg:" ++ show p

                      if (packet_get_str_always p "type") == "state"
                        then io $ logg nick (packet_get_str_always p "text" ++ " joined")
                        else return ()

                      if (packet_get_str_always p "type") == "chat"
                        then do
                          mparticipant <- chat_participant chat (packet_get_str_always p "from")
                          let participant = gfromJust "tft" mparticipant
                          io $ logg nick ((packet_get_str_always participant "text")
                                          ++ "> " ++ (packet_get_str_always p "text"))
                        else return ()

                  typ -> do
                    logT $ "not processing channel type:" ++ typ
                    util_chan_popall c Nothing
                if chState c == ChanEnded
                  then chan_free c
                  else return ()

            util_chan_popall c Nothing
            rx_loop

  let inPath = PNone
  logT $ "about to enter forever loop"
  forever $ do
    logT $ "top of forever loop"
    inp <- io $ readChan chInput

    switch_loop
    rx_loop
    util_sendall sock

    case inp of
      IUdp msg rinfo -> do
        recvTelex msg rinfo
      IConsole l -> do
        logT $ "console gave:" ++ l
        if | isPrefixOf "/quit" l -> do
              io $ killThread threadConsole
              io $ killThread threadUdp
              me <- io $ myThreadId
              io $ killThread me

           | isPrefixOf "/nick " l -> do
              mp <- chat_message chat
              case mp of
                Just p -> do
                  let nick = (drop (length ("/nick ")) l)
                  let p2 = packet_set_str p "text" nick
                  void $ chat_join chat p2
                  io $ logg nick ""
                Nothing -> return ()

           | isPrefixOf "/get " l -> do
              io $ logg nick ("get " ++ drop 5 l)
              p <- chan_note admin Nothing
              let p2 = rxTelexToTxTelex p (swId sw)
              let p3 = packet_set_str p2 "uri" (drop 5 l)
              void $ thtp_req p3

           | isPrefixOf "/chat " l -> do
              chat_free chat
              mchat <- chat_get (Just (drop 6 l))
              case mchat of
                Nothing -> return ()
                Just chat2 -> do
                  putChat chat2
                  mp <- chat_message chat2
                  case mp of
                    Nothing -> return ()
                    Just p -> do
                      let p2 = packet_set_str p "text" nick
                      chat_join chat2 p2
                      io $ logg nick ("joining chat " ++ show (ecId chat2,packet_get_str p2 "id", ecRHash chat2))

           | otherwise -> do
              -- default send as message
              mp <- chat_message chat
              case mp of
                Nothing -> return ()
                Just p -> do
                  let p2 = packet_set_str p "text" l
                  chat_send chat p2
                  io $ logg nick ""


    switch_loop
    rx_loop
    util_sendall sock

  assert False undefined

{-
char nick[16];
void logg(char * format, ...)
{
    char buffer[1024];
    va_list args;
    va_start (args, format);
    vsnprintf (buffer, 1024, format, args);
    if(strlen(buffer))
    {
      printf("\n%s\n%s> ", buffer, nick);
    }else{
      printf("%s> ",nick);
    }
    va_end (args);
}

int main(int argc, char *argv[])
{
  switch_t s;
  chan_t c, admin;
  packet_t p, note;
  path_t in;
  chat_t chat;
  int sock, len;
  char buf[256];
  const int fd = fileno(stdin);
  const int fcflags = fcntl(fd,F_GETFL);
  fcntl(fd,F_SETFL,fcflags | O_NONBLOCK);

  if(argc > 1) platform_debugging(1);

  crypt_init();
  s = switch_new(0);
  seek_auto(s);
  sprintf(nick,"%d",getpid());

  // make a dummy thtp response
  p = packet_new();
  packet_set_int(p,"status",200);
  packet_body(p,(unsigned char*)"bar\n",4);
  note = packet_new();
  packet_link(note,p);
  thtp_path(s,"/foo",note);

  if(util_loadjson(s) != 0 || (sock = util_server(0,100)) <= 0)
  {
    printf("failed to startup %s or %s\n", strerror(errno), crypt_err());
    return -1;
  }

  DEBUG_PRINTF("loaded hashname %s\n",s->id->hexname);

  // new chat, must be after-init
  chat = chat_get(s,"tft");
  chat_add(chat,"*","invited");
  p = chat_message(chat);
  packet_set_str(p,"text",nick);
  chat_join(chat,p);
  printf("created chat %s %s %s\n",chat->id,packet_get_str(p,"id"),chat->rhash);
  printf("%s> ",nick);

  link_hn(s, bucket_get(s->seeds, 0));
  util_sendall(s,sock);

  // create an admin channel for notes
  admin = chan_new(s, s->id, ".admin", 0);

  in = path_new("ipv4");
  while(util_readone(s, sock, in) == 0)
  {
    switch_loop(s);

    while((c = switch_pop(s)))
    {
      // our internal testing stuff
      if(c == admin)
      {
        while((p = chan_notes(c)))
        {
          printf("admin note %.*s\n",p->json_len,p->json);
          packet_free(p);
        }
        continue;
      }

      DEBUG_PRINTF("channel active %d %s %s\n",c->state,c->hexid,c->to->hexname);
      if(c->handler) c->handler(c);
      else {
        if(util_cmp(c->type,"connect") == 0) ext_connect(c);
        if(util_cmp(c->type,"thtp") == 0) ext_thtp(c);
        if(util_cmp(c->type,"link") == 0) ext_link(c);
        if(util_cmp(c->type,"seek") == 0) ext_link(c);
        if(util_cmp(c->type,"path") == 0) ext_path(c);
        if(util_cmp(c->type,"peer") == 0) ext_peer(c);
        if(util_cmp(c->type,"chat") == 0 && ext_chat(c)) while((p = chat_pop(chat)))
        {
          if(util_cmp(packet_get_str(p,"type"),"state") == 0)
          {
            logg("%s joined",packet_get_str(p,"text"));
          }
          if(util_cmp(packet_get_str(p,"type"),"chat") == 0)
          {
            logg("%s> %s",packet_get_str(chat_participant(chat,packet_get_str(p,"from")),"text"),packet_get_str(p,"text"));
          }
          packet_free(p);
        }
      }

      while((p = chan_pop(c)))
      {
        printf("unhandled channel packet %.*s\n", p->json_len, p->json);
        packet_free(p);
      }

      if(c->state == CHAN_ENDED) chan_free(c);
    }

    if((len = fread(buf,1,255,stdin)))
    {
      buf[len-1] = 0;
      if(strncmp(buf,"/nick ",6) == 0)
      {
        snprintf(nick,16,"%s",buf+6);
        p = chat_message(chat);
        packet_set_str(p,"text",nick);
        chat_join(chat,p);
        logg("");
      }else if(strcmp(buf,"/quit") == 0){
        // TODO test freeing all
        return 0;
      }else if(strcmp(buf,"/debug") == 0){
        platform_debugging(-1); // toggle
        logg("");
      }else if(strncmp(buf,"/get ",5) == 0){
        logg("get %s\n",buf+5);
        p = chan_note(admin,NULL);
        packet_set_str(p,"uri",buf+5);
        thtp_req(s,p);
      }else if(strncmp(buf,"/chat ",6) == 0){
        chat_free(chat);
        chat = chat_get(s,buf+6);
        p = chat_message(chat);
        packet_set_str(p,"text",nick);
        chat_join(chat,p);
        logg("joining chat %s %s %s\n",chat->id,packet_get_str(p,"id"),chat->rhash);
      }else if(strlen(buf)){
        // default send as message
        p = chat_message(chat);
        packet_set_str(p,"text",buf);
        chat_send(chat,p);
        logg("");
      }else{
        logg("");
      }
    }

    util_sendall(s,sock);
  }

  perror("exiting");
  return 0;
}

-}
