
-- based on tft.c in telehash-c

import Control.Concurrent
import Control.Exception
import Control.Monad.State
import System.IO
import System.Log.Handler.Simple
import System.Log.Logger

import Network.TeleHash.Bucket
import Network.TeleHash.Ext.Chat
import Network.TeleHash.Ext.Connect
import Network.TeleHash.Ext.Link
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

  seek_auto
  myId <- io myThreadId
  let nick = show myId
  logT $ "nick=" ++ nick

  -- make a dummy thtp response
  let p1 = packet_new (HN "null")
      p2 = packet_set_int p1 "status" 200
      p3 = packet_body p2 (BC.pack "bar\n")
      note = packet_new (HN "null")
      note2 = packet_link note p3
  thtp_path "/foo" note2

  util_loadjson
  sock <- io $ util_server 0 100

  sw <- get
  logT $ "loaded hashname " ++ show (swId sw)

  -- new chat, must be after-init
  chat <- chat_get (Just "tft")

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
