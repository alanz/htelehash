module Network.TeleHash.Ext.Thtp
  (
    thtp_path
  , thtp_glob
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
typedef struct thtp_struct
{
  xht_t index;
  packet_t glob;
} *thtp_t;
-}

-- ---------------------------------------------------------------------

{-
thtp_t thtp_new(switch_t s, xht_t index)
{
  thtp_t t;
  t = malloc(sizeof (struct thtp_struct));
  memset(t,0,sizeof (struct thtp_struct));
  if(!index) index = xht_new(13);
  t->index = index;
  xht_set(s->index,"thtp",t);
  return t;

}

// typeless wrapper
void thtp_init(switch_t s, xht_t index)
{
  thtp_new(s,index);
}
-}

-- ---------------------------------------------------------------------

thtp_get :: TeleHash Thtp
thtp_get = do
  sw <- get
  case (swThtp sw) of
    Just t -> return t
    Nothing -> do
      let t = Thtp Map.empty []
      put $ sw { swThtp = Just t }
      return t

{-
thtp_t thtp_get(switch_t s)
{
  thtp_t t;
  t = xht_get(s->index,"thtp");
  return t ? t : thtp_new(s,NULL);
}
-}

-- ---------------------------------------------------------------------

thtp_put :: Thtp -> TeleHash ()
thtp_put t = do
  sw <- get
  put $ sw { swThtp = Just t}

-- ---------------------------------------------------------------------

{-
void thtp_free(switch_t s)
{
  thtp_t t = thtp_get(s);
  //xht_walk() TODO free all notes
  // free all globs
  xht_free(t->index);
  free(t);
}
-}
-- ---------------------------------------------------------------------

thtp_glob :: Maybe String -> RxTelex -> TeleHash ()
thtp_glob mglob note = do
  logT $ "thtp_glob:(mglob,mnote)" ++ show (mglob,note)
  t <- thtp_get
  let note2 =
       case mglob of
         Nothing -> note
         Just v -> packet_set_str note "glob" v
  thtp_put $ t { thGlob = note2:(thGlob t)}

{-
// TODO support NULL note to delete
void thtp_glob(switch_t s, char *glob, packet_t note)
{
  thtp_t t = thtp_get(s);
  if(glob) packet_set_str(note,"glob",glob);
  note->next = t->glob;
  t->glob = note;
}
-}

-- ---------------------------------------------------------------------

thtp_path :: String -> TxTelex -> TeleHash ()
thtp_path path note = do
  t <- thtp_get
  let note2 = packet_set_str note "path" path
      t2 = t { thIndex = Map.insert path note2 (thIndex t) }
  sw <- get
  put $ sw { swThtp = Just t2 }

{-
void thtp_path(switch_t s, char *path, packet_t note)
{
  thtp_t t = thtp_get(s);
  packet_set_str(note,"path",path);
  xht_set(t->index,packet_get_str(note,"path"),(void*)note);
}
-}

-- ---------------------------------------------------------------------

{-
packet_t _thtp_glob(thtp_t t, char *p1)
{
  char *p2;
  packet_t cur;
  if(!t || !p1) return NULL;
  cur = t->glob;
  while(cur)
  {
    p2 = packet_get_str(cur,"glob");
    if(strncasecmp(p1,p2,strlen(p2)) == 0) return cur;
    cur = cur->next;
  }
  return NULL;
}

// chunk the packet out
void thtp_send(chan_t c, packet_t req)
{
  packet_t chunk;
  unsigned char *raw;
  unsigned short len, space;
  if(!c || !req) return;
  DEBUG_PRINTF("THTP sending %.*s %.*s",req->json_len,req->json,req->body_len,req->body);
  raw = packet_raw(req);
  len = packet_len(req);
  while(len)
  {
    chunk = chan_packet(c);
    if(!chunk) return; // TODO backpressure
    space = packet_space(chunk);
    if(space > len) space = len;
    packet_body(chunk,raw,space);
    if(len==space) packet_set(chunk,"end","true",4);
    chan_send(c,chunk);
    raw+=space;
    len-=space;
  }
}

// generate an outgoing request, send the response attached to the note
chan_t thtp_req(switch_t s, packet_t note)
{
  char *uri, *path, *method;
  hn_t to = NULL;
  packet_t req;
  chan_t c;
  if(!s || !note) return NULL;

  method = packet_get_str(note,"method");
  path = packet_get_str(note,"path");
  if((uri = packet_get_str(note,"uri")) && strncmp(uri,"thtp://",7) == 0)
  {
    uri += 7;
    path = strchr(uri,'/');
    to = hn_gethex(s->index,uri);
  }
  if(!to) to = hn_gethex(s->index,packet_get_str(note,"to"));
  if(!to) return NULL;
  req = packet_linked(note);
  if(!req)
  {
    req = packet_chain(note);
    packet_set_str(req,"path",path?path:"/");
    packet_set_str(req,"method",method?method:"get");
  }

  DEBUG_PRINTF("thtp req %s %s %s %.*s",packet_get_str(req,"method"),packet_get_str(req,"path"),to->hexname,note->json_len,note->json);

  // open channel and send req
  c = chan_new(s, to, "thtp", 0);
  c->arg = packet_link(NULL,note); // create buffer packet w/ the note linked
  c->handler = ext_thtp; // shortcut
  chan_reliable(c,10);
  thtp_send(c,req);

  return c;
}

void ext_thtp(chan_t c)
{
  packet_t p, buf, req, match, note;
  char *path;
  thtp_t t = thtp_get(c->s);

  // incoming note as an answer
  if(c->state == CHAN_ENDING && (note = chan_notes(c)))
  {
    DEBUG_PRINTF("got note resp %.*s",note->json_len,note->json);
    thtp_send(c,packet_linked(note));
    packet_free(note);
    return;
  }

  while((p = chan_pop(c)))
  {
    if(!c->arg)
    {
      c->arg = buf = p;
    }else{
      buf = c->arg;
      packet_append(buf,p->body,p->body_len);
      packet_free(p);
    }
    // for now we're processing whole-requests-at-once, to do streaming we can try parsing note->body for the headers anytime
    if(!c->state == CHAN_ENDING) continue;

    // parse the payload
    p = packet_parse(buf->body,buf->body_len);

    // this is a response, send it
    if((note = packet_unlink(buf)))
    {
      packet_free(buf);
      if(p)
      {
        DEBUG_PRINTF("got response %.*s for %.*s",p->json_len,p->json,note->json_len,note->json);        
      }
      packet_link(note,p);
      packet_set_str(note,"thtp","resp");
      chan_reply(c,note);
      chan_end(c,NULL);
      return;
    }

    // this is an incoming request
    packet_free(buf);
    if(!p) return (void)chan_fail(c,"422");
    req = p;

    DEBUG_PRINTF("thtp req packet %.*s", req->json_len, req->json);
    path = packet_get_str(req,"path");
    match = xht_get(t->index,path);
    if(!match) match = _thtp_glob(t,path);
    if(!match)
    {
      chan_fail(c,"404");
      packet_free(req);
      return;
    }

    // built in response
    if(packet_linked(match))
    {
      thtp_send(c,packet_linked(match));
      packet_free(req);
      return;
    }

    // attach and route request to a new note
    note = packet_copy(match);
    packet_link(note,req);
    packet_set_str(note,"thtp","req");
    if(chan_reply(c,note) == 0) return;

    chan_fail(c,"500");
    packet_free(req);
  }

  // optionally sends ack if needed
  chan_ack(c);
}
-}
