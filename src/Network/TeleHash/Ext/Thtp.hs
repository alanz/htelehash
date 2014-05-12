module Network.TeleHash.Ext.Thtp
  (
    ext_thtp
  , thtp_path
  , thtp_glob
  , thtp_req
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

-- |Store a value in the global store
thtp_glob :: Maybe String -> RxTelex -> TeleHash ()
thtp_glob mglob note = do
  logT $ "thtp_glob:(mglob,mnote)" ++ show (mglob,note)
  t <- thtp_get
  sw <- get
  let note2 =
       case mglob of
         Nothing -> note
         Just v -> packet_set_str note "glob" v
      note3 = rxTelexToTxTelex note2 (swId sw)
      key = fromMaybe "glob" mglob
  -- thtp_put $ t { thGlob = note2:(thGlob t)}
  let t2 = t { thGlob = note3:(thGlob t)}
  thtp_put t2
  logT $ "thtp_glob:thGlob=" ++ show (thGlob t2)

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

-- |Retrieve a value from the global store
_thtp_glob :: Thtp -> String -> TeleHash (Maybe TxTelex)
_thtp_glob t p1 = do
  logT $ "_thtp_glob:(t,p1)=" ++ show (t,p1)
  let myMatch str1 p =
        case packet_get_str p "glob" of
          Nothing -> False
          Just str2 -> (take (length str2) str1) == str2
      mcur = filter (myMatch p1) (thGlob t)
      r = case mcur of
            [] -> Nothing
            (x:_) -> Just x
  logT $ "_thtp_glob:mcur=" ++ show mcur
  return r


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
-}

-- ---------------------------------------------------------------------

-- chunk the packet out
thtp_send :: TChan -> TxTelex -> TeleHash ()
thtp_send c req = do
  logT $ "thtp_send:sending " ++ showJson (tJs req)
  lpraw <- packet_raw req
  let LP raw = lpraw
  logT $ "thtp_send:raw " ++ show raw

  -- send until everything is done

  let
    sendChunks toSend = do
      mchunk <- chan_packet c
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
                         else packet_set chunk2 "end" True
          logT $ "thtp_send:sending " ++ show chunk3
          chan_send c chunk3
          if restLen > 0
            then sendChunks rest
            else return ()

  sendChunks raw


{-
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
-}

-- ---------------------------------------------------------------------

-- generate an outgoing request, send the response attached to the note
thtp_req :: TxTelex -> TeleHash (Maybe TChan)
thtp_req note = do
  let method = packet_get_str note "method"
      path   = packet_get_str note "path"
      muri   = packet_get_str note "uri"

  logT $ "thtp_req:(method,path,uri)=" ++ show (method,path,muri)
  to <- if isJust muri && isPrefixOf "thtp://" (fromJust muri)
          then assert False undefined
          else return (tTo note)
  req <- case packet_linked note of
    Just r -> return r
    Nothing -> do
      let r1 = packet_chain note
          r2 = packet_set_str r1 "path" (fromMaybe "/" path)
          r3 = packet_set_str r2 "method" (fromMaybe "get" method)
      return r3

  logT $ "thtp_req:req " ++ show req

  -- open channel and send req
  c <- chan_new to "thtp" Nothing
  let c2 = c  { chArg = CArgTx (packet_link Nothing note) }
      c3 = c2 { chHandler = Just ext_thtp } -- shortcut
  putChan c3
  chan_reliable c3 10
  thtp_send c3 req
  c4 <- getChan (chUid c3)
  return (Just c4)

{-
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
-}

-- ---------------------------------------------------------------------

ext_thtp :: Uid -> TeleHash ()
ext_thtp cid = do
  c <- getChan cid
  t <- thtp_get
  -- incoming note as an answer
  mnote <- chan_notes c
  if chState c == ChanEnding && isJust mnote
    then do
      logT $ "ext_thtp:got note resp " ++ showJson (tJs $ fromJust mnote)
      assert False undefined
    else do
      rxs <- chan_pop_all cid
      forM_ rxs $ \p -> do
        c2 <- getChan cid -- may have changed in earlier loop iteration
        case packet_get_str p "err" of
          Just v -> do
            logT $ "ext_thtp:got err: " ++ show v
          Nothing -> return ()
        logT $ "ext_thtp:chArg:" ++ show (chArg c2)
        buf <- case chArg c2 of
          CArgNone -> do
            let pt = rxTelexToTxTelex p (HN "thtp")
            putChan $ c2 { chArg = CArgTx pt }
            return $ pt
          CArgRx r -> do
            let pt = rxTelexToTxTelex p (HN "thtp")
                rt = rxTelexToTxTelex r (HN "thtp")
            let r2 = packet_append rt (unBody $ paBody (tPacket pt))
            putChan $ c2 { chArg = CArgTx r2 }
            return r2
          CArgTx r -> do
            let r2 = packet_append r (unBody $ paBody (rtPacket p))
            putChan $ c2 { chArg = CArgTx r2 }
            return r2
            -- putChan $ c2 { chArg = CArgRx p }
            -- return p
          arg -> do
            logT $ "ext_thtp:unexpected cArg:" ++ show arg
            assert False undefined

        -- for now we're processing whole-requests-at-once, to do streaming
        --  we can try parsing note->body for the headers anytime
        c3 <- getChan cid
        if not (chState c3 == ChanEnding)
          then return ()
          else do
            -- When the last chunk is sent, the "end" flag is set, should all be here in the body

            -- parse the payload
            logT $ "ext_thtp:(rtPacket buf)=" ++ show (tPacket buf)
            let bufBody = unBody $ paBody (tPacket buf)
            let mnp = fromNetworkPacket (LP bufBody)
            logT $ "ext_thtp:mnp=" ++ show mnp
            case mnp of
              Nothing -> do
                logT $ "ext_thtp:malformed long packet received:" ++ show (bufBody)
                void $ chan_fail c (Just "422")
                return ()
              Just (OpenPacket _b _bs) -> do
                logT $ "ext_thtp:unexpected OpenPacket received:" ++ show (mnp)
                void $ chan_fail c (Just "422")
                return ()
              Just (LinePacket _pbody) -> do
                logT $ "ext_thtp:unexpected LinePacket received:" ++ show (mnp)
                void $ chan_fail c (Just "422")
                return ()
              Just (PingPongPacket lp) -> do
                logT $ "ext_thtp:PingPongPacket received:" ++ show (lp)

                -- this is a response, send it
                logT $ "ext_thtp: TODO: reinstate response process"
{-
ext_thtp:chArg:CArgTx (TxTelex {tId = 0, tTo = HN "packet_link", tOut = PNone, tJs = fromList [], tPacket = Packet {paHead = HeadEmpty, paBody = Body ""}, tChain = Nothing, tLp = Nothing})

ext_thtp:PingPongPacket received:Packet {paHead = HeadJson "{\"status\":200}", paBody = Body "{\"*\":\"invited\",\"49eb85838a320f60ce1894234f7d1ec04ec5957cb4644fa11750e01a6c88b58a\":\"177b603c,1000\"}"}
-}

                case packet_unlink buf of
                -- case Nothing of
                  Just note -> do
                    logT $ "ext_thtp:got response " ++ (show $ paHead lp) ++ " for " ++ showJson (tJs note)
                    assert False undefined
                  Nothing -> do
                    -- this is an incoming request
                    let req = lp
                    logT $ "ext_thtp:got req:" ++ show (paHead lp)
                    let mjs = packetJson lp
                    case mjs of
                      Nothing -> do
                        logT $ "ext_thtp: malformed request received"
                        void $ chan_fail c (Just "422")
                        return ()
                      Just v -> do
                        logT $ "ext_thtp:req json=" ++ showJson v
                        let mpath  = get_str_from_value "path" v
                            mmatch = if isJust mpath then Map.lookup (fromJust mpath) (thIndex t)
                                                     else Nothing
                        mmatch2 <- if isJust mmatch then return mmatch
                                                    else _thtp_glob t (gfromJust "ext_thtp" mpath)

                        case mmatch2 of
                          Nothing -> do
                            logT $ "ext_thtp:no match value in request"
                            void $ chan_fail c (Just "404")
                            return ()
                          Just mm -> do
                            -- built in response
                            case packet_linked mm of
                              Just linked -> do
                                thtp_send c linked
                                return ()
                              Nothing -> do
                                -- attach and route request to a new note
                                let note = mm
                                sw <- get
                                let f = packet_link (Just note) ((packet_new (swId sw)){ tPacket = req } )
                                r <- chan_reply c note
                                case r of
                                  Ok   -> return ()
                                  Fail -> do
                                    chan_fail c (Just "500")
                                    return ()

  -- optionally sends ack if needed
  chan_ack c

{-
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
