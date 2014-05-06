module Network.TeleHash.Ext.Chat
  (
    chat_get
  , chat_add
  , chat_message
  , chat_join
  , chat_free
  , chat_send

  -- * persisting
  , getChat
  , putChat
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
import Network.TeleHash.Ext.Thtp
import Network.TeleHash.Hn
import Network.TeleHash.Packet
import Network.TeleHash.Path
import Network.TeleHash.Paths
import Network.TeleHash.SwitchApi
import Network.TeleHash.SwitchUtils
import Network.TeleHash.Types
import Network.TeleHash.Utils

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.PubKey.DH as DH
import qualified Crypto.Types.PubKey.ECDSA as ECDSA
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Murmur as Murmur
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as SB

-- ---------------------------------------------------------------------

chatr_new :: Chat -> ChatR
chatr_new chat
  = ChatR
      { ecrChat = chat
      , ecrIn = packet_new_rx
      , ecrJoined = False
      , ecrOnline = False
      }

{-

chatr_t chatr_new(chat_t chat)
{
  chatr_t r;
  r = malloc(sizeof (struct chatr_struct));
  memset(r,0,sizeof (struct chatr_struct));
  r->chat = chat;
  r->in = packet_new();
  return r;
}

void chatr_free(chatr_t r)
{
  packet_free(r->in);
  free(r);
}
-}

-- ---------------------------------------------------------------------




-- |Every chat is identified by a unique endpoint@originator. The
-- originator is always the hashname that first created the chat, and
-- the endpoint is up to 32 lower case alphanumeric word characters
-- (ASCII [a-z0-9_]) in length. The endpoint is typically
-- automatically generated on demand to be unique and not visible.
parseChatId :: String -> Maybe ChatId
parseChatId str = r
  where
    (f,b) = break (=='@') str
    b2 = if null b
           then b
           else if head b == '@'
                  then tail b
                  else b
    okChar c = (c >= 'a' && c <= 'z')
               || (c >= '0' && c <= '9')
               || c == '_'
    badChar c = not (okChar c)

    fOk = (not (null f))
          && (null (filter (badChar) f))
          && (length f <= 32)

    isHex :: Char -> Bool
    isHex x | ((y >= '0') && (y <= '9')) || ((y >= 'a') && (y <= 'f')) = True
                    | otherwise = False
                            where y = toLower x
    notHex c = not (isHex c)

    bOk = (null b2)
          || ((null (filter notHex b2))
           && length b2 == 64)

    r = if fOk && bOk
          then if null b2 then Just (ChatId f Nothing)
                          else Just (ChatId f (Just (HN b2)))
          else Nothing
          -- else Just (ChatId (show (f,length b2,(filter notHex b2))) (Just (HN b2)))

-- tp = parseChatId "alanz@7766e761afb226d7b398379ea1bf12c53dc02580c683b173568b0c6cc3a09c00"

-- ---------------------------------------------------------------------

chat_rhash :: Chat -> TeleHash Murmur.Hash
chat_rhash chat = do
  let r = intercalate "," $ concatMap (\(k,v) -> [k,v]) $ Map.toAscList (ecRoster chat)
  logT $ "chat_rhash:r=" ++ show r
  let rhash = Murmur.hash $ r
      chat2 = chat { ecRHash = rhash }
  putChat chat2
  return rhash

{-
char *chat_rhash(chat_t chat)
{
  char *buf, *str;
  int at=0, i, len;
  // TODO, should be a way to optimize doing the mmh on the fly
  buf = malloc(chat->roster->json_len);
  packet_sort(chat->roster);
  for(i=0;(str = packet_get_istr(chat->roster,i));i++)
  {
    len = strlen(str);
    memcpy(buf+at,str,len);
    at += len;
  }
  util_murmur((unsigned char*)buf,at,chat->rhash);
  return chat->rhash;
}
-}

-- ---------------------------------------------------------------------

-- |if mid is Nothing it requests the roster, otherwise requests message id
chat_cache :: Chat -> HashName -> Maybe String -> TeleHash ()
chat_cache chat hn mid = do
  if isJust mid
     && not (',' `elem` (fromJust mid))
    then return ()
    else do
      -- only fetch from the origin until joined
      let via = if ecJoin chat == Nothing
                  then hn
                  else ecOrigin chat
      if isJust mid && (Map.member (fromJust mid) (ecLog chat))
        then return () -- cached message id
        else do
          sw <- get
          if swId sw == via
            then return () -- can't request from ourselves
            else do
              hub <- getChan (ecHub chat)
              note <- chan_note hub Nothing
              let note2 = packet_set_str note  "to" (unHN via)
                  note3 = packet_set_str note2 "for" (unHN hn)
                  note4 = if isNothing mid
                            then packet_set_str note3 "path" ("/chat/" ++ (word32AsHexString $ ecIdHash chat) ++ "/roster")
                            else packet_set_str note3 "path" ("/chat/" ++ (word32AsHexString $ ecIdHash chat) ++ "/id/" ++ fromJust mid)
              void $ thtp_req (rxTelexToTxTelex note4 via)
{-
// if id is NULL it requests the roster, otherwise requests message id
void chat_cache(chat_t chat, char *hn, char *id)
{
  char *via;
  packet_t note;
  if(!chat || (id && !strchr(id,','))) return;
  // only fetch from origin until joined
  via = (!hn || !chat->join) ? chat->origin->hexname : hn;
  if(id && xht_get(chat->log,id)) return; // cached message id
  if(util_cmp(via,chat->s->id->hexname) == 0) return; // can't request from ourselves
  note = chan_note(chat->hub,NULL);
  packet_set_str(note,"to",via);
  packet_set_str(note,"for",hn);
  if(!id) packet_set_printf(note,"path","/chat/%s/roster",chat->idhash);
  else packet_set_printf(note,"path","/chat/%s/id/%s",chat->idhash,id);
  thtp_req(chat->s,note);
}
-}

-- ---------------------------------------------------------------------

putChat :: Chat -> TeleHash ()
putChat chat = do
  sw <- get
  put $ sw {swIndexChat = Map.insert (ecId chat) chat (swIndexChat sw) }


getChat :: ChatId -> TeleHash Chat
getChat cid = do
  sw <- get
  return $ gfromJust ("getChat " ++ show cid) $ Map.lookup cid (swIndexChat sw)

-- ---------------------------------------------------------------------


chat_get :: Maybe String -> TeleHash (Maybe Chat)
chat_get mid = do
  sw <- get
  mcid <- case mid of
    Just sid -> do
      -- if there's an id, validate and optionally parse out originator
      case parseChatId sid of
        Nothing -> do
          logT $ "invalid chatid:" ++ sid
          return Nothing
        Just cid -> do
          case ciOriginator cid of
            Just hn -> do
              mo <- getHNMaybe hn
              case mo of
                Nothing -> return Nothing
                Just _  -> return $ Just (cid { ciOriginator = Just hn })
            Nothing -> return $ Just (cid { ciOriginator = Just  (swId sw)})

    Nothing -> do
      epVal <- randomHEX 8
      return $ Just (ChatId epVal (Just (swId sw)))

  case mcid of
    Nothing -> return Nothing
    Just cid -> do
      case Map.lookup cid (swIndexChat sw) of
        Just chat -> return (Just chat)
        Nothing -> do
          hubc <- chan_new (swId sw) "chat" Nothing

          randWord32 <- randomWord32
          let chat = Chat
                { ecEp     = ciEndpoint cid
                , ecId     = cid
                , ecIdHash = Murmur.hash (chatIdToString cid)
                , ecOrigin = gfromJust "chat_get" (ciOriginator cid)
                , ecRHash  = 0
                , ecLocal  = ciOriginator cid == Just (swId sw)
                , ecSeed   = randWord32
                , ecSeq    = 1000
                , ecRoster = Map.empty
                , ecConn   = Map.empty
                , ecLog    = Map.empty
                , ecMsgs   = Nothing
                , ecJoin   = Nothing
                , ecSent   = Nothing
                , ecAfter  = Nothing

                , ecHub    = chUid hubc

                }
          -- an admin channel for distribution and thtp requests
          let chatr = chatr_new chat
          let hub = hubc { cArg = CArgChatR chatr }
          note <- chan_note hub Nothing
          let note2 = packet_set_str note "glob" ("/chat/" ++ (show $ ecIdHash chat) ++ "/")
          logT $ "chat,glob:" ++ show (ecId chat,packet_get_str note2 "glob")
          thtp_glob Nothing note2
          putChat chat

          -- any other hashname and we try to initialize
          if not (ecLocal chat)
            then do
              chat_cache chat (ecOrigin chat) Nothing
            else return ()
          return (Just chat)

{-
chat_t chat_get(switch_t s, char *id)
{
  chat_t chat;
  packet_t note;
  hn_t origin = NULL;
  int at;
  char buf[128];

  chat = xht_get(s->index,id);
  if(chat) return chat;

  // if there's an id, validate and optionally parse out originator
  if(id)
  {
    at = chat_eplen(id);
    if(at < 0) return NULL;
    if(at > 0)
    {
      id[at] = 0;
      origin = hn_gethex(s->index,id+(at+1));
      if(!origin) return NULL;
    }
  }

  chat = malloc(sizeof (struct chat_struct));
  memset(chat,0,sizeof (struct chat_struct));
  if(!id)
  {
    crypt_rand((unsigned char*)buf,4);
    util_hex((unsigned char*)buf,4,(unsigned char*)chat->ep);
  }else{
    memcpy(chat->ep,id,strlen(id)+1);
  }
  chat->origin = origin ? origin : s->id;
  if(chat->origin == s->id) chat->local = 1;
  sprintf(chat->id,"%s@%s",chat->ep,chat->origin->hexname);
  util_murmur((unsigned char*)chat->id,strlen(chat->id),chat->idhash);
  chat->s = s;
  chat->roster = packet_new();
  packet_json(chat->roster,(unsigned char*)"{}",2);
  chat->log = xht_new(101);
  chat->conn = xht_new(7);
  chat->seq = 1000;
  crypt_rand((unsigned char*)&(chat->seed),4);

  // an admin channel for distribution and thtp requests
  chat->hub = chan_new(s, s->id, "chat", 0);
  chat->hub->arg = chatr_new(chat); // stub so we know it's the hub chat channel
  note = chan_note(chat->hub,NULL);
  packet_set_printf(note,"glob","/chat/%s/",chat->idhash);
  DEBUG_PRINTF("chat %s glob %s",chat->id,packet_get_str(note,"glob"));
  thtp_glob(s,0,note);
  xht_set(s->index,chat->id,chat);

  // any other hashname and we try to initialize
  if(!chat->local) chat_cache(chat,chat->origin->hexname,NULL);

  return chat;
}
-}

-- ---------------------------------------------------------------------

chat_free :: Chat -> TeleHash ()
chat_free chat = do
  sw <- get
  -- TODO: individually release the connections etc
  put $ sw {swIndexChat = Map.delete (ecId chat) (swIndexChat sw) }

{-
chat_t chat_free(chat_t chat)
{
  if(!chat) return chat;
  xht_set(chat->s->index,chat->id,NULL);
  // TODO xht-walk chat->log and free packets
  // TODO xht-walk chat->conn and end channels
  // TODO unregister thtp
  // TODO free chat->msgs chain
  xht_free(chat->log);
  xht_free(chat->conn);
  packet_free(chat->roster);
  free(chat);
  return NULL;
}
-}

-- ---------------------------------------------------------------------

chat_message :: Chat -> TeleHash (Maybe TxTelex)
chat_message chat = do
  (TOD at _)  <- io getClockTime
  if ecSeq chat == 0
    then return Nothing
    else do
      sw <- get
      let p1 = packet_new (swId sw)
      let idVal = word32AsHexString (ecSeed chat)
      logT $ "chat_message:idVal=" ++ idVal
      -- do repeated murmur hash
      let idW32 = last $ take (fromIntegral $ ecSeq chat) $ iterate Murmur.hash (ecSeed chat)
          idHex = word32AsHexString idW32
          idFull = idHex ++ "," ++ show (ecSeq chat)
      logT $ "chat_message:idFull=" ++ idFull
      let chat2 = chat { ecSeq = (ecSeq chat) - 1 }
      putChat chat2
      let p2 = packet_set_str p1 "id" idFull
          p3 = packet_set_str p2 "type" "chat"
          p4 = case (ecAfter chat2) of
                 Just after -> packet_set_str p3 "after" after
                 Nothing -> p3
          p5 = packet_set_int p4 "at" (fromIntegral at)
      return (Just p5)

{-
packet_t chat_message(chat_t chat)
{
  packet_t p;
  char id[32];
  unsigned char buf[4];
  uint16_t step;
  unsigned long at = platform_seconds();

  if(!chat || !chat->seq) return NULL;

  p = packet_new();
  // this can be optimized by not converting to/from hex once mmh32 is endian safe
  util_hex((unsigned char*)chat->seed,4,(unsigned char*)id);
  for(step=0; step <= chat->seq; step++) util_murmur(util_unhex((unsigned char*)id,8,(unsigned char*)buf),4,id);
  sprintf(id+8,",%d",chat->seq);
  chat->seq--;
  packet_set_str(p,"id",id);
  packet_set_str(p,"type","chat");
  if(chat->after) packet_set_str(p,"after",chat->after);
  if(at > 1396184861) packet_set_int(p,"at",at); // only if platform_seconds() is epoch
  return p;
}
-}

-- ---------------------------------------------------------------------

{-
// add msg to queue
void chat_push(chat_t chat, packet_t msg)
{
  packet_t prev;
  if(!chat) return (void)packet_free(msg);
  msg->next = NULL; // paranoid safety
  if(!chat->msgs) return (void)(chat->msgs = msg);
  prev = chat->msgs;
  while(prev->next) prev = prev->next;
  prev->next = msg;
}

packet_t chat_pop(chat_t chat)
{
  packet_t msg;
  if(!chat || !chat->msgs) return NULL;
  msg = chat->msgs;
  chat->msgs = msg->next;
  msg->next = NULL;
  return msg;
}
-}

-- ---------------------------------------------------------------------

-- |updates current stored state to notify app of changes
chat_restate :: ChatId -> String -> TeleHash ()
chat_restate cid hn = do
  if hn == ""
    then return ()
    else do
      chat <- getChat cid
      -- load from roster
      case Map.lookup hn (ecRoster chat) of
        Nothing -> return ()
        Just idVal -> do
          --  see if there's a join message cached
          state <- case Map.lookup idVal (ecLog chat) of
            Just join -> do
              return join
            Nothing -> do
              if ',' `elem` idVal
                then do
                  -- we should have the join id, try to get it again
                  void $ chat_cache chat (HN hn) (Just idVal)
                  return $ packet_set_str (packet_new (HN hn)) "text" "connecting"
                else do
                  return $ packet_set_str (packet_new (HN hn)) "text" idVal
          -- make a state packet
          let state2 = packet_set_str state "type" "state"
              state3 = packet_set_str state2 "from" hn

          state4 <- case Map.lookup hn (ecConn chat) of
            Just uid -> do
              c <- getChan uid
              case cArg c of
                CArgChatR r -> do
                  if ecrOnline r
                    then do
                      return $ packet_set_str state3 "online" "true"
                    else do
                      return $ packet_set_str state3 "online" "false"
                _ -> do
                  return $ packet_set_str state3 "online" "false"
            Nothing -> do
              return $ packet_set_str state3 "online" "false"
          -- if the new state is the same, drop it
          case Map.lookup hn (ecLog chat) of
            Nothing -> return ()
            Just cur -> do
              if packet_cmp state4 cur == True
                then return ()
                else do
                  assert False undefined

{-
// updates current stored state to notify app of changes
void chat_restate(chat_t chat, char *hn)
{
  char *id;
  packet_t join, state, cur;
  chan_t c;
  chatr_t r;

  if(!chat || !hn) return;

  // load from roster
  id = packet_get_str(chat->roster,hn);
  if(!id) return;

  // see if there's a join message cached
  join = xht_get(chat->log,id);
  if(join)
  {
    state = packet_copy(join);
  }else{
    state = packet_new();
    if(strchr(id,','))
    {
      // we should have the join id, try to get it again
      chat_cache(chat,hn,id);
      packet_set_str(state,"text","connecting");
    }else{
      packet_set_str(state,"text",id);
    }
  }

  // make a state packet
  packet_set_str(state,"type","state");
  packet_set_str(state,"from",hn);

  if((c = xht_get(chat->conn,hn)) && (r = c->arg) && r->online) packet_set(state,"online","true",4);
  else packet_set(state,"online","false",5);

  // if the new state is the same, drop it
  cur = xht_get(chat->log,hn);
  if(packet_cmp(state,cur) == 0) return (void)packet_free(state);

  // replace/save new state
  xht_set(chat->log,packet_get_str(state,"from"),state);
  packet_free(cur);

  // notify if not ourselves
  if(util_cmp(hn,chat->s->id->hexname)) chat_push(chat,packet_copy(state));
}
-}

-- ---------------------------------------------------------------------

-- |process roster to check connection/join state
chat_sync :: ChatId -> TeleHash ()
chat_sync cid = do
  sw <- get
  chat <- getChat cid
  let joined = case ecJoin chat of
                 Nothing -> False
                 Just _  -> True
  let roster = Map.toAscList (ecRoster chat)
  forM_ roster $ \(part,v) -> do
    if length part /= 64 || (swId sw) == HN part
      then return ()
      else do
        case Map.lookup part (ecConn chat) of
          Just uid -> do
            c <- getChan uid
            let mr = cArg c
            case mr of
              CArgChatR r -> do
                if ecrJoined r == joined
                  then return ()
                  else do
                    logT $ "chat_sync:initiating chat to " ++ show (part,ecId chat)
                    -- state change
                    chat_restate (ecId chat) part
                    assert False undefined
              _ -> return ()
          Nothing -> do
            assert False undefined
{-
// process roster to check connection/join state
void chat_sync(chat_t chat)
{
  char *part;
  chan_t c;
  chatr_t r;
  packet_t p;
  int joined, i = 0;

  joined = chat->join ? 1 : 0;
  while((part = packet_get_istr(chat->roster,i)))
  {
    i += 2;
    if(strlen(part) != 64) continue;
    if(util_cmp(part,chat->s->id->hexname) == 0) continue;
    if((c = xht_get(chat->conn,part)) && (r = c->arg) && r->joined == joined) continue;

    DEBUG_PRINTF("initiating chat to %s for %s",part,chat->id);
    // state change
    chat_restate(chat,part);

    // try to connect
    c = chan_start(chat->s, part, "chat");
    if(!c) continue;
    xht_set(chat->conn,c->to->hexname,c);
    r = c->arg = chatr_new(chat);
    r->joined = joined;
    p = chan_packet(c);
    packet_set_str(p,"to",chat->id);
    packet_set_str(p,"from",chat->join);
    packet_set_str(p,"roster",chat->rhash);
    chan_send(c,p);
  }
}
-}

-- ---------------------------------------------------------------------

-- |just add to the log
chat_log :: ChatId -> TxTelex -> TeleHash ()
chat_log cid msg = do
  chat <- getChat cid
  case packet_get_str msg "id" of
    Nothing -> return ()
    Just idVal -> do
      putChat (chat { ecLog = Map.insert idVal msg (ecLog chat) })

{-
// just add to the log
void chat_log(chat_t chat, packet_t msg)
{
  packet_t cur;
  char *id = packet_get_str(msg,"id");
  if(!chat || !id) return (void)packet_free(msg);
  cur = xht_get(chat->log,id);
  xht_set(chat->log,id,msg);
  packet_free(cur);
}
-}

-- ---------------------------------------------------------------------

chat_join :: Chat -> TxTelex -> TeleHash (Maybe Chat)
chat_join chat join = do
  -- paranoid, don't double-join
  if isJust (ecJoin chat)
     && ((ecJoin chat) == (packet_get_str join "id"))
    then return Nothing
    else do
      let j2 = packet_set_str join "type" "join"
          chat2 = chat {ecJoin = packet_get_str j2 "id"}
      putChat chat2
      chat_log (ecId chat2) j2
      chat3 <- getChat (ecId chat2)
      sw <- get
      let chat4 = chat3 { ecRoster = Map.insert (unHN $ swId sw)
                                                (gfromJust "chat_join" $ ecJoin chat3)
                                                (ecRoster chat3) }
      putChat chat4
      chat_restate (ecId chat4) (unHN $ swId sw)
      chat5 <- getChat (ecId chat4)
      chat_rhash chat5

      -- create/activate all chat channels
      chat_sync (ecId chat5)
      chat6 <- getChat (ecId chat5)
      return $ Just chat6

{-
chat_t chat_join(chat_t chat, packet_t join)
{
  if(!chat || !join) return NULL;

  // paranoid, don't double-join
  if(util_cmp(chat->join,packet_get_str(join,"id")) == 0) return (chat_t)packet_free(join);

  packet_set_str(join,"type","join");
  chat->join = packet_get_str(join,"id");
  chat_log(chat,join);
  packet_set_str(chat->roster,chat->s->id->hexname,chat->join);
  chat_restate(chat,chat->s->id->hexname);
  chat_rhash(chat);

  // create/activate all chat channels
  chat_sync(chat);

  return chat;
}
-}

-- ---------------------------------------------------------------------

chat_chunk :: TChan -> TxTelex -> TeleHash ()
chat_chunk c msg = do
  assert False undefined
{-
// chunk the packet out
void chat_chunk(chan_t c, packet_t msg)
{
  packet_t chunk;
  unsigned char *raw;
  unsigned short len, space;
  if(!c || !msg) return;
  raw = packet_raw(msg);
  len = packet_len(msg);
  while(len)
  {
    chunk = chan_packet(c);
    if(!chunk) return; // TODO backpressure
    space = packet_space(chunk);
    if(space > len) space = len;
    packet_body(chunk,raw,space);
    if(len==space) packet_set(chunk,"done","true",4);
    chan_send(c,chunk);
    raw+=space;
    len-=space;
  }
}
-}

-- ---------------------------------------------------------------------

chat_send :: Chat -> TxTelex -> TeleHash ()
chat_send chatIn msg = do
  chat <- getChat (ecId chatIn)
  chat_log (ecId chat) msg

  -- send as a note to all connected
  forM_ (Map.elems $ ecRoster chat) $ \idVal -> do
    case Map.lookup idVal (ecConn chat) of
      Nothing -> return ()
      Just cuid -> do
        c <- getChan cuid
        case (cArg c) of
          CArgChatR r -> do
            if (not (ecrJoined r) || not (ecrOnline r))
              then return ()
              else do
                chat_chunk c msg
          _ -> return ()

{-
chat_t chat_send(chat_t chat, packet_t msg)
{
  char *part;
  chan_t c;
  chatr_t r;
  int i = 0;

  if(!chat || !msg) return NULL;

  chat_log(chat,msg);

  // send as a note to all connected
  while((part = packet_get_istr(chat->roster,i)))
  {
    i += 2;
    if(!(c = xht_get(chat->conn,part))) continue;
    r = c->arg;
    if(!r || !r->joined || !r->online) continue;
    chat_chunk(c,msg);
  }
  return NULL;
}
-}
-- ---------------------------------------------------------------------

chat_add :: Chat -> String -> String -> TeleHash (Maybe Chat)
chat_add chat hn val = do
  if hn == "" || val == "" || (not $ ecLocal chat)
    then return Nothing
    else do
      let roster = Map.insert hn val (ecRoster chat)
          chat2 = chat { ecRoster = roster }
      putChat chat2
      chat_rhash chat2
      chat_sync (ecId chat2)
      -- try to load if it's a message id
      if ',' `elem` val
        then chat_cache chat2 (HN hn) (Just val)
        else return ()
      return (Just chat2)

{-
chat_t chat_add(chat_t chat, char *hn, char *val)
{
  if(!chat || !hn || !val) return NULL;
  if(!chat->local) return NULL; // has to be our own to add
  packet_set_str(chat->roster,hn,val);
  chat_rhash(chat);
  chat_sync(chat);
  // try to load if it's a message id
  if(strchr(val,',')) chat_cache(chat,hn,val);
  return chat;
}
-}

-- ---------------------------------------------------------------------

{-
// participant permissions, -1 blocked, 0 read-only, 1 allowed
int chat_perm(chat_t chat, char *hn)
{
  char *val;
  if(!chat || !hn) return -1;
  val = packet_get_str(chat->roster,hn);
  if(!val) val = packet_get_str(chat->roster,"*");
  if(!val) return 0;
  if(util_cmp(val,"blocked") == 0) return -1;
  return 1;
}

// this is the hub channel, it just receives notes
chat_t chat_hub(chat_t chat)
{
  packet_t p, note, req, resp;
  char *path, *thtp, *id;
  
  while((note = chan_notes(chat->hub)))
  {
    thtp = packet_get_str(note,"thtp");

    // incoming requests
    if(util_cmp(thtp,"req") == 0)
    {
      req = packet_linked(note);
      DEBUG_PRINTF("note req packet %.*s", req->json_len, req->json);
      path = packet_get_str(req,"path");
      p = packet_new();
      packet_set_int(p,"status",404);
      if(path && strstr(path,"/roster"))
      {
        packet_set_int(p,"status",200);
        packet_body(p,chat->roster->json,chat->roster->json_len);
        DEBUG_PRINTF("roster '%.*s' %d",p->body_len,p->body,packet_len(p));
      }
      if(path && (id = strstr(path,"/id/")) && (id += 4) && (resp = xht_get(chat->log,id)))
      {
        packet_set_int(p,"status",200);
        packet_body(p,packet_raw(resp),packet_len(resp));
      }
      packet_link(note,p);
      chan_reply(chat->hub,note);
    }

    // answers to our requests
    if(util_cmp(thtp,"resp") == 0)
    {
      resp = packet_linked(note);
      path = packet_get_str(note,"path");
      DEBUG_PRINTF("note resp packet %s %.*s", path, resp->json_len, resp->json);
      if(strstr(path,"/roster"))
      {
        p = packet_new();
        if(packet_json(p,resp->body,resp->body_len) == 0)
        {
          packet_free(chat->roster);
          chat->roster = p;
          chat_sync(chat);
        }else{
          packet_free(p);
        }
      }
      if(strstr(path,"/id/"))
      {
        p = packet_parse(resp->body,resp->body_len);
        if(p)
        {
          id = packet_get_str(note,"for");
          packet_set_str(p,"from",id);
          chat_log(chat,p);
          // is either a join to be processed or an old msg
          if(util_cmp(packet_get_str(p,"type"),"join") == 0) chat_restate(chat,id);
          else chat_push(chat,packet_copy(p));
        }
      }
      packet_free(note);
    }
  }

  return chat->msgs ? chat : NULL;
}

chat_t ext_chat(chan_t c)
{
  packet_t p, msg;
  chatr_t r = c->arg;
  chat_t chat = NULL;
  int perm;
  char *id;

  // this is the hub channel, process it there
  if(r && r->chat->hub == c) return chat_hub(r->chat);

  // channel start request
  if(!r)
  {
    if(!(p = chan_pop(c))) return (chat_t)chan_fail(c,"500");
    chat = chat_get(c->s,packet_get_str(p,"to"));
    if(!chat) return (chat_t)chan_fail(c,"500");
    perm = chat_perm(chat,c->to->hexname);
    id = packet_get_str(p,"from");
    DEBUG_PRINTF("chat %s from %s is %d",chat->id,c->to->hexname,perm);
    if(perm < 0) return (chat_t)chan_fail(c,"blocked");
    if(perm == 0 && id) return (chat_t)chan_fail(c,"read-only");

    // legit new chat conn
    r = c->arg = chatr_new(chat);
    r->online = 1;
    xht_set(chat->conn,c->to->hexname,c);

    // response
    p = chan_packet(c);
    if(chat->join)
    {
      r->joined = 1;
      packet_set_str(p,"from",chat->join);
    }

    // add to roster if given
    if(id) chat_add(chat,c->to->hexname,id);
    packet_set_str(p,"roster",chat->rhash);
    
    // re-fetch roster if hashes don't match
    if(util_cmp(packet_get_str(p,"roster"),chat->rhash) != 0) chat_cache(chat,chat->origin->hexname,NULL);

    chan_send(c,p);
  }
  
  // response to a join
  if(!r->online && (p = chan_pop(c)))
  {
    id = packet_get_str(p,"from");
    DEBUG_PRINTF("chat online %s from %s %s",r->chat->id,c->to->hexname,id?id:packet_get_str(p,"err"));
    if(!id) return (chat_t)chan_fail(c,"invalid");
    r->online = 1;
    chat_restate(r->chat,c->to->hexname);
    packet_free(p);
  }

  while((p = chan_pop(c)))
  {
    DEBUG_PRINTF("chat packet %.*s", p->json_len, p->json);
    packet_append(r->in,p->body,p->body_len);
    if(util_cmp(packet_get_str(p,"done"),"true") == 0)
    {
      msg = packet_parse(r->in->body,r->in->body_len);
      if(msg)
      {
        packet_set_str(msg,"from",c->to->hexname);
        chat_push(r->chat,msg);        
      }
      packet_body(r->in,NULL,0);
    }
    packet_free(p);
  }
  
  // optionally sends ack if needed
  chan_ack(c);
  
  if(c->state == CHAN_ENDING || c->state == CHAN_ENDED)
  {
    // if it's this channel in the index, zap it
    if(xht_get(r->chat->conn,c->to->hexname) == c) xht_set(r->chat->conn,c->to->hexname,NULL);
    chatr_free(r);
    c->arg = NULL;
  }

  return r->chat->msgs ? r->chat : NULL;
}

packet_t chat_participant(chat_t chat, char *hn)
{
  if(!chat) return NULL;
  return xht_get(chat->log,hn);
}

packet_t chat_iparticipant(chat_t chat, int index)
{
  if(!chat) return NULL;
  return xht_get(chat->log,packet_get_istr(chat->roster,index*2));
}
-}
