{-# LANGUAGE OverloadedStrings #-}
module Network.TeleHash.Ext.Chat
  (
    ext_chat
  , chat_get
  , chat_add
  , chat_message
  , chat_join
  , chat_free
  , chat_send
  , chat_pop
  , chat_pop_all
  , chat_participant

  ) where

-- import Control.Applicative
-- import Control.Concurrent
import Control.Exception
import Control.Monad
-- import Control.Monad.Error
import Control.Monad.State
-- import Crypto.Random
-- import Data.Aeson (object,(.=), (.:), (.:?) )
-- import Data.Aeson.Encode
import Data.Aeson.Types
import Data.Bits
import Data.Char
-- import Data.IP
import Data.List
import Data.List.Split
import Data.Maybe
-- import Data.String.Utils
-- import Data.Text.Lazy.Builder
-- import Data.Typeable
import Data.Word
-- import Network.BSD
-- import Network.Socket
import Prelude hiding (id, (.), head, either)
-- import System.IO
-- import System.Log.Handler.Simple
-- import System.Log.Logger
import System.Time

import Network.TeleHash.Convert
-- import Network.TeleHash.Crypt
import Network.TeleHash.Ext.Thtp
import Network.TeleHash.Hn
import Network.TeleHash.Packet
-- import Network.TeleHash.Path
-- import Network.TeleHash.Paths
import Network.TeleHash.SwitchApi
import Network.TeleHash.SwitchUtils
import Network.TeleHash.Types
import Network.TeleHash.Utils

-- import qualified Crypto.Hash.SHA256 as SHA256
-- import qualified Crypto.PubKey.DH as DH
-- import qualified Crypto.Types.PubKey.ECDSA as ECDSA
import qualified Data.Aeson as Aeson
-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
-- import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
-- import qualified Data.Set as Set
import qualified Data.Text as Text
-- import qualified Data.Text.Lazy as TL
-- import qualified Network.Socket as NS
-- import qualified Network.Socket.ByteString as SB

-- ---------------------------------------------------------------------

chatr_new :: Chat -> Uid -> TeleHash ChatR
chatr_new chat uid = do
  crid <- getNextCrid
  let cr =
        ChatR
          { ecrId = crid
          , ecrChat = (ecId chat)
          , ecrIn = packet_new_rx
          , ecrJoined = False
          , ecrOnline = False
          , ecrChan = uid
          }
  putChatR cr
  return cr

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

chat_rhash :: ChatId -> TeleHash ChatHash
chat_rhash cid = do
  chat <- getChat cid
  logT $ "chat_rhash:chat=" ++ show chat
  let r = intercalate "," $ concatMap (\(k,v) -> [k,v]) $ Map.toAscList (ecRoster chat)
  logT $ "chat_rhash:r=" ++ show r
  let rhash = thash r
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

-- |
-- Calculate murmur3 hash of a string
--
-- >>> thash "49eb85838a320f60ce1894234f7d1ec04ec5957cb4644fa11750e01a6c88b58a6ca4ea6d,1000"
-- CH "d49cf6fc"
--
-- >>> thash "tft@0ecedc9f49472737b9285c0e10066fd860983bb5aa3a04e1f0acc3d3b3c5e348"
-- CH "56419861"
--
thash :: String -> ChatHash
thash v = CH (word32AsHexString $ thash_raw v)

thash_raw :: String -> Word32
thash_raw v = r
  where
    c1 = 0xcc9e2d51
    c2 = 0x1b873593
    m2 = 0xe6546b64

    -- v = "49eb85838a320f60ce1894234f7d1ec04ec5957cb4644fa11750e01a6c88b58a6ca4ea6d,1000"
    -- v = "foobar"
    go acc vv = case take 4 vv of
                  [] -> acc
                  x -> go (acc ++ [x]) (drop 4 vv)
    vs = go [] v

    -- See https://en.wikipedia.org/wiki/MurmurHash
    mkTail :: Word32 -> Word32 -> Word32
    mkTail h1 ts = r1
      where
        k1 = ts
        k1_2 = k1 * c1
        k1_3 = rotateL k1_2 15
        k1_4 = k1_3 * c2
        r1 = h1 `xor` k1_4
     -- c code
     --  k1 *= c1; k1 = rotl32(k1,15); k1 *= c2; h1 ^= k1;


    mkHash :: Word32 -> Word32 -> Word32
    mkHash h1 inVal = h1_4
      where
        k1_1 = inVal * c1
        k1_2 = rotateL k1_1 15
        k1_3 = k1_2 * c2

        h1_2 = h1 `xor` k1_3
        h1_3 = rotateL h1_2 13
        h1_4 = h1_3 * 5 + m2

{-
    int i;
    for(i = -nblocks; i; i++)
    {
        uint32_t k1 = blocks[i];

        k1 *= c1;
        k1 = rotl32(k1, 15);
        k1 *= c2;

        h1 ^= k1;
        h1 = rotl32(h1, 13);
        h1 = h1*5+0xe6546b64;
    }

-}

    mkFinalize :: Word32 -> Word32 -> Word32
    mkFinalize len h1 = h1_6
      where
        h1_1 = h1   `xor` len
        h1_2 = h1_1 `xor` (shiftR h1_1 16)
        h1_3 = h1_2 * 0x85ebca6b;
        h1_4 = h1_3 `xor` (shiftR h1_3 13)
        h1_5 = h1_4 * 0xc2b2ae35
        h1_6 = h1_5 `xor` (shiftR h1_5 16)

{-
    //----------
    // finalization

    h1 ^= len;

    h1 ^= h1 >> 16;
    h1 *= 0x85ebca6b;
    h1 ^= h1 >> 13;
    h1 *= 0xc2b2ae35;
    h1 ^= h1 >> 16;

-}
    strTow32 :: String -> Word32
    strTow32 str = foldl (\acc vv -> 256 * acc + (fromIntegral vv)) (0::Word32)
                 $ BL.unpack $ cbsTolbs $ BC.pack $ reverse str

    vsw32 = map strTow32 vs
    t = last vsw32
    vsm = init vsw32
    -- r = word32AsHexString $ Murmur.hash $ map vsw32 vs
    -- r = (t,vsm,length v `mod` 4)
    mainHash = foldl' mkHash 0 vsm
    -- mainHashP = scanl mkHash 0 vsm
    tailHash = if ((length (last vs)) `mod` 4) == 0
                  then mkHash mainHash t
                  else mkTail mainHash t
    finalHash = mkFinalize (fromIntegral $ length v) tailHash
    -- r = (word32AsHexString finalHash,word32AsHexString mainHash,word32AsHexString tailHash,map word32AsHexString vsw32,vs,map word32AsHexString mainHashP)
    r = finalHash


{-


about to call util_murmur for 49eb85838a320f60ce1894234f7d1ec04ec5957cb4644fa11750e01a6c88b58a6ca4ea6d,1000

util_mmh32 entered,len=77
util_mmh32 nblocks=19
util_mmh32 in body, k1=0x62653934
util_mmh32 in body, h1=0x00000000
util_mmh32 in body, k1=0x33383538
util_mmh32 in body, h1=0xa64a1766
util_mmh32 in body, k1=0x32336138
util_mmh32 in body, h1=0x0de2c89f
util_mmh32 in body, k1=0x30366630
util_mmh32 in body, h1=0x42769105
util_mmh32 in body, k1=0x38316563
util_mmh32 in body, h1=0x3cf2f6fc
util_mmh32 in body, k1=0x33323439
util_mmh32 in body, h1=0xf2e54095
util_mmh32 in body, k1=0x64376634
util_mmh32 in body, h1=0x67dbd725
util_mmh32 in body, k1=0x30636531
util_mmh32 in body, h1=0x18f5216a
util_mmh32 in body, k1=0x35636534
util_mmh32 in body, h1=0xe938bf67
util_mmh32 in body, k1=0x63373539
util_mmh32 in body, h1=0x6a9055ec
util_mmh32 in body, k1=0x34363462
util_mmh32 in body, h1=0xdfe7f00f
util_mmh32 in body, k1=0x31616634
util_mmh32 in body, h1=0xdfa38d5c
util_mmh32 in body, k1=0x30353731
util_mmh32 in body, h1=0x3b1c397f
util_mmh32 in body, k1=0x61313065
util_mmh32 in body, h1=0x9770bba3
util_mmh32 in body, k1=0x38386336
util_mmh32 in body, h1=0x2ae4260e
util_mmh32 in body, k1=0x61383562
util_mmh32 in body, h1=0x6578861b
util_mmh32 in body, k1=0x34616336
util_mmh32 in body, h1=0x2be3e250
util_mmh32 in body, k1=0x64366165
util_mmh32 in body, h1=0x4771a2c9
util_mmh32 in body, k1=0x3030312c
util_mmh32 in body, h1=0x286df121
util_mmh32 after body 0xe5b8da39
util_mmh32 after tail 0xf6581d85
util_mmh32 final 0xd49cf6fc

-------------------------------

created chat tft@49eb85838a320f60ce1894234f7d1ec04ec5957cb4644fa11750e01a6c88b58a ec87f1ff,1000 510200ed
about to call util_murmur for [foobar]util_mmh32 entered
util_mmh32 in body, k1=0x626f6f66
util_mmh32 after body 0x07f455ca
util_mmh32 after tail 0x8fddf7c0
util_mmh32 final 0xa4c4d4bd
-}
-- ---------------------------------------------------------------------

-- |if mid is Nothing it requests the roster, otherwise requests message id
chat_cache :: ChatId -> HashName -> Maybe String -> TeleHash ()
chat_cache cid hn mid = do
  chat <- getChat cid
  logT $ "chat_cache:chat=" ++ show chat
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
                            then packet_set_str note3 "path" ("/chat/" ++ (unCH $ ecIdHash chat) ++ "/roster")
                            else packet_set_str note3 "path" ("/chat/" ++ (unCH $ ecIdHash chat) ++ "/id/" ++ fromJust mid)
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

-- |return the chat if we aready know about it, else create a new one
chat_get :: Maybe String -> TeleHash (Maybe Chat)
chat_get mid = do
  logT $ "chat_get:mid=" ++ show mid
  sw <- get
  mcid <- case mid of
    Just sid -> do
      -- if there's an id, validate and optionally parse out originator
      case parseChatId sid of
        Nothing -> do
          logT $ "invalid chatid:" ++ sid
          return Nothing
        Just cid -> do
          logT $ "chat_get:got cid" ++ show cid
          case ciOriginator cid of
            Just hn -> do
              _hc <- hn_get hn -- make sure we know about the hn
              return $ Just (cid { ciOriginator = Just hn })
            Nothing -> return $ Just (cid { ciOriginator = Just  (swId sw)})

    Nothing -> do
      epVal <- randomHEX 8
      return $ Just (ChatId epVal (Just (swId sw)))

  logT $ "chat_get:mcid=" ++ show mcid
  case mcid of
    Nothing -> return Nothing
    Just cid -> do
      case Map.lookup cid (swIndexChat sw) of
        Just chat -> return (Just chat)
        Nothing -> do
          logT $ "chat_get:making new chat"
          hubc <- chan_new (swId sw) "chat" Nothing

          logT $ "chat_get:hub=" ++ showChan hubc

          randWord32 <- randomWord32
          let chat = Chat
                { ecEp     = ciEndpoint cid
                , ecId     = cid
                , ecIdHash = thash (chatIdToString cid)
                , ecOrigin = gfromJust "chat_get" (ciOriginator cid)
                , ecRHash  = CH ""
                , ecLocal  = ciOriginator cid == Just (swId sw)
                , ecSeed   = CH $ word32AsHexString randWord32
                , ecSeq    = 1000
                , ecRoster = Map.empty
                , ecConn   = Map.empty
                , ecLog    = Map.empty
                , ecMsgs   = []
                , ecJoin   = Nothing
                , ecSent   = Nothing
                , ecAfter  = Nothing

                , ecHub    = chUid hubc

                }
          -- an admin channel for distribution and thtp requests
          chatr <- chatr_new chat (chUid hubc)
          let hub = hubc { chArg = CArgChatR (ecrId chatr) }
          putChan hub
          note <- chan_note hub Nothing
          let note2 = packet_set_str note "glob" ("/chat/" ++ (unCH $ ecIdHash chat) ++ "/")
          logT $ "chat_get:chat,glob:" ++ show (ecId chat,packet_get_str note2 "glob")
          logT $ "chat_get:glob full:" ++ show (note2)
          thtp_glob Nothing note2
          putChat chat

          -- any other hashname and we try to initialize
          if not (ecLocal chat)
            then do
              chat_cache (ecId chat) (ecOrigin chat) Nothing
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

chat_free :: ChatId -> TeleHash ()
chat_free cid = do
  logT $ "chat_free:" ++ chatIdToString cid
  sw <- get
  -- TODO: individually release the connections etc
  put $ sw {swIndexChat = Map.delete cid (swIndexChat sw) }

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

chat_message :: ChatId -> TeleHash (Maybe TxTelex)
chat_message cid = do
  chat <- getChat cid
  logT $ "chat_message:chat=" ++ show chat
  (TOD at _)  <- io getClockTime
  if ecSeq chat == 0
    then return Nothing
    else do
      sw <- get
      let p1 = packet_new (swId sw)
      let idVal = (ecSeed chat)
      logT $ "chat_message:idVal=" ++ unCH idVal
      -- do repeated murmur hash
      let idW32 = last $ take (fromIntegral $ ecSeq chat) $ iterate (\h -> thash $ unCH h) (ecSeed chat)
          idHex = unCH idW32
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

-- |add msg to queue
chat_push :: ChatId -> RxTelex -> TeleHash ()
chat_push cid p = do
  chat <- getChat cid
  logT $ "chat_push:chat=" ++ show chat
  putChat $ chat { ecMsgs = (ecMsgs chat) ++ [p]}

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
-}

-- ---------------------------------------------------------------------

chat_pop_all :: ChatId -> TeleHash [RxTelex]
chat_pop_all cid = do
  chat <- getChat cid
  logT $ "chat_pop_all:chat=" ++ show chat
  let msgs = ecMsgs chat
  putChat $ chat { ecMsgs = [] }
  return msgs

-- ---------------------------------------------------------------------

chat_pop :: ChatId -> TeleHash (Maybe RxTelex)
chat_pop cid = do
  chat <- getChat cid
  logT $ "chat_pop:chat=" ++ show chat
  if null (ecMsgs chat)
    then return Nothing
    else do
      let p = head (ecMsgs chat)
      putChat $ chat { ecMsgs = tail (ecMsgs chat) }
      return (Just p)

{-
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
      logT $ "chat_restate:chat=" ++ show chat

      -- load from roster
      case Map.lookup hn (ecRoster chat) of
        Nothing -> return ()
        Just idVal -> do
          --  see if there's a join message cached
          state1 <- case Map.lookup idVal (ecLog chat) of
            Just joinVal -> do
              return joinVal
            Nothing -> do
              if ',' `elem` idVal
                then do
                  -- we should have the join id, try to get it again
                  void $ chat_cache cid (HN hn) (Just idVal)
                  return $ packet_set_str (packet_new (HN hn)) "text" "connecting"
                else do
                  return $ packet_set_str (packet_new (HN hn)) "text" idVal
          -- make a state packet
          let state2 = packet_set_str state1 "type" "state"
              state3 = packet_set_str state2 "from" hn

          state4 <- case Map.lookup (HN hn) (ecConn chat) of
            Just uid -> do
              mcr <- getChatRFromChan uid
              case mcr of
                Just r -> do
                  if ecrOnline r
                    then do
                      return $ packet_set_str state3 "online" "true"
                    else do
                      return $ packet_set_str state3 "online" "false"
                Nothing -> do
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
  logT $ "chat_sync:chat=" ++ show chat
  let joined = case ecJoin chat of
                 Nothing -> False
                 Just _  -> True
  let roster = Map.toAscList (ecRoster chat)
  forM_ roster $ \(part,v) -> do
    logT $ "chat_sync:processing (part,v)" ++ show (part,v)
    if length part /= 64 || (swId sw) == HN part
      then do
        logT $ "chat_sync:not processing part " ++ show part
        return ()
      else do
        cont <- case Map.lookup (HN part) (ecConn chat) of
          Just uid -> do
            logT $ "chat_sync:got channel uid:" ++ show uid
            mr <- getChatRFromChan uid
            case mr of
              Just r -> do
                logT $ "chat_sync:(ecrJoined r,joined)=" ++ show (ecrJoined r,joined)
                if ecrJoined r == joined
                  then return False
                  else return True
              Nothing -> do
                logT $ "chat_sync: unexpected cArg:" ++ show mr
                return False
          Nothing -> return True

        if not cont
           then return ()
           else do
            logT $ "chat_sync:initiating chat to " ++ show (part,ecId chat)
            -- state change
            chat_restate (ecId chat) part

            -- try to connect
            c <- chan_start (HN part) "chat"
            putChan c

            chat2 <- getChat cid
            logT $ "chat_sync:chat2=" ++ show chat2

            let chat3 = chat2 { ecConn = Map.insert (HN part) (chUid c) (ecConn chat2) }
            putChat chat3

            cr <- chatr_new chat3 (chUid c)
            let r = cr { ecrJoined = joined }
                c2 = c { chArg = CArgChatR (ecrId r) }
            putChatR r
            putChan c2
            mp <- chan_packet (chUid c2) True
            let p1 = gfromJust "chat_sync" mp
                p2 = packet_set_str p1 "to" (chatIdToString $ ecId chat3 )
                p3 = packet_set_str p2 "from" (gfromJust "chat_sync" $ ecJoin chat)
                p4 = packet_set_str p3 "roster" (unCH (ecRHash chat))
            logT $ "chat_sync:p4=" ++ show p4
            chan_send (chUid c2) p4

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
  logT $ "chat_log:chat=" ++ show chat
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

chat_join :: ChatId -> TxTelex -> TeleHash (Maybe Chat)
chat_join cid joinVal = do
  chat <- getChat cid
  logT $ "chat_join:(ecJoin,join)=" ++ show (ecJoin chat,joinVal)
  -- paranoid, don't double-join
  if isJust (ecJoin chat)
     && ((ecJoin chat) == (packet_get_str joinVal "id"))
    then do
      logT $ "chat_join: avoiding double join"
      return Nothing
    else do
      let j2 = packet_set_str joinVal "type" "join"
          chat2 = chat {ecJoin = packet_get_str j2 "id"}
      putChat chat2
      chat_log (ecId chat2) j2
      chat3 <- getChat (ecId chat2)
      logT $ "chat_join:chat3=" ++ show chat3
      sw <- get
      let chat4 = chat3 { ecRoster = Map.insert (unHN $ swId sw)
                                                (gfromJust "chat_join" $ ecJoin chat3)
                                                (ecRoster chat3) }
      putChat chat4
      chat_restate (ecId chat4) (unHN $ swId sw)
      chat5 <- getChat cid
      logT $ "chat_join:chat5=" ++ show chat5
      void $ chat_rhash cid

      -- create/activate all chat channels
      chat_sync cid
      chat6 <- getChat cid
      logT $ "chat_join:ecRoster=" ++ show (ecRoster chat6)
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

-- |chunk the packet out
chat_chunk :: Uid -> TxTelex -> TeleHash ()
chat_chunk cid msg = do
  util_chunk_out cid msg "done"

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

chat_send :: ChatId -> TxTelex -> TeleHash ()
chat_send cid msg = do
  chat <- getChat cid
  chat_log (ecId chat) msg

  logT $ "chat_send:ecConn=" ++ show (ecConn chat)

  -- send as a note to all connected
  forM_ (Map.toList $ ecRoster chat) $ \(hn,idVal) -> do
    logT $ "chat_send:processing " ++ show (hn,idVal)
    case Map.lookup (HN hn) (ecConn chat) of
      Nothing -> return ()
      Just cuid -> do
        logT $ "chat_send:got cuid:" ++ show cuid
        mr <- getChatRFromChan cuid
        case mr of
          Just r -> do
            logT $ "chat_send:got r:" ++ show r
            if (not (ecrJoined r) || not (ecrOnline r))
              then return ()
              else do
                logT $ "chat_send:calling chat_chunk"
                chat_chunk cuid msg
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

chat_add :: ChatId -> String -> String -> TeleHash (Maybe Chat)
chat_add cid hn val = do
  chat <- getChat cid
  logT $ "chat_add:chat=" ++ show chat

  logT $ "chat_add:(hn,val)=" ++ show (hn,val)
  if hn == "" || val == "" || (not $ ecLocal chat)
    then return Nothing
    else do
      let roster = Map.insert hn val (ecRoster chat)
          chat2 = chat { ecRoster = roster }
      putChat chat2
      logT $ "chat_add:ecRoster=" ++ show (ecRoster chat2)
      void $ chat_rhash cid
      chat_sync cid
      -- try to load if it's a message id
      if ',' `elem` val
        then chat_cache cid (HN hn) (Just val)
        else return ()
      chat3 <- getChat cid
      logT $ "chat_add:chat3=" ++ show chat3
      return (Just chat3)

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

-- |participant permissions, -1 blocked, 0 read-only, 1 allowed
chat_perm :: ChatId -> HashName -> TeleHash ChatPerm
chat_perm chid hn = do
  chat <- getChat chid
  logT $ "chat_perm:chat=" ++ show chat
  let val =case Map.lookup (unHN hn) (ecRoster chat) of
             Nothing ->
              case Map.lookup "*" (ecRoster chat) of
                Just v -> v
                Nothing -> ""
             Just v -> v

  case val of
    "blocked" -> return PermBlocked
    ""        -> return PermReadOnly
    _         -> return PermAllowed

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
-}

-- ---------------------------------------------------------------------

-- |this is the hub channel, it just receives notes
chat_hub :: ChatId -> TeleHash (Maybe Chat)
chat_hub chatId = do
  chat <- getChat chatId
  -- logT $ "chat_hub:chat=" ++ show chat
  hub <- getChan (ecHub chat)
  notes <- chan_notes_all hub
  forM_ notes $ \note -> do
    -- logT $ "chat_hub:got note " ++ show note
    let mthtp = packet_get_str note "thtp"
    case mthtp of
      Nothing -> do
        logT $ "chat_hub:got non-thtp response:" ++ show note
        assert False undefined

      Just "req" -> do
        -- incoming requests
        let pn = packet_new (HN "chat_hub")
        p <- case packet_linked note of
          Just req -> do
            -- logT $ "chat_hub:note req packet " ++ show req
            let mj = packetJson (tPacket req)
            -- logT $ "chat_hub:note req packet json " ++ show mj
            case mj of
              Nothing -> do
                return $ packet_set_int pn "status" 404
              Just (Aeson.Object js) -> do
                let mpath = HM.lookup "path" js
                case mpath of
                  Nothing -> do
                    return $ packet_set_int pn "status" 404
                  Just (Aeson.String path) -> do
                    if isInfixOf "/roster" (Text.unpack path)
                      then do
                        let p2 = packet_set_int pn "status" 200
                            p3 = packet_body p2 (lbsTocbs $ Aeson.encode (ecRoster chat))
                        -- logT $ "chat_hub:ecRoster=" ++ show (ecRoster chat)
                        -- logT $ "chat_hub:roster p3=" ++ show p3
                        return p3
                      else do
                        -- /chat/56419861/id/01544f0d,1000
                        if isInfixOf "/id/" (Text.unpack path)
                          then do
                            -- logT $ "chat_hub:path=" ++ show path
                            let pathParts = splitOn "/" (Text.unpack path)
                                idVal = last pathParts
                            -- logT $ "chat_hub:idVal=" ++ show idVal
                            case Map.lookup idVal (ecLog chat) of
                              Nothing -> do
                                logT $ "chat_hub:no log for " ++ show idVal
                                return $ packet_set_int pn "status" 404
                              Just resp -> do
                                (LP respBody) <- packet_raw resp
                                let p2 = packet_set_int pn "status" 200
                                    p3 = packet_body p2 respBody
                                -- logT $ "chat_hub:id p3=" ++ show p3
                                return p3
                          else do
                            assert False undefined
                  Just aeson -> do
                    logT $ "chat_hub:unexpected js1:" ++ show aeson
                    return $ packet_set_int pn "status" 404
              Just aeson -> do
                logT $ "chat_hub:unexpected js2:" ++ show aeson
                return $ packet_set_int pn "status" 404
          Nothing -> do
            assert False undefined
        let note2 = packet_link (Just note) p
        chan_reply (ecHub chat) note2

      Just "resp" -> do
        -- answers to our requests
        let resp = tPacket note
            mpath = packet_get_str note "path"
        -- logT $ "chat_hub:note resp packet " ++ show (mpath, resp)
        case mpath of
          Nothing -> do
            assert False undefined
          Just path -> do
            if isInfixOf "/roster" path
              then do
                let -- mjhead = packetJson resp
                    mjbody = Aeson.decode (cbsTolbs $ unBody $ paBody resp) :: Maybe Aeson.Value
                -- logT $ "chat_hub:(mjhead,mjbody)=" ++ show (mjhead,mjbody)
                case mjbody of
                  Nothing -> return ()
                  Just (Object hm) -> do
                    let
                      doOne (k,String v) = (Text.unpack k,Text.unpack v)
                      doOne (k,v) = error $ "chat_hub:doOne got " ++ show (k,v)
                      rval = map doOne $ HM.toList hm
                    -- logT $ "chat_hub:rval=" ++ show rval
                    let chat2 = chat { ecRoster = Map.fromList rval }
                    putChat chat2
                    chat_sync (ecId chat2)
                  _ -> do
                    logT $ "chat_hub:got unexpected JSON value:" ++ show mjbody
                    assert False undefined
                return Ok
              else return Ok
      Just unk -> do
        logT $ "chat_hub:got unexpected thtp type:" ++ unk
        return Fail

  chat2 <- getChat (ecId chat)
  -- logT $ "chat_hub:chat2=" ++ show chat2
  if null (ecMsgs chat2)
    then return Nothing
    else return (Just chat2)


{-
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
-}

-- ---------------------------------------------------------------------

ext_chat :: Uid -> TeleHash (Maybe Chat)
ext_chat cid = do
  c <- getChan cid
  mr <- getChatRFromChan cid
  -- logT $ "ext_chan:mr=" ++ show mr
  case mr of
    Just r -> do
      chat <- getChat (ecrChat r)
      -- logT $ "ext:chat=" ++ show chat
      if ecHub chat == cid
        then do
          -- this is the hub channel, process it there
          _mchat <- chat_hub (ecrChat r)
          return ()
        else do
          logT $ "ext_chat:processing response to a join"
          -- response to a join
          if (not $ ecrOnline r)
            then do
              mp <- chan_pop cid
              case mp of
                Nothing -> return ()
                Just p -> do
                  logT $ "ext_chat:processing join response:" ++ show p
                  case packet_get_str p "err" of
                    Just _errVal -> do
                      logT $ "ext_chat:join response has error " ++ show p
                      -- assert False undefined
                      return ()
                    Nothing -> do
                      let mid = packet_get_str p "from"
                      case mid of
                        Nothing -> do
                          chan_fail cid (Just "invalid")
                          return ()
                        Just _idVal -> do
                          let r2 = r { ecrOnline = True }
                          putChatR r2
                          chat_restate (ecrChat r) (unHN $ chTo c)
                          return ()
            else return ()


    Nothing -> do
      -- channel start request
      mp <- chan_pop cid
      logT $ "ext_chat:chan start:mp=" ++ show mp
      case mp of
        Nothing -> do
          logT $ "ext_chat:chan start got bad channel"
          -- chansStr <- showAllChans
          -- logT $ "ext_chat:current channels:\n" ++ chansStr
          chan_fail cid (Just "500")
          return ()
        Just p -> do
          mchat <- chat_get (packet_get_str p "to")
          case mchat of
            Nothing -> do
              chan_fail cid (Just "500")
              return ()
            Just chat -> do
              logT $ "ext_chat: got chat " ++ show chat
              perm <- chat_perm (ecId chat) (chTo c)
              let idVal = packet_get_str_always p "from"
              -- logT $ "ext_chat:chat from is:" ++ show (idVal,chatIdToString $ ecId chat,chTo c,perm)
              continue <- case perm of
                PermBlocked -> do
                  chan_fail cid (Just "blocked")
                  return False
                PermReadOnly -> do
                  if idVal /= ""
                    then do
                      chan_fail cid (Just "read-only")
                      return False
                    else return True
                PermAllowed -> do
                  return True
              if not continue
                then return ()
                else do
                  -- legit new chat conn
                  logT $ "ext_chat:legit new chat conn"
                  cr <- chatr_new chat (chUid c)
                  let r     = cr { ecrOnline = True }
                      c2    = c { chArg = CArgChatR (ecrId r) }
                      chat2 = chat { ecConn = Map.insert (chTo c) (chUid c2) (ecConn chat) }
                  putChatR r
                  putChan c2
                  putChat chat2

                  -- response
                  mp2 <- chan_packet (chUid c2) True
                  let p1 = gfromJust "ext_chat" mp2
                  p2 <- case ecJoin chat2 of
                    Nothing -> return p1
                    Just j -> do
                      let r2' = r { ecrJoined = True }
                          p2' = packet_set_str p1 "from" j
                      putChatR r2'
                      return p2'

                  -- add to roster if given
                  if idVal /= ""
                    then do
                      void $ chat_add (ecId chat2) (unHN $ chTo c) idVal
                    else return ()
                  chat3 <- getChat (ecId chat2)
                  -- logT $ "ext_chat:chat3=" ++ show chat3

                  -- re-fetch roster if hashes don't match
                  if packet_get_str_always p2 "roster" /= (unCH $ ecRHash chat3)
                    then do
                      chat_cache (ecId chat3) (ecOrigin chat3) Nothing
                    else return ()

                  let p3 = packet_set_str p2 "roster" (unCH $ ecRHash chat3)

                  logT $ "ext_chat:p3=" ++ show p3

                  void $ chan_send (chUid c2) p3
                  return ()

  rxs <- chan_pop_all cid
  forM_ rxs $ \p -> do
    -- logT $ "ext_chat:chat packet " ++ showJson (rtJs p)
    -- logT $ "ext_chat:chat packet body " ++ show (paBody $ rtPacket p)
    mr2 <- getChatRFromChan cid
    if isJust mr2
      then do
        let rIn = ecrIn $ fromJust mr2
        let rin2 = packet_append rIn (unBody $ paBody $ rtPacket p)
        putChatR ((fromJust mr2) { ecrIn = rin2 })
      else return ()
    -- logT $ "ext_chat:chat packet done str=" ++ show (packet_get_str_always p "done")
    if packet_get_str_always p "done" == "Bool True"
      then do
        mr3 <- getChatRFromChan cid
        -- logT $ "ext_chat:got done msg:(mr3,p)=" ++ show (mr3,p)
        case mr3 of
          Nothing -> do
            logT $ "ext_chat:mr3=Nothing"
            return ()
          Just r -> do
            let pp = fromLinePacket (LP $ unBody $ paBody $ rtPacket $ ecrIn r)
            logT $ "ext_chat:pp=" ++ show pp
            case pp of
              Just p2@(Packet (HeadJson js) _body) -> do
                let mjson = Aeson.decode (cbsTolbs js) :: Maybe Aeson.Value
                case mjson of
                  Nothing -> do
                    logT $ "invalid js in packet:" ++ show js
                    return ()
                  Just (Aeson.Object jsHashMap) -> do
                    c2 <- getChan cid
                    let rp = packet_new_rx { rtPacket = p2
                                           , rtJs = jsHashMap
                                           }
                        rp2 = packet_set_str rp "from" (unHN $ chTo c2)
                    chat_push (ecrChat r) rp2
                    putChatR (r { ecrIn = packet_new_rx })
                  Just _ -> do
                    logT $ "ext_chat:unexpeced value for mjson" ++ show mjson
                    return ()
              _ -> do
                logT $ "ext_chat:unexpected value for pp" ++ show pp
                return ()

      else return ()

  -- optionally send ack if needed
  logT $ "ext_chat:re-instate chan_ack"
  chan_ack cid
  logT $ "ext_chat:re-instate chan_ack done"

  c2 <- getChan cid
  ok <- if isJust mr && (chState c2 == ChanEnding || chState c2 == ChanEnded)
          then do
            -- if its this channel in the index, zap it
            chat <- getChat (ecrChat $ fromJust mr)
            logT $ "ext_chat:n chat=" ++ show chat
            putChan $ c2 { chArg = CArgNone }
            case Map.lookup (chTo c2) (ecConn chat) of
              Just _uid -> do
                let chat2 = chat { ecConn = Map.delete (chTo c2) (ecConn chat) }
                putChat chat2
                return False
              Nothing -> do
                return True
          else return True
  if ok && isJust mr
    then do
      chat3 <- getChat (ecrChat $ fromJust mr)
      -- logT $ "ext_chat:final chat="  ++ show chat3
      if null (ecMsgs chat3)
        then return Nothing
        else return (Just chat3)
    else return Nothing


{-
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
-}

-- ---------------------------------------------------------------------

chat_participant :: ChatId -> String -> TeleHash (Maybe TxTelex)
chat_participant cid hn = do
  chat <- getChat cid
  logT $ "chat_participant:(hn,chat)=" ++ show (hn,chat)
  return $ Map.lookup hn (ecLog chat)

{-
packet_t chat_participant(chat_t chat, char *hn)
{
  if(!chat) return NULL;
  return xht_get(chat->log,hn);
}
-}

-- ---------------------------------------------------------------------

{-
packet_t chat_iparticipant(chat_t chat, int index)
{
  if(!chat) return NULL;
  return xht_get(chat->log,packet_get_istr(chat->roster,index*2));
}
-}
