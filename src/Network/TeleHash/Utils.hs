{-# LANGUAGE OverloadedStrings #-}
module Network.TeleHash.Utils
  (
  -- * telehash-c api
    PacketApi(..)

  , packet_copy
  , packet_get_packet
  , packet_from_val
  , packet_body
  , packet_link
  , packet_unlink
  , packet_chain
  , packet_cmp
  , packet_linked
  , packet_raw
  , telexToPacket
  , packet_space

  , get_str_from_value

  -- * Channels
  , putChan
  , getChan
  , getChanMaybe
  , queueChan
  , dequeueChan
  , rmChan
  , getChanFromHn
  , rmChanFromHn

  , getNextUid
  , getNextTxid
  , getNextCrid

  , getChat
  , getChatMaybe
  , putChat
  , getChatCurrent
  , putChatCurrent
  , getChatRFromChan
  , getChatR
  , putChatR

  -- * Hashcontainers
  , getHN
  , getHNMaybe
  , putHN
  , withHN
  , withHNM
  , putPath
  , getPath
  , putHexLine
  , getHexLine
  , getHexLineMaybe

  -- * Other
  , getCrypto

  -- * Info about the switch
  , showAllChans
  , showChan
  , showChanShort

  -- * Utility
  , logT
  , logP
  , logR
  , mainLoggerName
  , lineLoggerName
  , io

  , ghead
  , glast
  , gtail
  , gfromJust

  , getTxTelexChannelId
  , getRxTelexChannelId
  , getTxTelexType
  , getRxTelexType

  , parts2hn
  , showSwitch
  , randomHEX
  , randomWord32
  , asHexString
  , word32AsHexString
  , parseJs
  , parseJsVal
  , b16Tobs
  , b16ToCbs
  , showJson

  ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Crypto.Random
import Crypto.Number.Serialize
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
import Network.TeleHash.Paths
import Network.TeleHash.Types
import Network.TeleHash.Packet

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
import qualified System.Random as R

-- ---------------------------------------------------------------------
-- telehash-c api

-- TODO: strip this down to a simple getJS/putJS interface, then
-- harvest commonality
class PacketApi a where
  packet_set_str :: a -> String -> String -> a
  packet_get_str :: a -> String -> Maybe String
  packet_get_str_always :: a -> String -> String

  packet_set_int :: a -> String -> Int -> a
  packet_get_int :: a -> String -> Maybe Int

  packet_set :: (Aeson.ToJSON b) => a -> String -> b -> a
  packet_get :: a -> String -> Maybe Aeson.Value

  packet_has_key :: a -> String -> Bool

  packet_append :: a -> BC.ByteString -> a

instance PacketApi TxTelex where
  packet_set_str packet key val
    = packet { tJs = HM.insert (Text.pack key) (toJSON val) (tJs packet) }

  packet_get_str p key
    = case HM.lookup (Text.pack key) (tJs p) of
        Nothing -> Nothing
        Just (Aeson.String s) -> Just (Text.unpack s)
        Just v -> Just (show v)

  packet_get_str_always p key = fromMaybe "" $ packet_get_str p key


  packet_set_int p key val
   = p { tJs = HM.insert (Text.pack key) (toJSON val) (tJs p) }

  packet_get_int p key =
    case HM.lookup (Text.pack key) (tJs p) of
      Nothing -> Nothing
      Just (Aeson.Number v) -> Just (round v)



  packet_set p key val
    = p { tJs = HM.insert (Text.pack key) (toJSON val) (tJs p) }

  packet_get p key = HM.lookup (Text.pack key) (tJs p)

  packet_has_key p key = HM.member (Text.pack key) (tJs p)


  packet_append p chunk = r
    where
      body = unBody $ paBody (tPacket p)
      packetNew = (tPacket p) { paBody = Body (BC.append body chunk) }
      r = p { tPacket = packetNew }


instance PacketApi RxTelex where
  packet_set_str packet key val
    = packet { rtJs = HM.insert (Text.pack key) (toJSON val) (rtJs packet) }

  packet_get_str p key
    = case HM.lookup (Text.pack key) (rtJs p) of
        Nothing -> Nothing
        Just (Aeson.String s) -> Just (Text.unpack s)
        Just v -> Just (show v)

  packet_get_str_always p key = fromMaybe "" $ packet_get_str p key


  packet_set_int p key val
   = p { rtJs = HM.insert (Text.pack key) (toJSON val) (rtJs p) }

  packet_get_int p key =
    case HM.lookup (Text.pack key) (rtJs p) of
      Nothing -> Nothing
      Just (Aeson.Number v) -> Just (round v)



  packet_set p key val
    = p { rtJs = HM.insert (Text.pack key) (toJSON val) (rtJs p) }

  packet_get p key = HM.lookup (Text.pack key) (rtJs p)

  packet_has_key p key = HM.member (Text.pack key) (rtJs p)

  packet_append p chunk = r
    where
      body = unBody $ paBody (rtPacket p)
      packetNew = (rtPacket p) { paBody = Body (BC.append body chunk) }
      r = p { rtPacket = packetNew }

packet_copy :: TxTelex -> TxTelex
packet_copy p = p
{-
packet_t packet_copy(packet_t p)
{
  packet_t np;
  np = packet_parse(packet_raw(p), packet_len(p));
  np->to = p->to;
  np->from = p->from;
  np->out = p->out;
  return np;
}
-}

packet_get_packet :: RxTelex -> String -> Maybe Aeson.Value
packet_get_packet p key = HM.lookup (Text.pack key) (rtJs p)

packet_from_val :: Aeson.Value -> RxTelex
packet_from_val (Object v) = packet_new_rx {rtJs = v}
packet_from_val x = error $ "packet_from_val expecting Object,got:" ++ show x

packet_body :: TxTelex -> BC.ByteString -> TxTelex
packet_body p bs = p { tPacket = (tPacket p) { paBody = Body bs }}

-- ---------------------------------------------------------------------

packet_link :: Maybe TxTelex -> TxTelex -> TxTelex
packet_link mparent child = r
  where
    parent = case mparent of
      Nothing -> packet_new (tTo child)
      Just p -> p
    child2 = if tChain child == Just parent
               then child { tChain = Nothing }
               else child
    parent2 = parent { tChain = Just child2 }
    r = parent2

{-
packet_t packet_link(packet_t parent, packet_t child)
{
  if(!parent) parent = packet_new();
  if(!parent) return NULL;
  if(parent->chain) packet_free(parent->chain);
  parent->chain = child;
  if(child && child->chain == parent) child->chain = NULL;
  return parent;
}
-}

-- ---------------------------------------------------------------------

packet_unlink :: TxTelex -> Maybe TxTelex
packet_unlink parent = r
  where
    child = tChain parent
    r = child
    -- r = error "packet_unlink"

{-
packet_t packet_unlink(packet_t parent)
{
  packet_t child;
  if(!parent) return NULL;
  child = parent->chain;
  parent->chain = NULL;
  return child;
}
-}
-- ---------------------------------------------------------------------

packet_chain :: TxTelex -> TxTelex
packet_chain p = r
  where
    np = packet_new (tTo p)
    r = np {tChain = Just p
            , tTo = tTo p
            , tOut = tOut p
            }
    -- copy in meta pointers for convenience
{-
packet_t packet_chain(packet_t p)
{
  packet_t np = packet_new();
  if(!np) return NULL;
  np->chain = p;
  // copy in meta-pointers for convenience
  np->to = p->to;
  np->from = p->from;
  np->out = p->out;
  return np;
}
-}

-- ---------------------------------------------------------------------

packet_cmp :: TxTelex -> TxTelex -> Bool
packet_cmp a b = assert False undefined
{-
int packet_cmp(packet_t a, packet_t b)
{
  int i = 0;
  char *str;
  if(!a || !b) return -1;
  if(a->body_len != b->body_len) return -1;
  if(packet_keys(a) != packet_keys(b)) return -1;

  packet_sort(a);
  packet_sort(b);
  while((str = packet_get_istr(a,i)))
  {
    if(strcmp(str,packet_get_istr(b,i)) != 0) return -1;
    i++;
  }

  return memcmp(a->body,b->body,a->body_len);
}
-}

-- ---------------------------------------------------------------------

packet_linked :: TxTelex -> Maybe TxTelex
packet_linked parent = tChain parent

{-
packet_t packet_linked(packet_t parent)
{
  if(!parent) return NULL;
  return parent->chain;
}
-}

-- ---------------------------------------------------------------------

packet_raw :: TxTelex -> TeleHash LinePacket
packet_raw tx = do
  p <- telexToPacket tx
  return $ toLinePacket (tPacket p)

-- ---------------------------------------------------------------------

telexToPacket :: TxTelex -> TeleHash TxTelex
telexToPacket telex = do
  case (HM.toList $ tJs telex) of
    [] -> return $ telex
    _js -> do
      -- logT $ "telexToPacket: encoded js=" ++ (BC.unpack $ lbsTocbs $ encode (tJs telex))
      let packet = (tPacket telex) { paHead = HeadJson (lbsTocbs $ encode (tJs telex)) }
      return $ telex { tPacket = packet}

-- ---------------------------------------------------------------------

packet_space :: TxTelex -> TeleHash Int
packet_space p = do
  raw <- packet_raw p
  let len = BC.length $ unLP raw
  if len > 1440
    then return 0
    else return (1440 - len)

{-
unsigned short packet_space(packet_t p)
{
  unsigned short len;
  if(!p) return 0;
  len = 2+p->json_len+p->body_len;
  if(len > 1440) return 0;
  return 1440-len;
}

-}

-- ---------------------------------------------------------------------


{-
void packet_append(packet_t p, unsigned char *chunk, unsigned short len)
{
  void *ptr;
  if(!p || !chunk || !len) return;
  if(!(ptr = realloc(p->raw,2+len+p->body_len+p->json_len))) return;
  p->raw = (unsigned char *)ptr;
  p->json = p->raw+2;
  p->body = p->raw+(2+p->json_len);
  memcpy(p->body+p->body_len,chunk,len);
  p->body_len += len;
}
-}

-- ---------------------------------------------------------------------

get_str_from_value :: String -> Aeson.Value -> Maybe String
get_str_from_value k (Aeson.Object hm) =
  case HM.lookup (Text.pack k) hm of
    Nothing -> Nothing
    Just (Aeson.String str) -> Just (Text.unpack str)
    _ -> Nothing
get_str_from_value _ _ = Nothing

-- ---------------------------------------------------------------------
-- Channels

-- ---------------------------------------------------------------------

queueChan :: TChan -> TeleHash ()
queueChan chanIn = do
  sw <- get
  put $ sw { swChans = Set.insert (chUid chanIn) (swChans sw)}

-- ---------------------------------------------------------------------

-- |remove channel id from switch processing queue
dequeueChan :: Uid -> TeleHash ()
dequeueChan chanUid = do
  sw <- get
  put $ sw { swChans = Set.delete chanUid (swChans sw)}

-- ---------------------------------------------------------------------

putChan :: TChan -> TeleHash ()
putChan chan = do
  -- logT $ "putChan:" ++ show (chId chan, chUid chan, chSeq chan)
  sw <- get
  put $ sw { swIndexChans = Map.insert (chUid chan) chan (swIndexChans sw)}

-- ---------------------------------------------------------------------

getChan :: Uid -> TeleHash TChan
getChan chanUid = do
  sw <- get
  return $ gfromJust ("getChan:" ++ show chanUid) $ Map.lookup chanUid (swIndexChans sw)

getChanMaybe :: Uid -> TeleHash (Maybe TChan)
getChanMaybe chanUid = do
  sw <- get
  return $ Map.lookup chanUid (swIndexChans sw)

-- ---------------------------------------------------------------------

getChanFromHn :: HashName -> ChannelId -> TeleHash (Maybe TChan)
getChanFromHn hn cid = do
  hc <- getHN hn
  case Map.lookup cid (hChans hc) of
    Nothing -> return Nothing
    Just uid -> do
      chan <- getChan uid
      return $ Just chan

-- ---------------------------------------------------------------------

rmChan :: Uid -> TeleHash ()
rmChan uid = do
  sw <- get
  put $ sw { swIndexChans = Map.delete uid (swIndexChans sw)}

-- ---------------------------------------------------------------------

rmChanFromHn :: HashName -> ChannelId -> TeleHash ()
rmChanFromHn hn cid = do
  withHN hn $ \hc ->
    hc { hChans = Map.delete cid (hChans hc) }

-- ---------------------------------------------------------------------

getNextUid :: TeleHash Uid
getNextUid = do
  sw <- get
  let uid = 1 + swUid sw
  put sw { swUid = uid }
  return uid


-- ---------------------------------------------------------------------

getNextTxid :: TeleHash TxId
getNextTxid = do
  sw <- get
  let txid = 1 + swTxid sw
  put sw { swTxid = txid }
  return txid

-- ---------------------------------------------------------------------

getNextCrid :: TeleHash ChatRId
getNextCrid = do
  sw <- get

  let (CR cridOld) = swCrid sw
      crid = CR (1 + cridOld)
  put sw { swCrid = crid }
  return crid

-- ---------------------------------------------------------------------

putChat :: Chat -> TeleHash ()
putChat chat = do
  logT $ "putChat: " ++ show (chatIdToString $ ecId chat,ecConn chat,ecRoster chat)
  sw <- get
  put $ sw {swIndexChat = Map.insert (ecId chat) chat (swIndexChat sw) }


getChat :: ChatId -> TeleHash Chat
getChat cid = do
  -- logT $ "getChat: " ++ (chatIdToString cid)
  sw <- get
  return $ gfromJust ("getChat " ++ show (chatIdToString cid)) $ Map.lookup cid (swIndexChat sw)

getChatMaybe :: ChatId -> TeleHash (Maybe Chat)
getChatMaybe cid = do
  sw <- get
  return $ Map.lookup cid (swIndexChat sw)

putChatCurrent :: ChatId -> TeleHash ()
putChatCurrent cid = do
  sw <- get
  put $ sw { swCurrentChat = Just cid }

getChatCurrent :: TeleHash ChatId
getChatCurrent = do
  sw <- get
  return $ gfromJust "getChatCurrent" $ swCurrentChat sw

-- ---------------------------------------------------------------------

getChatRFromChan :: Uid -> TeleHash (Maybe ChatR)
getChatRFromChan uid = do
  c <- getChan uid
  case chArg c of
    CArgChatR rid -> do
      cr <- getChatR rid
      return (Just cr)
    _ -> return Nothing

getChatR :: ChatRId -> TeleHash ChatR
getChatR crid = do
  sw <- get
  return $ gfromJust ("getChatR:" ++ show crid) $ Map.lookup crid (swIndexChatR sw)

putChatR :: ChatR -> TeleHash ()
putChatR r = do
  logT $ "putChatR:r=" ++ show r
  sw <- get
  put $ sw {swIndexChatR = Map.insert (ecrId r) r (swIndexChatR sw)}

-- ---------------------------------------------------------------------

getHN :: HashName -> TeleHash HashContainer
getHN hn = do
  sw <- get
  return $ gfromJust ("getHN " ++ (show hn)) (Map.lookup hn (swIndex sw))

-- ---------------------------------------------------------------------

getHNMaybe :: HashName -> TeleHash (Maybe HashContainer)
getHNMaybe hn = do
  sw <- get
  return $ (Map.lookup hn (swIndex sw))


-- ---------------------------------------------------------------------

putHN :: HashContainer -> TeleHash ()
putHN hc = do
  sw <- get
  put $ sw { swIndex = Map.insert (hHashName hc) hc (swIndex sw) }

-- ---------------------------------------------------------------------

withHN :: HashName -> (HashContainer -> HashContainer) -> TeleHash ()
withHN hn fn = do
  hc <- getHN hn
  putHN (fn hc)

-- ---------------------------------------------------------------------

withHNM :: HashName -> (HashContainer -> TeleHash HashContainer) -> TeleHash ()
withHNM hn fn = do
  hc <- getHN hn
  hc' <- fn hc
  putHN hc'

-- ---------------------------------------------------------------------

putPath :: HashName -> Path -> TeleHash ()
putPath hn path = do
  hc <- getHN hn
  putHN $ hc { hPaths = Map.insert (pJson path) path (hPaths hc)}

-- ---------------------------------------------------------------------

getPath :: HashName -> PathJson -> TeleHash Path
getPath hn pj = do
  hc <- getHN hn
  return $ gfromJust "getPath" $ Map.lookup pj (hPaths hc)


-- ---------------------------------------------------------------------

putHexLine :: String -> HashName -> TeleHash ()
putHexLine lineHex hn = do
  sw <- get
  put $ sw { swIndexLines = Map.insert lineHex hn (swIndexLines sw)}

-- ---------------------------------------------------------------------

getHexLine :: String -> TeleHash HashName
getHexLine lineHex = do
  sw <- get
  return $ gfromJust ("getLine " ++ lineHex)
         $ Map.lookup lineHex (swIndexLines sw)

-- ---------------------------------------------------------------------

getHexLineMaybe :: String -> TeleHash (Maybe HashName)
getHexLineMaybe lineHex = do
  sw <- get
  return $ Map.lookup lineHex (swIndexLines sw)

-- ---------------------------------------------------------------------

getCrypto :: String -> TeleHash (Maybe Crypto)
getCrypto csid = do
  sw <- get
  return $ Map.lookup csid (swIndexCrypto sw)

-- ---------------------------------------------------------------------

showAllChans :: TeleHash String
showAllChans = do
  sw <- get
  r <- forM (Map.elems $ swIndexChans sw) $ \c -> do
    return $ showChan c
  return $ unlines r

-- ---------------------------------------------------------------------

showChan :: TChan -> String
showChan c = "(chan:" ++ show (chUid c,chId c,chTo c,chReliable c,chState c,chType c) ++ ")"

showChanShort :: TChan -> String
showChanShort c = "(chan:" ++ show (chUid c,chId c,chReliable c,chState c,chType c) ++ ")"

-- ---------------------------------------------------------------------
-- Logging

{-
Kinds of logging

1. Line packets immediateley before/after crypto

2. Debug messages

-}

mainLoggerName :: String
mainLoggerName = "Network.TeleHash.Main"

-- |Debug log stuff
logT :: String -> TeleHash ()
logT str = io (debugM mainLoggerName str)

-- |normal status reports
logR :: String -> TeleHash ()
logR str = io (noticeM mainLoggerName str)

--------

lineLoggerName :: String
lineLoggerName = "Network.TeleHash.Line"

-- |line traffic
logP :: String -> TeleHash ()
logP str = io (noticeM lineLoggerName str)



-- ---------------------------------------------------------------------
-- Convenience.
--
io :: IO a -> TeleHash a
io = Control.Monad.State.liftIO

-- ---------------------------------------------------------------------

ghead :: String -> [a] -> a
ghead  info []    = error $ "ghead "++info++" []"
ghead _info (h:_) = h

glast :: String -> [a] -> a
glast  info []    = error $ "glast " ++ info ++ " []"
glast _info h     = last h

gtail :: String -> [a] -> [a]
gtail  info []   = error $ "gtail " ++ info ++ " []"
gtail _info h    = tail h

gfromJust :: [Char] -> Maybe a -> a
gfromJust _info (Just h) = h
gfromJust  info Nothing = error $ "gfromJust " ++ info ++ " Nothing"

-- ---------------------------------------------------------------------

getTxTelexChannelId :: TxTelex -> Maybe ChannelId
getTxTelexChannelId packet
  = case (HM.lookup "c" (tJs packet)) of
      Nothing -> Nothing
      Just (Number n) -> Just $ CID (round n)
      _ -> Nothing

-- ---------------------------------------------------------------------

getRxTelexChannelId :: RxTelex -> Maybe ChannelId
getRxTelexChannelId packet
  = case (HM.lookup "c" (rtJs packet)) of
      Nothing -> Nothing
      Just (Number n) -> Just $ CID (round n)
      _ -> Nothing

-- ---------------------------------------------------------------------

getTxTelexType :: TxTelex -> Maybe String
getTxTelexType packet
  = case (HM.lookup "type" (tJs packet)) of
      Nothing -> Nothing
      Just (String typ) -> Just (Text.unpack typ)
      _ -> Nothing

-- ---------------------------------------------------------------------

getRxTelexType :: RxTelex -> Maybe String
getRxTelexType packet
  = case (HM.lookup "type" (rtJs packet)) of
      Nothing -> Nothing
      Just (String typ) -> Just (Text.unpack typ)
      vv -> error $ "getRxTelexType:strange value" ++ show vv
      -- _ -> Nothing

-- ---------------------------------------------------------------------

{-
function parts2hn(parts)
{
  var rollup = new Buffer(0);
  Object.keys(parts).sort().forEach(function(id){
    rollup = crypto.createHash("sha256").update(Buffer.concat([rollup,new Buffer(id)])).digest();
    rollup = crypto.createHash("sha256").update(Buffer.concat([rollup,new Buffer(parts[id])])).digest();
  });
  return rollup.toString("hex");
}
-}

parts2hn :: Parts -> HashName
parts2hn parts = HN r
  where
    sp = sort parts
    iCtx = SHA256.init
    vals = concatMap (\(a,b) -> [BC.pack a,BC.pack b]) sp
    ctx = SHA256.updates iCtx vals
    -- bsfinal = SHA256.finalize ctx

    bsfinal = foldl' (\acc cur -> SHA256.hash (BC.append acc cur)) BC.empty vals

    r = BC.unpack $ B16.encode bsfinal

-- testParts2hn = parts2hn (sParts $ head initialSeeds)

testParts2hn = parts2hn [ ("1a","o0UL/D6qQ+dcSX7hCoyMjLDYeA6dNScZ+YY/fcX4fyCtsSO2u9L5Lg==")
                        , ("1a_secret","iollyIcHaGeD/JpUNn/7ef1QAzE=")]


-- ---------------------------------------------------------------------

showSwitch :: Switch -> String
showSwitch sw =
  ("switch:"++ show (swId sw)
{-
  ++ "\nlinescount=" ++ show (Map.size $ swLines sw)
  ++ "\n  " ++ show (swLines sw)
  ++ "\nhashcount:" ++  show (Map.size $ swAll sw)
  ++ "\n  " ++ (intercalate "\n  " (map show $ Map.keys (swAll sw)))
  ++ "\nbucketCount:" ++ show (Map.size (swBuckets sw))
  ++ "\nbuckets:" ++ show (swBuckets sw)
-}
  )

-- ---------------------------------------------------------------------

randomHEX :: Int -> TeleHash String
randomHEX len = do
  sw <- get
  let (bytes,newRNG) = cprgGenerate len (swRNG sw)
  put $ sw {swRNG = newRNG}
  return $ BC.unpack $ B16.encode bytes

-- ---------------------------------------------------------------------

randomWord32 :: TeleHash Word32
randomWord32 = do
  sw <- get
  let (bytes,newRNG) = cprgGenerate 4 (swRNG sw)
  put $ sw {swRNG = newRNG}
  return $ fromIntegral $ os2ip bytes

-- ---------------------------------------------------------------------

word32AsHexString :: Word32 -> String
word32AsHexString w32 = r
  where
    (Just v) = i2ospOf 4 (fromIntegral w32)
    r = asHexString v

-- ---------------------------------------------------------------------

asHexString :: BC.ByteString -> String
asHexString bs = BC.unpack $ B16.encode bs

-- ---------------------------------------------------------------------

parseJs :: (FromJSON a) => (HM.HashMap Text.Text Aeson.Value) -> Maybe a
parseJs v = r
  where
    mp = fromJSON (Object v)
    r = case mp of
        Error _err1 -> Nothing
        Success val -> Just val

parseJsVal :: (FromJSON a) => (Aeson.Value) -> Maybe a
parseJsVal v = r
  where
    mp = fromJSON v
    r = case mp of
        Error _err1 -> Nothing
        Success val -> Just val

-- ---------------------------------------------------------------------

b16Tobs :: BC.ByteString -> BC.ByteString
b16Tobs str = r
  where
   (r,_) = B16.decode str

b16ToCbs :: String -> BC.ByteString
b16ToCbs str = r
  where (r,_) = B16.decode $ BC.pack str

-- ---------------------------------------------------------------------

showJson :: Aeson.ToJSON a => a -> String
showJson j = BC.unpack $ lbsTocbs $ encode $ j

-- ---------------------------------------------------------------------

{-
seeds=Just 
(Object fromList 
 [("ce9d2cfccf34345b1c1a1c5b6c72cb0cf625ec88cdc64b54921303b26a655949"
    ,Object fromList 
      [("admin",String "http://github.com/quartzjer"),
       ("keys",Object fromList 
          [("3a",String "azQ23XvFzj3238HlcUNsnIntl5VJY7ABMSQZWB6SFgo="),
           ("2a",String "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA6mvKCqjGj7PI2o+NXLRdgwDXx98271HN01ut873FrbJ4kkk3OmA//TpYTRKaE6xmeetXZnocci4q6X09TbfKpm2eNK0d898vWiYpGiRvQuy/5nUGM2bge3CPOS3wQZWv5ZSvpRkhGufekzCg5p6WpdUG0u9D382E9LzdLidFnzHvIdfp0eOc2EMcX7/JSj5w7BbwsXfZNaWpOkUAQEYfPi/qF/teo0y8cTh70JVufCRDx+2/FtA/c8+JpjtgeCZoFO3bYuKjCQiYmm4Zqcu1A6DYttCPkSKPXjirn9pdZFZBRH7IS7Mj5AJo2/L9nFYyLAE5xwMpBCE2rCY6wyzs7wIDAQAB"),
           ("1a",String "vRQvjqB6PM7QevqIW2YF3hY/AgDlhP7d0YDo1H6dZJAcYxbcsS/1Qw==")])
      ,("parts",Object fromList 
          [("3a",String "61b979399a285ec8a7159ea75f2953090612f26fe8ec80b4bdd3d746c7cba1f8"),("2a",String "df99cf38a79eb730b7b5c583faa4bcb21ccb044b5548df27837e608a3da8c57a"),("1a",String "4dd170c2523653dfaca8d2eca6c10ef4f703b3a95f4b77f57b81476d037e40b1")])

       ,("paths",Array (fromList 
            [Object fromList 
               [("type",String "http"),("http",String "http://208.68.164.253:42424")]
            ,Object fromList 
               [("ip",String "208.68.164.253"),("port",Number 42424.0),("type",String "ipv4")]
            ,Object fromList [("ip",String "2605:da00:5222:5269:230:48ff:fe35:6572"),("port",Number 42424.0),("type",String "ipv6")]]))])])
-}

tp = do
 let
  ps =  "{\"parts\": {\"2a\": \"beb07e8864786e1d3d70b0f537e96fb719ca2bbb4a2a3791ca45e215e2f67c9a\",\"1a\": \"6c0da502755941a463454e9d478b16bbe4738e67\"}}"
 vv <- Aeson.decode ps :: Maybe Parts
 return vv



-- ---------------------------------------------------------------------

