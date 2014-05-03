{-# LANGUAGE OverloadedStrings #-}
module Network.TeleHash.Utils
  (
  -- * telehash-c api
    PacketApi(..)

  , packet_set_int
  , packet_copy
  , packet_get_packet
  , packet_from_val
  , packet_body
  , packet_link

  -- * Channels
  , putChan
  , getChan
  , queueChan
  , dequeueChan
  , rmChan
  , getChanFromHn
  , rmChanFromHn

  , getNextUid

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

  -- * Original
  , logT
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
  packet_set :: (Aeson.ToJSON b) => a -> String -> b -> a
  packet_get :: a -> String -> Maybe Aeson.Value
  packet_has_key :: a -> String -> Bool

instance PacketApi TxTelex where
  packet_set_str packet key val
    = packet { tJs = HM.insert (Text.pack key) (toJSON val) (tJs packet) }

  packet_get_str p key
    = case HM.lookup (Text.pack key) (tJs p) of
        Nothing -> Nothing
        Just (Aeson.String s) -> Just (Text.unpack s)
        Just v -> Just (show v)

  packet_set p key val
    = p { tJs = HM.insert (Text.pack key) (toJSON val) (tJs p) }

  packet_get p key = HM.lookup (Text.pack key) (tJs p)

  packet_has_key p key = HM.member (Text.pack key) (tJs p)

instance PacketApi RxTelex where
  packet_set_str packet key val
    = packet { rtJs = HM.insert (Text.pack key) (toJSON val) (rtJs packet) }

  packet_get_str p key
    = case HM.lookup (Text.pack key) (rtJs p) of
        Nothing -> Nothing
        Just (Aeson.String s) -> Just (Text.unpack s)
        Just v -> Just (show v)

  packet_set p key val
    = p { rtJs = HM.insert (Text.pack key) (toJSON val) (rtJs p) }

  packet_get p key = HM.lookup (Text.pack key) (rtJs p)

  packet_has_key p key = HM.member (Text.pack key) (rtJs p)

packet_set_int :: TxTelex -> String -> Int -> TxTelex
packet_set_int p key val
 = p { tJs = HM.insert (Text.pack key) (toJSON val) (tJs p) }


packet_copy :: TxTelex -> TeleHash TxTelex
packet_copy = assert False undefined

packet_get_packet :: RxTelex -> String -> Maybe Aeson.Value
packet_get_packet p key = HM.lookup (Text.pack key) (rtJs p)

packet_from_val :: Aeson.Value -> RxTelex
packet_from_val (Object v) = packet_new_rx {rtJs = v}
packet_from_val x = error $ "packet_from_val expecting Object,got:" ++ show x

packet_body :: TxTelex -> BC.ByteString -> TxTelex
packet_body p bs = p { tPacket = (tPacket p) { paBody = Body bs }}

packet_link :: TxTelex -> TxTelex -> TxTelex
packet_link parent child = r
  where
    r = assert False undefined
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
-- Channels

-- ---------------------------------------------------------------------

queueChan :: TChan -> TeleHash ()
queueChan chan = do
  sw <- get
  put $ sw { swChans = Map.insert (chUid chan) chan (swChans sw)}

-- ---------------------------------------------------------------------

dequeueChan :: TChan -> TeleHash ()
dequeueChan chan = do
  sw <- get
  put $ sw { swChans = Map.delete (chUid chan) (swChans sw)}

-- ---------------------------------------------------------------------

putChan :: TChan -> TeleHash ()
putChan chan = do
  logT $ "putChan:" ++ show (chId chan, chUid chan)
  sw <- get
  put $ sw { swIndexChans = Map.insert (chUid chan) chan (swIndexChans sw)}

-- ---------------------------------------------------------------------

getChan :: Uid -> TeleHash TChan
getChan chanUid = do
  sw <- get
  return $ gfromJust ("getChan:" ++ show chanUid) $ Map.lookup chanUid (swIndexChans sw)

-- ---------------------------------------------------------------------

getChanFromHn :: HashName -> ChannelId -> TeleHash (Maybe TChan)
getChanFromHn hn cid = do
  hc <- getHN hn
  return $ Map.lookup cid (hChans hc)

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
-- Logging

logT :: String -> TeleHash ()
logT str = io (warningM "Controller" str)

-- ---------------------------------------------------------------------
-- Convenience.
--
io :: IO a -> TeleHash a
io = liftIO

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

