{-# LANGUAGE OverloadedStrings #-}
module TeleHash.New.Utils
  (
  -- * telehash-c api
    packet_set_str
  , packet_get_str
  , packet_set_int
  , packet_set
  , packet_copy

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
import TeleHash.New.Types
import TeleHash.New.Packet

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

packet_set_str = assert False undefined
packet_get_str = assert False undefined
packet_set_int = assert False undefined

packet_set :: (Aeson.ToJSON a) => TxTelex -> String -> a -> TxTelex
packet_set = assert False undefined

packet_copy :: TxTelex -> TeleHash TxTelex
packet_copy = assert False undefined

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
      _ -> Nothing

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

