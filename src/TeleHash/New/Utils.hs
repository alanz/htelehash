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
