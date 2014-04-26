module TeleHash.New.Switch
   (
   -- * Hashcontainers
     getHN
   , putHN
   , withHN
   , withHNM

   -- * Channels
   , putChan
   , rmChan
   , getChanFromHn
   , rmChanFromHn

   , getNextUid
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
import TeleHash.New.Hn
import TeleHash.New.Types
import TeleHash.New.Utils

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

getHN :: HashName -> TeleHash HashContainer
getHN hn = do
  sw <- get
  return $ gfromJust ("getHN " ++ (show hn)) (Map.lookup hn (swIndex sw))


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

putChan :: TChan -> TeleHash ()
putChan chan = do
  sw <- get
  put $ sw { swIndexChans = Map.insert (chUid chan) chan (swIndexChans sw)}

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

