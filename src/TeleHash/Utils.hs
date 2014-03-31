module TeleHash.Utils
  (
   TeleHash
  , Switch(..)
  , PathId(..)
  , SeedInfo(..)
  , Path(..)
  , PathType(..)
  , Telex(..)
  , Body(..)
  , Packet(..)
  , To(..)
  , HashName(..)
  , HashContainer(..)
  , Parts(..)
  , Channel(..)
  , Line(..)
  , CSet(..)
  , PublicKey(..)
  , PrivateKey(..)
  , io
  , logT
  ) where


import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Crypto.Random
import Data.Aeson
import Data.Bits
import Data.Char
import Data.List
import Data.Maybe
import Data.String.Utils
import Network.BSD
import qualified Network.Socket as NS
import Prelude hiding (id, (.), head, either, catch)
import System.IO
import System.Log.Handler.Simple
import System.Log.Logger
import System.Time


import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Network.Socket.ByteString as SB
import qualified System.Random as R
import qualified Crypto.Types.PubKey.ECDSA as ECDSA
-- ---------------------------------------------------------------------

-- The 'TeleHash' monad, a wrapper over IO, carrying the switch's immutable state.
--
type TeleHash = StateT Switch IO

-- ---------------------------------------------------------------------

data Telex = Telex
data Body = Body

-- ---------------------------------------------------------------------

data Packet = Packet

-- ---------------------------------------------------------------------

type Channel = String

-- ---------------------------------------------------------------------

data HashName = HN String
              deriving (Eq,Show)

data HashContainer = HC
  { hcHashName :: HashName
  , hcParts :: Parts
  , hcCsid :: String
  , hcKey :: String
  , hcPublic :: PublicKey
  , hcPrivate :: Maybe PrivateKey
  }

data PublicKey = Public1a ECDSA.PublicKey
data PrivateKey = Private1a ECDSA.PrivateKey

-- ---------------------------------------------------------------------

data CSet = CS
  { csLoadkey :: HashContainer -> String -> Maybe String -> TeleHash (HashContainer,Bool)

  }



-- ---------------------------------------------------------------------

data PathType = PathType String
              deriving (Show,Eq,Ord)

data Path = Path
      { pType    :: PathType
      , pIp      :: String
      , pPort    :: Int
      , pHttp    :: String
      , pLastIn  :: Maybe ClockTime
      , pLastOut :: Maybe ClockTime
      } deriving (Show,Eq)

-- ---------------------------------------------------------------------

data PathId = PId Int
            deriving (Ord,Eq,Show)

-- ---------------------------------------------------------------------

data To = To { pathOut :: PathType -> PathType
             }

-- ---------------------------------------------------------------------

type Bucket = [Line]
data Line = Line { lineAge :: ClockTime
                 , lineSeed :: String
                 , lineAddress :: String
                 , lineLinked :: Maybe Path
                 , lineAlive :: Bool
                 , lineSentat :: Maybe ClockTime
                 } deriving Show

-- ---------------------------------------------------------------------

data Seed = Seed { sAlive :: Bool
                 , sLink :: TeleHash () ->TeleHash ()
                 }

-- ---------------------------------------------------------------------

type Parts = [(String,String)] -- [(csid,key)]

data SeedInfo = SI
  { sId :: String
  , sAdmin :: String
  , sPaths :: [Path]
  , sParts :: Parts -- crypto ids?
  , sKeys :: [(String,String)] -- crypto scheme name, crypto key
  , sIsBridge :: Bool
  } deriving Show


-- ---------------------------------------------------------------------

data Switch = Switch
       { swSeeds :: [Seed]
       , swLocals :: [String]
       , swLines :: [String]
       , swBridges :: [String]
       , swBridgeLine :: [String]
       , swAll :: [String]
       , swBuckets :: [Bucket]
       , swCapacity :: [String]
       , swRels :: [String]
       , swRaws :: Map.Map String (String -> Packet -> Channel -> IO ())
       , swPaths :: Map.Map PathId Path
       , swBridgeCache :: [String]
       , swNetworks :: Map.Map PathType (PathId,(PathType -> Packet -> Maybe To -> TeleHash ()))

       , swId :: Map.Map String String
       , swCs :: Map.Map String String
       , swKeys :: Map.Map String String

       , swCSets :: Map.Map String CSet
       , swParts :: Parts

       , swLoad :: String -> Bool -- load function
       , swMake :: () -> () -> IO ()

       , swNat :: Bool
       , swSeed :: Bool
       , swLanToken :: Maybe String

       -- udp socket stuff
       , swPcounter :: Int
       , swReceive :: Packet -> Path -> IO ()

       -- outgoing packets to the network
       , swDeliver :: String -> () -> ()
       , swSend    :: PathType -> Packet -> Maybe To -> TeleHash ()
       , swPathSet :: Path -> IO ()

       -- need some seeds to connect to, addSeed({ip:"1.2.3.4", port:5678, public:"PEM"})
       , swAddSeed :: SeedInfo -> TeleHash ()

       --  map a hashname to an object, whois(hashname)
       , swWhois :: HashName -> TeleHash (Maybe HashContainer)
       , swWhokey :: Parts -> String -> Map.Map String String -> TeleHash (Maybe HashName)

       , swStart :: String -> String -> String -> () -> IO ()
       , swOnline :: TeleHash () -> TeleHash ()
       , swIsOnline :: Bool
       , swListen :: String -> () -> IO ()

       -- advanced usage only
       , swRaw :: String -> () -> IO ()

       -- primarily internal, to seek/connect to a hashname
       , swSeek :: String -> () -> IO ()
       , swBridge :: PathType -> Packet -> Maybe To -> TeleHash ()

       -- for modules
       , swPencode :: Telex -> Body -> Packet
       , swPdecode :: Packet -> (Telex,Body)
       , swIsLocalIP :: String -> Bool
       , swRandomHEX :: Int -> TeleHash String
       , swUriparse :: String -> String
       , swIsHashname :: String -> String
       , swWraps :: IO ()
       , swWaits :: [String]
       , swWaiting :: Maybe (TeleHash ())
       , swWait :: Bool -> IO ()


       -- crypto
       , swRNG :: SystemRNG

           -- , swCountOnline :: Int
           -- , swCountTx :: Int
           -- , swCountRx :: Int
           }

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

