module TeleHash.Utils
  (
   TeleHash
  , Switch(..)
  , PathId(..)
  , SeedInfo(..)
  , Bucket(..)
  , HashDistance
  , Path(..)
  , PathType(..)
  , PathPriority
  , Telex(..)
  , Body(..)
  , Packet(..)
  , To(..)
  , Hash(..)
  , unHash
  , HashName(..)
  , HashContainer(..)
  , HashCrypto(..)
  , Parts(..)
  , Channel(..)
  , Line(..)
  , CSet(..)
  , PublicKey(..)
  , PrivateKey(..)
  , SocketHandle(..)
  , parts2hn
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
import Data.IP
import Data.List
import Data.Maybe
import Data.String.Utils
import Network.BSD
import Network.Socket
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
type HashDistance = Int

-- ---------------------------------------------------------------------

newtype Hash = Hash String
             deriving (Eq,Show,Ord)
unHash :: Hash -> String
unHash (Hash str) = str

-- ---------------------------------------------------------------------

data HashName = HN String
              deriving (Eq,Show,Ord)

data HashContainer = H
  { hHashName :: HashName
  , hChans :: [Channel]
  , hSelf :: Maybe HashCrypto
  , hPaths :: [Path]
  , hIsAlive :: Bool
  , hIsPublic :: Bool
  , hIsSeed :: Bool
  , hAt :: ClockTime
  , hBucket :: HashDistance
  , hChanOut :: Integer -- 2 for normal, 1 only for self
  } deriving Show

-- ---------------------------------------------------------------------

data HashCrypto = HC
  { hcHashName :: HashName
  , hcHexName :: Hash
  , hcParts :: Parts
  , hcCsid :: String
  , hcKey :: String
  , hcPublic :: PublicKey
  , hcPrivate :: Maybe PrivateKey
  } deriving Show

data PublicKey = Public1a ECDSA.PublicKey deriving Show
data PrivateKey = Private1a ECDSA.PrivateKey deriving Show

-- ---------------------------------------------------------------------

data CSet = CS
  { csLoadkey :: String -> Maybe String -> TeleHash (Maybe HashCrypto)

  }



-- ---------------------------------------------------------------------

data PathType = PathType String
              deriving (Show,Eq,Ord)

type PathPriority = Int

data Path = Path
      { pType    :: PathType
      , pIp      :: Maybe IP -- ipv4,ipv6
      , pPort    :: Int    -- ipv4,ipv6
      , pHttp    :: String -- http
      , pRelay   :: Maybe Relay -- relay
      , pId      :: Maybe HashName -- local
      , pLastIn  :: Maybe ClockTime
      , pLastOut :: Maybe ClockTime
      , pPriority :: Maybe PathPriority
      , pIsSeed :: Bool
      } deriving (Show,Eq)

-- ---------------------------------------------------------------------

data PathId = PId Int
            deriving (Ord,Eq,Show)

-- ---------------------------------------------------------------------

data Relay = Relay deriving (Show,Eq)

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

       { swH :: Maybe SocketHandle
       , swSeeds :: [HashName]
       , swLocals :: [String]
       , swLines :: [String]
       , swBridges :: [String]
       , swBridgeLine :: [String]
       , swAll :: Map.Map HashName HashContainer
       , swBuckets :: [Bucket]
       , swCapacity :: [String]
       , swRels :: [String]
       , swRaws :: Map.Map String (String -> Packet -> Channel -> IO ())
       , swPaths :: Map.Map PathId Path
       , swBridgeCache :: [String]
       , swNetworks :: Map.Map PathType (PathId,(PathType -> Packet -> Maybe To -> TeleHash ()))

       , swHashname :: Maybe HashName
       , swId :: Map.Map String String
       , swCs :: Map.Map String (Map.Map String String)
       , swKeys :: Map.Map String (Map.Map String String)

       , swCSets :: Map.Map String CSet
       , swParts :: Parts -- ++AZ++ TODO: this should be a packet


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
       , swWhokey :: Parts -> Either String (Map.Map String String) -> TeleHash (Maybe HashContainer)

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
       , swIsLocalIP :: IP -> Bool
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

    r = BU.toString $ B16.encode bsfinal

-- testParts2hn = parts2hn (sParts $ head initialSeeds)

testParts2hn = parts2hn [ ("1a","o0UL/D6qQ+dcSX7hCoyMjLDYeA6dNScZ+YY/fcX4fyCtsSO2u9L5Lg==")
                        , ("1a_secret","iollyIcHaGeD/JpUNn/7ef1QAzE=")]


-- ---------------------------------------------------------------------

data SocketHandle =
    SocketHandle {slSocket :: Socket
                 --, slAddress :: SockAddr
                 } deriving (Eq,Show)

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

