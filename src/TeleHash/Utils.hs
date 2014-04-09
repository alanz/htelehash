{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , Hash(..)
  , unHash
  , HashName(..)
  , unHN
  , HashContainer(..)
  , HashCrypto(..)
  , Parts(..)
  , Channel(..)
  , ChannelId(..)
  , Line(..)
  , CSet(..)
  , PublicKey(..)
  , PrivateKey(..)
  , Id(..)
  , SocketHandle(..)
  , CallBack
  , nullCb
  , parts2hn
  , io
  , logT

  , ghead
  , glast
  , gtail
  , gfromJust

  , putHN
  , getHN
  ) where


import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Crypto.Random
import TeleHash.Packet
import Data.Aeson
import Data.Bits
import Data.Char
import Data.IP
import Data.List
import Data.Maybe
import Data.String.Utils
import Data.Word
import Network.BSD
import Network.Socket
import qualified Network.Socket as NS
import Prelude hiding (id, (.), head, either, catch)
import System.IO
import System.Log.Handler.Simple
import System.Log.Logger
import System.Time


import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.PubKey.DH as DH
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
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

newtype Hash = Hash String
             deriving (Eq,Show,Ord)
unHash :: Hash -> String
unHash (Hash str) = str

-- ---------------------------------------------------------------------

data HashName = HN String
              deriving (Eq,Show,Ord)
unHN :: HashName -> String
unHN (HN s) = s

data HashContainer = H
  { hHashName   :: HashName
  , hChans      :: Map.Map ChannelId Channel
  , hSelf       :: Maybe HashCrypto
  , hPaths      :: [Path]
  , hTo         :: Maybe Path
  , hIsAlive    :: Bool
  , hIsPublic   :: Bool
  , hIsSeed     :: Bool
  , hAt         :: ClockTime
  , hBucket     :: HashDistance
  , hChanOut    :: ChannelId
  , hLineOut    :: String -- randomHEX 16 output. Make it a type
  , hLineAt     :: Maybe ClockTime
  , hLineIn     :: Maybe BC.ByteString
  , hSendSeek   :: Maybe ClockTime
  , hVias       :: Map.Map HashName String
  , hLastPacket :: Maybe Telex
  , hParts      :: Maybe Parts
  , hOpened     :: Maybe LinePacket
  , hCsid       :: Maybe String

  , hLineIV :: Word32 -- crypto 1a IV value
  , hEncKey :: Maybe BC.ByteString
  , hDecKey :: Maybe BC.ByteString
  , hEcc    :: Maybe (PublicKey,PrivateKey) -- our DH ECC key for
                                            -- communicating with this remote
  } deriving Show

-- ---------------------------------------------------------------------
{-
typedef struct crypt_struct
{
  char csidHex[3], *part;
  int isprivate, lined, keylen;
  unsigned long atOut, atIn;
  unsigned char lineOut[16], lineIn[16], lineHex[33];
  unsigned char *key, csid;
  void *cs; // for CS usage
} *crypt_t;

-}
data HashCrypto = HC
  { hcHashName :: HashName
  , hcHexName  :: Hash
  , hcParts    :: Parts
  , hcCsid     :: String
  , hcKey      :: String
  , hcPublic   :: PublicKey
  , hcPrivate  :: Maybe PrivateKey
  -- , hcEccKeys  :: Maybe (DH.PublicNumber,DH.PrivateNumber)
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
      , pRelay   :: Maybe Channel -- relay
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

-- a Telex gets packed to/from a Packet
data Telex = Telex { tId   :: Maybe HashContainer
                   , tType :: Maybe String
                   , tPath :: Maybe Path
                   , tTo   :: Maybe Path -- Do we need both of these? Need to clarify the type of this one first
                   , tJson :: Map.Map String String
                   , tPacket :: Maybe Packet

                   -- TODO: break Inner out into its own type
                   -- openize stuff, used in 'inner'
                   , tAt   :: Maybe ClockTime
                   , tToHash  :: Maybe HashName
                   , tFrom :: Maybe Parts
                   , tLine :: Maybe String -- lineOut
                   } deriving Show

-- ---------------------------------------------------------------------

data Channel = Chan
  { chType     :: String
  , chCallBack :: CallBack
  , chId       :: ChannelId
  , chHashName :: HashName -- for convenience
  , chLast     :: Maybe Path
  , chSentAt   :: Maybe ClockTime
  , chEnded    :: Bool
  } deriving (Show,Eq)

instance Show (CallBack) where
  show _ = "CallBack"

instance Eq CallBack where
  _ == _ = True

type HashDistance = Int

-- |channel id is a positive number from 1 to 4,294,967,295 (UINT32)
data ChannelId = CID Int deriving (Eq,Show,Ord)

instance Num ChannelId where
  (CID a) + (CID b) = CID (a + b)
  (CID _) * (CID _) = error "cannot multiply ChannelIds"
  (CID a) - (CID b) = CID (a - b)
  abs (CID a) = CID (abs a)
  signum (CID a) = CID (signum a)
  fromInteger i = CID (fromIntegral i)

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

{-
data Seed = Seed { sAlive :: Bool
                 , sLink :: CallBack ->TeleHash ()
                 }
-}

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

data Id = Id { id1a :: String
             , id1a_secret :: String
             } deriving Show


instance FromJSON Id where
     parseJSON (Object v) = Id <$>
                            v .: "1a" <*>
                            v .: "1a_secret"
     -- A non-Object value is of the wrong type, so fail.
     parseJSON _          = mzero

-- ---------------------------------------------------------------------

data Switch = Switch

       { swH :: Maybe SocketHandle
       , swSender :: (String -> SockAddr -> TeleHash ())

       , swSeeds :: [HashName]
       , swLocals :: [String]
       , swLines :: [String]
       , swBridges :: [String]
       , swBridgeLine :: [String]
       , swAll :: Map.Map HashName HashContainer
       , swBuckets :: [Bucket]
       , swCapacity :: [String]
       , swRels :: [String]
       , swRaws :: Map.Map String (String -> Telex -> Channel -> IO ())
       , swPaths :: Map.Map PathId Path
       , swBridgeCache :: [String]
       , swNetworks :: Map.Map PathType (PathId,(Path -> LinePacket -> Maybe HashContainer -> TeleHash ()))

       , swHashname :: Maybe HashName

       , swId :: Map.Map String String
       , swIdCrypto :: Maybe HashCrypto

       -- , swCs :: Map.Map String (Map.Map String String)
       , swCs :: Map.Map String HashCrypto

       , swKeys :: Map.Map String String

       , swCSets :: Map.Map String CSet
       , swParts :: Parts -- ++AZ++ TODO: this should be a packet


       , swLoad :: Id -> TeleHash ()
       , swMake :: () -> () -> IO ()

       , swNat :: Bool
       , swSeed :: Bool
       , swLanToken :: Maybe String

       -- udp socket stuff
       , swPcounter :: Int
       , swReceive :: Packet -> Path -> IO ()

       -- outgoing packets to the network
       , swDeliver :: String -> () -> ()
       , swSend    :: Path -> LinePacket -> Maybe HashContainer -> TeleHash ()
       , swPathSet :: Path -> IO ()

       -- need some seeds to connect to, addSeed({ip:"1.2.3.4", port:5678, public:"PEM"})
       , swAddSeed :: SeedInfo -> TeleHash ()

       --  map a hashname to an object, whois(hashname)
       , swWhois :: HashName -> TeleHash (Maybe HashContainer)
       , swWhokey :: Parts -> Either String (Map.Map String String) -> TeleHash (Maybe HashContainer)

       , swStart :: String -> String -> String -> () -> IO ()
       , swOnline :: CallBack -> TeleHash ()
       , swIsOnline :: Bool
       , swListen :: String -> () -> IO ()

       -- advanced usage only
       , swRaw :: HashContainer -> String -> Telex -> CallBack -> TeleHash Channel

       -- primarily internal, to seek/connect to a hashname
       , swSeek :: String -> () -> IO ()

       , swBridge :: Path -> LinePacket -> Maybe HashContainer -> TeleHash ()

       -- for modules
       , swPencode :: Telex -> Body -> Telex
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

type CallBack = TeleHash ()

nullCb :: TeleHash ()
nullCb = return ()

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


-- ---------------------------------------------------------------------
-- Utility

-- | get current state of the given HashContainer
getHN :: HashName -> TeleHash (Maybe HashContainer)
getHN hashName = do
  sw <- get
  return $ Map.lookup hashName (swAll sw)

-- | update the stored state of the given HashContainer
putHN :: HashContainer -> TeleHash ()
putHN hn = do
  sw <- get
  put $ sw { swAll = Map.insert (hHashName hn) hn (swAll sw)}

