{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module TeleHash.Utils
  (
   TeleHash
  , Signal(..)
  , Reply(..)
  , Switch(..)
  , showSwitch
  , PathId(..)
  , SeedInfo(..)
  , Bucket(..)
  , HashDistance
  , Path(..)
  -- , PathType(..)
  , pathFromPathJson
  , PathPriority
  , showPath
  , pathType
  , pathIp
  , pathPort
  , pathHttp
  , NetworkTelex(..)
  , OpenizeInner(..)
  , LinkMessage(..)
  , LinkMaintMessage(..)
  , Telex(..)
  , emptyTelex
  , RxTelex(..)
  , rxTelexToTelex
  , DeOpenizeResult(..)
  , Hash(..)
  , unHash
  , HashName(..)
  , unHN
  , HashContainer(..)
  , HashCrypto(..)
  , Parts(..)
  , Channel(..)
  , ChannelId(..)
  , unChannelId
  , channelSlot
  -- , Line(..)
  , CSet(..)
  , PublicKey(..)
  , PrivateKey(..)
  , Id(..)
  , SocketHandle(..)
  , CallBack
  , RxCallBack
  , HnCallBack
  , nullCb
  , nullRxCb
  , nullHnCb
  , IPP(..)
  , unIPP
  , parts2hn
  , io
  , logT

  , ghead
  , glast
  , gtail
  , gfromJust

  , valToBs
  , valToString
  , b16Tobs

  , showJson
  , parseJs

  , putHN
  , getHN
  , getHNsafe
  , putChan
  , getChan
  , delChan
  , incPCounter

  -- * DHT operations
  , getBucketContents
  , getBucketSize
  , storeHashInDht
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
import TeleHash.Convert
import TeleHash.Packet
import TeleHash.Paths

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
  { hHashName   :: !HashName
  , hSelf       :: !(Maybe HashCrypto)
  , hCsid       :: !(Maybe String)
  , hChans      :: !(Map.Map ChannelId Channel)
  , hPaths      :: ![Path]
  , hTo         :: !(Maybe Path)
  , hIsAlive    :: !Bool
  , hIsPublic   :: !Bool
  , hIsSeed     :: !Bool
  , hAt         :: !ClockTime
  , hBucket     :: !HashDistance
  , hAge        :: !(Maybe ClockTime)
  , hChanOut    :: !ChannelId
  , hLineOut    :: !String -- randomHEX 16 output. Make it a type
  , hLineAt     :: !(Maybe ClockTime)
  , hLineIn     :: !(Maybe BC.ByteString)
  , hSendSeek   :: !(Maybe ClockTime)
  , hVias       :: !(Map.Map HashName String)
  , hLastPacket :: !(Maybe Telex)
  , hParts      :: !(Maybe Parts)
  , hOpened     :: !(Maybe LinePacket)
  , hOpenAt     :: !(Maybe ClockTime)
  , hRecvAt     :: !(Maybe ClockTime)
  , hPriority   :: !(Maybe PathPriority)

  , hIp         :: !(Maybe IP)
  , hPort       :: !(Maybe Port)
  , hBridging   :: !Bool
  , hIsLocal    :: !Bool
  , hLinked     :: !(Maybe ChannelId)


  -- CS 1a stuff
  , hLineIV  :: !Word32 -- crypto 1a IV value
  -- , hlineInB :: !BC.ByteString
  , hEncKey  :: !(Maybe BC.ByteString)
  , hDecKey  :: !(Maybe BC.ByteString)
  , hEcc     :: !(Maybe (PublicKey,PrivateKey)) -- our DH ECC key for
                                               -- communicating with this remote
  } deriving (Show)

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
  { hcHashName :: !HashName
  , hcHexName  :: !Hash
  , hcParts    :: !Parts
  , hcCsid     :: !String
  , hcKey      :: !String
  , hcPublic   :: !PublicKey
  , hcPrivate  :: !(Maybe PrivateKey)
  -- , hcEccKeys  :: Maybe (DH.PublicNumber,DH.PrivateNumber)
  } deriving Show

data PublicKey = Public1a ECDSA.PublicKey deriving Show
data PrivateKey = Private1a ECDSA.PrivateKey deriving Show

-- ---------------------------------------------------------------------

data CSet = CS
  { csLoadkey   :: String -> Maybe String -> TeleHash (Maybe HashCrypto)
  , csOpenize   :: HashContainer -> OpenizeInner -> TeleHash LinePacket
  , csDeopenize :: NetworkPacket -> TeleHash DeOpenizeResult
  , csOpenLine  :: HashContainer ->  DeOpenizeResult -> TeleHash ()
  , csDelineize :: HashContainer -> NetworkTelex -> TeleHash (Either String RxTelex)
  }

-- ---------------------------------------------------------------------

{-
data PathType = PathType String
              deriving (Show,Eq,Ord)

unPathType :: PathType -> String
unPathType (PathType str) = str
-}

type PathPriority = Int

-- TODO: provide custom Eq instance, checking core vals only
data Path = Path
      { pJson     :: !PathJson

      , pRelay    :: !(Maybe Channel)  -- relay
      , pId       :: !(Maybe HashName) -- local
      , pLastIn   :: !(Maybe ClockTime)
      , pLastOut  :: !(Maybe ClockTime)
      , pPriority :: !(Maybe PathPriority)
      , pIsSeed   :: !Bool
      , pGone     :: !Bool -- may not be meaningful due to functional
                           -- nature of haskell
      } deriving (Show,Eq,Ord)

instance Aeson.ToJSON Path where
  toJSON p = Aeson.toJSON (pJson p)

-- ---------------------------------------------------------------------

pathFromPathJson :: PathJson -> Path
pathFromPathJson pj
  = Path { pJson = pj
         , pRelay = Nothing
         , pId = Nothing
         , pLastIn = Nothing
         , pLastOut = Nothing
         , pPriority = Nothing
         , pIsSeed = False
         , pGone = False
         }

-- ---------------------------------------------------------------------

pathType :: Path -> PathType
pathType p = pjsonType $ pJson p

pathIp :: Path -> Maybe IP
pathIp p = pjsonIp $ pJson p

pathPort :: Path -> Maybe Port
pathPort p = pjsonPort $ pJson p

pathHttp :: Path -> Maybe Url
pathHttp p = pjsonHttp $ pJson p

-- ---------------------------------------------------------------------

data PathId = PId Int
            deriving (Ord,Eq,Show)

-- ---------------------------------------------------------------------

showPath :: Path -> String
showPath p = showPathJson (pJson p)

-- ---------------------------------------------------------------------

data Relay = Relay deriving (Show,Eq)

-- ---------------------------------------------------------------------

-- a Telex gets packed to/from a Packet
data Telex = Telex { tId     :: !(Maybe HashName)
                   , tType   :: !(Maybe String)
                   , tPath   :: !(Maybe Path)
                   , tTo     :: !(Maybe Path)
                   , tJson   :: !(HM.HashMap Text.Text Aeson.Value)
                   , tPacket :: !(Maybe Packet)
                   , tCsid   :: !(Maybe String)
                   , tChanId :: !(Maybe ChannelId)
                   } deriving Show

emptyTelex :: Telex
emptyTelex = Telex { tId     = Nothing
                   , tType   = Nothing
                   , tPath   = Nothing
                   , tTo     = Nothing
                   , tJson   = HM.empty
                   , tPacket = Nothing
                   , tCsid   = Nothing
                   , tChanId = Nothing
                   }

data RxTelex = RxTelex { rtId     :: !Int
                       , rtSender :: !Path
                       , rtAt     :: !ClockTime
                       , rtJs     :: !(HM.HashMap Text.Text Aeson.Value)
                       , rtPacket :: !Packet
                       , rtChanId :: !(Maybe ChannelId) -- rtFrom
                       } deriving Show

data NetworkTelex = NetworkTelex
                       { ntId     :: !Int
                       , ntSender :: !Path
                       , ntAt     :: !ClockTime
                       , ntPacket :: !NetworkPacket
                       } deriving Show

data DeOpenizeResult = DeOpenizeVerifyFail
                     | DeOpenize { doLinePub :: !PublicKey
                                 , doKey     :: !BC.ByteString
                                 , doJs      :: !Aeson.Value
                                 , doCsid    :: !String
                                 }
                     deriving (Show)

-- ---------------------------------------------------------------------

-- |The information carried in the inner packet of an openize
data OpenizeInner = OpenizeInner
                    { oiAt   :: !ClockTime
                    , oiTo   :: !HashName
                    , oiFrom :: !Parts
                    , oiLine :: !String -- TODO: should be its own type
                    }

-- ---------------------------------------------------------------------

-- |Carried in a link message
data LinkMessage = LinkMessage
                   { lType :: !(Maybe String) -- only present in initial message
                   , lChan :: !ChannelId
                   , lSeed :: !Bool
                   , lSee :: ![String]
                   , lBridges :: !(Maybe [String])
                   } deriving (Show)

instance Aeson.ToJSON LinkMessage where
     toJSON lm = object $ stripNulls
                        [ "type"   .= lType lm
                        , "c"      .= lChan lm
                        , "seed"   .= lSeed lm
                        , "see"    .= lSee lm
                        , "bridge" .= lBridges lm
                        ]

instance Aeson.FromJSON LinkMessage where
     parseJSON (Aeson.Object v)
           = LinkMessage <$> v .:? "type" <*>
                             v .: "c" <*>
                             v .: "seed" <*>
                             v .: "see" <*>
                             v .:? "bridge"

-- ---------------------------------------------------------------------

-- |Carried in a link message for maintenance
data LinkMaintMessage = LinkMaintMessage
                   { lmChan :: !ChannelId
                   , lmSeed :: !Bool
                   } deriving (Show)

instance Aeson.ToJSON LinkMaintMessage where
     toJSON lm = object $ stripNulls
                        [ "c"      .= lmChan lm
                        , "seed"   .= lmSeed lm
                        ]

instance Aeson.FromJSON LinkMaintMessage where
     parseJSON (Aeson.Object v)
           = LinkMaintMessage <$>
                             v .: "c" <*>
                             v .: "seed"

-- ---------------------------------------------------------------------

rxTelexToTelex :: RxTelex -> Telex
rxTelexToTelex rx
 = emptyTelex
    { tJson   = rtJs rx
    , tPath   = Just (rtSender rx)
    -- , tPacket = Just (rtPacket rx)
    , tPacket = Nothing
    }
  where
    js = map (\(k,v) -> (Text.unpack k,TL.unpack $ toLazyText $ encodeToTextBuilder v))
            $ HM.toList (rtJs rx)

-- ---------------------------------------------------------------------

data Channel = Chan
  { chType     :: !String
  , chCallBack :: !RxCallBack
  , chId       :: !ChannelId
  , chHashName :: !HashName -- for convenience
  , chLast     :: !(Maybe Path)
  , chSentAt   :: !(Maybe ClockTime)
  , chRxAt     :: !(Maybe ClockTime)
  , chEnded    :: !Bool
  , chDone     :: !Bool
  } deriving (Show,Eq,Ord)

instance Show (CallBack) where
  show _ = "CallBack"

instance Eq CallBack where
  _ == _ = True

instance Show (RxCallBack) where
  show _ = "RxCallBack"

instance Eq RxCallBack where
  _ == _ = True

instance Ord RxCallBack where

type HashDistance = Int

-- |channel id is a positive number from 1 to 4,294,967,295 (UINT32)
data ChannelId = CID Int deriving (Eq,Show,Ord)
unChannelId (CID c) = c

instance Num ChannelId where
  (CID a) + (CID b) = CID (a + b)
  (CID _) * (CID _) = error "cannot multiply ChannelIds"
  (CID a) - (CID b) = CID (a - b)
  abs (CID a) = CID (abs a)
  signum (CID a) = CID (signum a)
  fromInteger i = CID (fromIntegral i)

channelSlot :: ChannelId -> Int
channelSlot (CID n) = n `mod` 2

instance Aeson.ToJSON ChannelId where
  toJSON (CID c) = Aeson.toJSON c

instance Aeson.FromJSON ChannelId where
  parseJSON c = CID <$> Aeson.parseJSON c

-- ---------------------------------------------------------------------

-- type Bucket = [Line]
type Bucket = Set.Set HashName

{-
data Line = Line { lineAge     :: !ClockTime
                 , lineSeed    :: !HashName
                 , lineAddress :: !String
                 , lineLinked  :: !(Maybe Path)
                 , lineAlive   :: !Bool
                 , lineSentat  :: !(Maybe ClockTime)
                 } deriving (Show,Eq,Ord)
-}

-- ---------------------------------------------------------------------

{-
data Seed = Seed { sAlive :: Bool
                 , sLink :: CallBack ->TeleHash ()
                 }
-}

-- ---------------------------------------------------------------------

type Parts = [(String,String)] -- [(csid,key)]


data SeedInfo = SI
  { sId       :: !String
  , sAdmin    :: !String
  , sPaths    :: ![Path]
  , sParts    :: !Parts -- crypto ids?
  , sKeys     :: ![(String,String)] -- crypto scheme name, crypto key
  , sIsBridge :: !Bool
  } deriving Show


-- ---------------------------------------------------------------------

data Id = Id { id1a        :: !String
             , id1a_secret :: !String
             } deriving Show


instance Aeson.FromJSON Id where
     parseJSON (Aeson.Object v) = Id <$>
                                  v Aeson..: "1a" <*>
                                  v Aeson..: "1a_secret"
     -- A non-Object value is of the wrong type, so fail.
     parseJSON _          = mzero

-- ---------------------------------------------------------------------

data Signal = SignalPingSeeds | SignalScanLines | SignalTapTap | SignalMsgRx BC.ByteString NS.SockAddr |
              SignalGetSwitch | SignalShowSwitch
            | SignalSyncPath HashName
              deriving (Typeable, Show, Eq)

data Reply = ReplyGetSwitch Switch
           -- deriving (Typeable, Show)
           deriving (Typeable)

-- ---------------------------------------------------------------------


data Switch = Switch

       { swH      :: !(Maybe SocketHandle)
       , swChan   :: !(Maybe (Chan Signal))
       , swSender :: !(LinePacket -> SockAddr -> TeleHash ())

       , swSeeds      :: ![HashName]
       , swLocals     :: !(Set.Set HashName)
       , swLines      :: !(Map.Map String HashName)
       , swBridges    :: ![String]
       , swBridgeLine :: ![String]
       , swBridging   :: !Bool
       , swAll        :: !(Map.Map HashName HashContainer)
       , swBuckets    :: !(Map.Map HashDistance Bucket)
       , swCapacity   :: ![String]
       , swRels       :: !(Map.Map String (Bool -> RxTelex -> Channel -> TeleHash ()))
       , swRaws       :: !(Map.Map String (Bool -> RxTelex -> Channel -> TeleHash ()))
       , swPaths      :: !(Map.Map PathId Path)
       , swBridgeCache :: ![String]
       , swNetworks    :: !(Map.Map PathType (PathId,(Path -> LinePacket -> Maybe HashName -> TeleHash ())))

       , swHashname :: !(Maybe HashName)

       , swId :: !(Map.Map String String)
       , swIdCrypto :: !(Maybe HashCrypto)

       -- , swCs :: Map.Map String (Map.Map String String)
       , swCs :: !(Map.Map String HashCrypto)

       , swKeys :: !(Map.Map String String)

       , swCSets :: !(Map.Map String CSet)
       , swParts :: !Parts


       , swLoad :: Id -> TeleHash ()
       , swMake :: () -> () -> IO ()

       , swNat :: !Bool
       , swSeed :: !Bool
       , swLanToken :: !(Maybe String)
       , swPub4 :: !(Maybe IP) -- Our public IPv4 address, behind
                               -- NATting etc.

       , swPriority :: !(Maybe PathPriority)

       , swPcounter :: !Int

       , swIsOnline :: !Bool


       , swWaits :: [String]
       , swWaiting :: Maybe (TeleHash ())
       -- , swWait :: Bool -> IO ()


       -- crypto
       , swRNG :: !SystemRNG

       , swCountOnline :: !Int
       , swCountTx :: !Int
       , swCountRx :: !Int
       }

-- ---------------------------------------------------------------------

showSwitch :: Switch -> String
showSwitch sw =
  ("switch:"++ show (swHashname sw)
  ++ "\nlinescount=" ++ show (Map.size $ swLines sw)
  ++" hashcount:" ++  show (Map.size $ swAll sw)
  ++"\n  " ++ (intercalate "\n  " (map show $ Map.keys (swAll sw)))
  ++"\nbucketCount:" ++ show (Map.size (swBuckets sw))
  ++"\nbuckets:" ++ show (swBuckets sw)
  )

-- ---------------------------------------------------------------------

type CallBack = TeleHash ()

nullCb :: TeleHash ()
nullCb = return ()

type RxCallBack = Bool -> RxTelex -> Channel -> TeleHash ()

nullRxCb :: Bool -> RxTelex -> Channel -> TeleHash ()
nullRxCb _ _ _= return ()

type HnCallBack = HashName -> TeleHash ()

nullHnCb :: HnCallBack
nullHnCb _ = return ()

-- ---------------------------------------------------------------------

newtype IPP = IPP String
             deriving (Eq,Show,Ord)
unIPP :: IPP -> String
unIPP (IPP str) = str

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

valToBs :: Aeson.Value -> BC.ByteString
valToBs (Aeson.String val) = BC.pack $ Text.unpack val

-- ---------------------------------------------------------------------

valToString :: Aeson.Value -> String
valToString (Aeson.String val) = Text.unpack val


b16Tobs :: BC.ByteString -> BC.ByteString
b16Tobs str = r
  where
   (r,_) = B16.decode str

-- ---------------------------------------------------------------------

showJson :: Aeson.ToJSON a => a -> String
showJson j = BC.unpack $ lbsTocbs $ encode $ j

-- ---------------------------------------------------------------------

-- See https://gist.github.com/alanz/2465584
--  and https://github.com/bos/aeson/issues/77
stripNulls :: [Pair] -> [Pair]
stripNulls xs = filter (\(_,v) -> v /= Aeson.Null) xs

-- ---------------------------------------------------------------------

parseJs :: (FromJSON a) => (HM.HashMap Text.Text Aeson.Value) -> Maybe a
parseJs v = r
  where
    mp = fromJSON (Object v)
    r = case mp of
        Error err1 -> Nothing
        Success val -> Just val

-- ---------------------------------------------------------------------

-- Utility

-- ---------------------------------------------------------------------

-- | get current state of the given HashContainer
getHN :: HashName -> TeleHash (Maybe HashContainer)
getHN hashName = do
  -- logT $ "getHN " ++ show hashName
  sw <- get
  return $ Map.lookup hashName (swAll sw)

-- | get current state of the given HashContainer
getHNsafe :: HashName -> String -> TeleHash HashContainer
getHNsafe hashName tag = do
  -- logT $ "getHNsafe " ++ show (hashName,tag)
  sw <- get
  let mhn = Map.lookup hashName (swAll sw)
  return $ gfromJust tag mhn

-- | update the stored state of the given HashContainer
putHN :: HashContainer -> TeleHash ()
putHN hn = do
  -- logT $ "putHN " ++ show (hHashName hn)
  sw <- get
  put $ sw { swAll = Map.insert (hHashName hn) hn (swAll sw)}


-- ---------------------------------------------------------------------

incPCounter :: TeleHash Int
incPCounter = do
  sw <- get
  let r = 1 + (swPcounter sw)
  put $ sw {swPcounter = r }
  return r


-- ---------------------------------------------------------------------

putChan :: HashName -> Channel -> TeleHash ()
putChan hn chan = do
  -- logT $ "putChan:" ++ show (hn,chId chan)
  hc <- getHNsafe hn ("putChan " ++ show hn)
  let chans = Map.insert (chId chan) chan (hChans hc)
  putHN $ hc { hChans = chans }

getChan :: HashName -> ChannelId -> TeleHash (Maybe Channel)
getChan hn cid = do
  -- logT $ "getChan:" ++ show (hn,cid)
  hc <- getHNsafe hn "getChan"
  return $ Map.lookup cid (hChans hc)

delChan :: HashName -> ChannelId -> TeleHash ()
delChan hn cid = do
  -- logT $ "delChan:" ++ show (hn,cid)
  hc <- getHNsafe hn "delChan"
  let chans = Map.delete cid (hChans hc)
  putHN $ hc { hChans = chans }

-- ---------------------------------------------------------------------

-- |Get the relevant bucket, and dereference all the HashNames
getBucketContents :: HashDistance -> TeleHash [HashContainer]
getBucketContents hd = do
  sw <- get
  let bucket = (Map.findWithDefault Set.empty hd (swBuckets sw))
  mhcs <- mapM getHN $ Set.toList bucket
  return $ catMaybes mhcs

-- ---------------------------------------------------------------------

-- |Get the current size of the given bucket
getBucketSize :: HashDistance -> TeleHash Int
getBucketSize hd = do
  buckets <- gets swBuckets
  case Map.lookup hd buckets of
    Nothing -> return 0
    Just bucket -> return (Set.size bucket)

-- ---------------------------------------------------------------------

-- |Insert the given hash into the appropriate bucket if required
storeHashInDht :: HashName -> HashDistance -> TeleHash ()
storeHashInDht hn hd = do
  sw <- get
  let
    swb = case Map.lookup hd (swBuckets sw) of
      Nothing -> Map.insert hd (Set.fromList [hn]) (swBuckets sw)
      Just bucket -> Map.insert hd (Set.insert hn bucket) (swBuckets sw)

  put $ sw { swBuckets = swb }

-- ---------------------------------------------------------------------

-- testing JSON for LinkMessage

ttelm :: Maybe LinkMessage
ttelm = Aeson.decode "{\"seed\":false,\"c\":1,\"see\":[],\"type\":\"link\"}"

ttelm1 :: Maybe LinkMessage
ttelm1 = Aeson.decode "{\"seed\":false,\"c\":1,\"see\":[]}"

ttelm2 :: Maybe LinkMessage
ttelm2 = Aeson.decode "{\"seed\":false,\"c\":1,\"see\":[],\"bridge\":[]}"

ttelm3 = Aeson.encode (LinkMessage
                        { lType = Nothing
                        , lChan = CID 1
                        , lSeed = True
                        , lSee = []
                        , lBridges = Just ["foo"]
                        })

ttelm4 = Aeson.encode (LinkMessage
                        { lType = Just "link"
                        , lChan = CID 1
                        , lSeed = True
                        , lSee = []
                        , lBridges = Nothing
                        })
