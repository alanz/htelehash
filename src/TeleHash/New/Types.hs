{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module TeleHash.New.Types
  (
    HashName(..)
  , unHN
  , Hash(..)
  , unHash
  , Uid
  , PathId
  , TxTelex(..)
  , RxTelex(..)
  , packet_new_rx
  , packet_new
  , TeleHash
  , Switch(..)
  , Bucket(..)
  , nullHandler

  , HashContainer(..)

  -- * Channel related types
  , TChan(..)
  , ChannelId(..)
  , unChannelId
  , channelSlot
  , ChannelHandler
  , ChannelState(..)
  , Crypto(..)
  , Parts
  , NetworkTelex(..)
  , DeOpenizeResult(..)
  , PublicKey
  , PrivateKey
  , Signal(..)
  , Reply(..)
  , SocketHandle(..)
  , Id(..)
  , SeedInfo(..)
  , Path(..)
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

-- import TeleHash.New.Crypt
import TeleHash.New.Packet
import TeleHash.New.Paths

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

data HashName = HN String
              deriving (Eq,Show,Ord)
unHN :: HashName -> String
unHN (HN s) = s

type Uid = Int

data RxTelex = RxTelex
      { rtId     :: !Int
      , rtSender :: !Path
      , rtAt     :: !ClockTime
      , rtJs     :: !(HM.HashMap Text.Text Aeson.Value)
      , rtPacket :: !Packet
      , rtChanId :: !(Maybe ChannelId) -- tFrom
      -- , tFrom   :: !HashName
      } deriving Show

data TxTelex = TxTelex
      { tId     :: !Int
      , tTo     :: !HashName
      , tOut    :: !PathId
      , tJs     :: !(HM.HashMap Text.Text Aeson.Value)
      , tPacket :: !Packet
      } deriving Show

packet_new_rx :: RxTelex
packet_new_rx =
  RxTelex
    { rtId = 0
    , rtSender = nullPath
    , rtAt = TOD 0 0
    , rtJs = HM.empty
    , rtPacket = newPacket
    , rtChanId = Nothing
    }

packet_new :: HashName -> TxTelex
packet_new to =
  TxTelex
    { tId = 0
    , tTo = to
    , tOut = nullPathId
    , tJs = HM.empty
    , tPacket = newPacket
    }


data Bucket = Bucket
      {
      } deriving Show

-- ---------------------------------------------------------------------

-- The 'TeleHash' monad, a wrapper over IO, carrying the switch's immutable state.
--
type TeleHash = StateT Switch IO

-- ---------------------------------------------------------------------

data Switch = Switch
       { swId         :: !HashName
       , swSeeds      :: !Bucket
       , swOut        :: ![TxTelex] -- packets waiting to be delivered
       , swLast       :: !(Maybe TxTelex)
       , swParts      :: !TxTelex
       , swChans      :: !(Map.Map Uid TChan) -- channels waiting to be processed
       , swUid        :: !Uid
       , swCap        :: !Int
       , swWindow     :: !Int
       , swIsSeed     :: !Bool
       , swIndex      :: !(Map.Map HashName HashContainer)
       , swIndexChans :: !(Map.Map Uid TChan) -- all channels
       , swHandler    :: !(HashContainer -> TeleHash ()) -- called w/ a hn that has no key info

       , swH      :: !(Maybe SocketHandle)
       , swChan   :: !(Maybe (Chan Signal))
       , swSender :: !(LinePacket -> SockAddr -> TeleHash ())
       }
     deriving Show

instance Show (HashContainer -> TeleHash ()) where
  show _ = "(HashContainer -> TeleHash ())"

instance Show (Chan Signal) where
  show _ = "(Chan Signal)"

{-

typedef struct switch_struct
{
  hn_t id;
  bucket_t seeds;
  packet_t out, last; // packets waiting to be delivered
  packet_t parts;
  chan_t chans; // channels waiting to be processed
  uint32_t uid;
  int cap, window;
  uint8_t isSeed;
  xht_t index;
  void (*handler)(struct switch_struct *, hn_t); // called w/ a hn that has no key info
} *switch_t;

-}

nullHandler :: HashContainer -> TeleHash ()
nullHandler _ = return ()

-- ---------------------------------------------------------------------

data HashContainer = H
  { hHashName :: !HashName
  , hCsid     :: !String
  , hChanOut  :: !ChannelId
  , hCrypto   :: !Crypto
  , hPaths    :: ![Path]
  , hLast     :: !(Maybe Path)
  , hChans    :: !(Map.Map ChannelId TChan)
  , hOnopen   :: !(Maybe TxTelex)
  , hParts    :: !(Maybe TxTelex)

                                               -- communicating with this remote
  } deriving (Show)

{-
typedef struct hn_struct
{
  unsigned char hashname[32];
  char csid, hexid[3], hexname[65]; // for convenience
  unsigned long chanOut;
  crypt_t c;
  path_t *paths, last;
  xht_t chans;
  packet_t onopen, parts;
} *hn_t;
-}


type PathPriority = Int

-- TODO: provide custom Eq instance, checking core vals only
data Path = Path
      { pJson     :: !PathJson

      , pRelay    :: !(Maybe TChan)  -- relay
      , pId       :: !(Maybe HashName) -- local
      , pLastIn   :: !(Maybe ClockTime)
      , pLastOut  :: !(Maybe ClockTime)
      , pPriority :: !(Maybe PathPriority)
      , pIsSeed   :: !Bool
      , pGone     :: !Bool -- may not be meaningful due to functional
                           -- nature of haskell
      } deriving (Show)

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

nullPath = pathFromPathJson (PWebRtc (PathWebRtc "*null*"))

nullPathId = PId (-1) -- horrible, need better way of doing this

-- ---------------------------------------------------------------------
-- Channel related types

{-

typedef struct chan_struct
{
  uint32_t id;
  unsigned char hexid[9], uid[9];
  struct switch_struct *s;
  struct hn_struct *to;
  char *type;
  int reliable;
  enum {STARTING, OPEN, ENDING, ENDED} state;
  struct path_struct *last;
  struct chan_struct *next;
  packet_t in, inend, notes;
  void *arg; // used by app
  void *seq, *miss; // used by chan_seq/chan_miss
  void (*handler)(struct chan_struct*); // auto-fire callback
} *chan_t;
-}

data ChannelState = ChanStarting | ChanOpen | ChanEnding | ChanEnded
         deriving (Eq,Show)

type ChannelHandler = (ChannelId -> TeleHash ())

instance Show ChannelHandler where
  show _ = "ChannelHandler"

data TChan = TChan
  { chId       :: !ChannelId -- also hexid,uid
  , chUid      :: !Uid -- Switch wide unique Id. pk.
  , chTo       :: !HashName
  , chType     :: !String
  , chReliable :: !Bool
  , chState    :: !ChannelState
  , chLast     :: !(Maybe PathId)
  , chNext     :: !(Maybe ChannelId)
  , chIn       :: ![RxTelex] -- queue of incoming messages
  , chNotes    :: ![RxTelex]
  , chHandler  :: !(Maybe ChannelHandler) -- auto-fire callback
  } deriving (Show)

{-
typedef struct chan_struct
{
  uint32_t id;
  unsigned char hexid[9], uid[9];
  struct switch_struct *s;
  struct hn_struct *to;
  char *type;
  int reliable;
  enum {STARTING, OPEN, ENDING, ENDED} state;
  struct path_struct *last;
  struct chan_struct *next;
  packet_t in, inend, notes;
  void *arg; // used by app
  void *seq, *miss; // used by chan_seq/chan_miss
  void (*handler)(struct chan_struct*); // auto-fire callback
} *chan_t;

-}


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

newtype Hash = Hash String
             deriving (Eq,Show,Ord)
unHash :: Hash -> String
unHash (Hash str) = str

type Parts = [(String,String)] -- [(csid,key)]

-- ---------------------------------------------------------------------

data NetworkTelex = NetworkTelex
                       { ntId     :: !Int
                       , ntSender :: !Path
                       , ntAt     :: !ClockTime
                       , ntPacket :: !NetworkPacket
                       } deriving Show

-- ---------------------------------------------------------------------

-- |The information carried in the inner packet of an openize
data OpenizeInner = OpenizeInner
                    { oiAt   :: !ClockTime
                    , oiTo   :: !HashName
                    , oiFrom :: !Parts
                    , oiLine :: !String -- TODO: should be its own type
                    }

-- ---------------------------------------------------------------------

data DeOpenizeResult = DeOpenizeVerifyFail
                     | DeOpenize { doLinePub :: !PublicKey
                                 , doKey     :: !BC.ByteString
                                 , doJs      :: !Aeson.Value
                                 , doCsid    :: !String
                                 }
                     deriving (Show)

-- ---------------------------------------------------------------------

data Crypto = Crypto
  { cCsid :: !String
  , cIsPrivate :: !Bool
  , cLined :: !Bool
  , cKeyLen :: !Int
  , cAtOut  :: !ClockTime
  , cAtIn   :: !ClockTime
  , cLineOut :: !String
  , cLineIn  :: !String
  , cKey     :: !String
  , cCs      :: !String -- TBD, individual crypto structures
  } deriving Show

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

data SocketHandle =
    SocketHandle {slSocket :: Socket
                 --, slAddress :: SockAddr
                 } deriving (Eq,Show)


instance Show (LinePacket -> SockAddr -> TeleHash ()) where
  show _ = "(LinePacket -> SockAddr -> TeleHash ())"
