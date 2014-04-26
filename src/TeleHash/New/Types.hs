{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TeleHash.New.Types
  (
    HashName
  , unHN
  , Uid
  , PathId
  , TxTelex(..)
  , RxTelex(..)
  , packet_new
  , TeleHash
  , Switch(..)

  , HashContainer(..)

  -- * Channel related types
  , TChan(..)
  , ChannelId(..)
  , unChannelId
  , channelSlot
  , ChannelHandler
  , ChannelState(..)
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

import TeleHash.New.Crypt
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


-- ---------------------------------------------------------------------

data HashName = HN String
              deriving (Eq,Show,Ord)
unHN :: HashName -> String
unHN (HN s) = s

type Uid = Int

type PathId = Int


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
      , tOut    :: !Path
      , tJs     :: !(HM.HashMap Text.Text Aeson.Value)
      , tPacket :: !Packet
      } deriving Show

packet_new :: HashName -> TxTelex
packet_new to =
  TxTelex
    { tId = 0
    , tTo = to
    , tOut = nullPath
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
       , swIndexChans :: !(Map.Map Uid TChan)
       , swHandler    :: !(HashContainer -> TeleHash ()) -- called w/ a hn that has no key info
       }
     deriving Show

instance Show (HashContainer -> TeleHash ()) where
  show _ = "(HashContainer -> TeleHash ())"

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


-- ---------------------------------------------------------------------

data Path = Path
     {
     } deriving Show

nullPath = Path

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
  , chInEnd    :: !(Maybe RxTelex)
  , chNotes    :: ![RxTelex]
  , chHandler  :: !(Maybe ChannelHandler) -- auto-fire callback
  } deriving Show

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
