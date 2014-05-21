{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverlappingInstances #-} -- For show of the function types

module Network.TeleHash.Types
  (
  -- * Constants
    param_k
  , param_link_max
  , param_link_ping_secs
  , param_link_timeout_secs
  , param_seek_wait_secs

  -- * Types
  , HashName(..)
  , unHN
  , Hash(..)
  , unHash
  , Uid
  , TxId
  , PathId
  , TxTelex(..)
  , RxTelex(..)
  , packet_new_rx
  , packet_new
  , rxTelexToTxTelex
  , txTelexToRxTelex
  , TeleHash
  , Switch(..)
  , Bucket
  , nullHandler

  , HashContainer(..)
  , newHashContainer
  , HashDistance

  -- * JSON holders
  , OpenizeInner(..)
  , LinkReply(..)

  -- * Channel related types
  , TChan(..)
  , CArg(..)
  , ChannelId(..)
  , nullChannelId
  , unChannelId
  , channelSlot
  , ChannelHandler
  , ChannelState(..)
  , Seq(..)
  , Miss(..)
  , Crypto(..)
  , Crypt1a(..)
  , PublicKey(..)
  , PrivateKey(..)
  , LinedState(..)
  , Parts
  , NetworkTelex(..)
  , DeOpenizeResult(..)
  , Signal(..)
  , Reply(..)
  , SocketHandle(..)
  , Id(..)
  , SeedInfo(..)
  , Path(..)
  , pathFromPathJson
  , pathType
  , pathIp
  , pathPort
  , pathHttp
  , showPath

  -- * Extensions types
  , Thtp(..)
  , OkFail(..)

  , ChatId(..)
  , ChatPerm(..)
  , ChatHash(..)
  , unCH
  , chatIdToString
  , Chat(..)
  , ChatRId(..)
  , ChatR(..)

  , Link(..)
  , Seek(..)
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Crypto.Random
import Data.Aeson.Types
import Data.IP
import Data.List
import Data.Maybe
import Data.Typeable
import Data.Word
import Network.Socket
import Prelude hiding (id, (.), head, either)
import System.Time

import Network.TeleHash.Convert
import Network.TeleHash.Packet
import Network.TeleHash.Paths

import qualified Crypto.Types.PubKey.ECDSA as ECDSA
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Network.Socket as NS
import Text.Show.Functions

-- ---------------------------------------------------------------------

-- Parameters. See
-- https://github.com/telehash/telehash.org/blob/master/implementers.md#default-configuration-values


param_k :: Int
param_k = 8

param_link_max :: Int
param_link_max = 256

param_link_ping_secs :: Integer
param_link_ping_secs = 29

param_link_timeout_secs :: Int
param_link_timeout_secs = 60

-- |How long to wait befor starting another seek
param_seek_wait_secs :: Int
param_seek_wait_secs = 5

-- ---------------------------------------------------------------------

data HashName = HN String
              deriving (Eq,Show,Ord)
unHN :: HashName -> String
unHN (HN s) = s

type Uid = Int
type TxId = Int

data RxTelex = RxTelex
      { rtId     :: !Int
      , rtSender :: !PathJson
      , rtAt     :: !ClockTime
      , rtJs     :: !(HM.HashMap Text.Text Aeson.Value)
      , rtPacket :: !Packet
      , rtChanId :: !(Maybe ChannelId) -- tFrom
      } deriving Show

data TxTelex = TxTelex
      { tId     :: !TxId
      , tTo     :: !HashName
      , tOut    :: !PathJson
      , tJs     :: !(HM.HashMap Text.Text Aeson.Value)
      , tPacket :: !Packet
      , tChain  :: !(Maybe TxTelex)
      , tLp     :: !(Maybe LinePacket)
      } deriving (Show,Eq)

packet_new_rx :: RxTelex
packet_new_rx =
  RxTelex
    { rtId = 0
    , rtSender = nullPathJson
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
    , tOut = nullPathJson
    , tJs = HM.empty
    , tPacket = newPacket
    , tChain = Nothing
    , tLp    = Nothing
    }

rxTelexToTxTelex :: RxTelex -> HashName -> TxTelex
rxTelexToTxTelex rx hn
 = (packet_new hn)
    { tId = 0
    , tOut = rtSender rx
    , tJs = rtJs rx
    , tPacket = rtPacket rx
    }

txTelexToRxTelex :: TxTelex -> RxTelex
txTelexToRxTelex tx
 = (packet_new_rx)
    { rtId = tId tx
    , rtSender = tOut tx
    , rtAt = TOD 0 0
    , rtJs = tJs tx
    , rtPacket = tPacket tx
    , rtChanId = Nothing
    }


type Bucket = Set.Set HashName

-- ---------------------------------------------------------------------

-- The 'TeleHash' monad, a wrapper over IO, carrying the switch's immutable state.
--
type TeleHash = StateT Switch IO

-- ---------------------------------------------------------------------

data Switch = Switch
       { swId          :: !HashName
       , swParts       :: !Parts
       , swExternalIPP :: !(Maybe PathIPv4) -- TODO: should be in the
                                            -- HC for swId
       , swSeeds       :: !Bucket
       , swOut         :: ![TxTelex] -- packets waiting to be delivered
       , swLast        :: !(Maybe TxTelex)
       , swChans       :: !(Set.Set Uid) -- channels waiting to be processed

       -- unique id sources
       , swUid         :: !Uid
       , swTxid        :: !TxId
       , swCrid        :: !ChatRId

       , swCap         :: !Int
       , swWindow      :: !Int
       , swIsSeed      :: !Bool

       -- The DHT
       , swDhtK        :: !Int -- max entries per bucket
       , swDhtLinkMax  :: !Int -- max number of concurrent links
       , swDht         :: !(Map.Map HashDistance Bucket)

       -- definitive stores for various mutable things
       , swIndex       :: !(Map.Map HashName HashContainer)
       , swIndexChans  :: !(Map.Map Uid TChan) -- all channels
       , swIndexCrypto :: !(Map.Map String Crypto)
       , swIndexLines  :: !(Map.Map String HashName)
       , swIndexSeeks  :: !(Map.Map HashName Seek)

       , swHandler     :: !(Maybe (HashName -> TeleHash ())) -- called w/ a hn that has no key info

       , swH      :: !(Maybe SocketHandle)
       , swChan   :: !(Maybe (Chan Signal))
       , swSender :: !(LinePacket -> SockAddr -> TeleHash ())

       -- extensions
       , swThtp       :: !(Maybe Thtp)
       , swIndexChat  :: !(Map.Map ChatId Chat)
       , swIndexChatR :: !(Map.Map ChatRId ChatR)
       , swLink       :: !(Maybe Link)

       -- horrible, belongs elswhere
       , swCurrentChat :: !(Maybe ChatId)

       , swRNG  :: !SystemRNG
       , swTick :: !Integer -- unix epoch
       }
     deriving Show

instance Show (HashName -> TeleHash ()) where
  show _ = "(HashName -> TeleHash ())"

instance Show (Chan Signal) where
  show _ = "(Chan Signal)"

instance Show SystemRNG where
  show _ = "SystemRNG"

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
  , hChanOut  :: !ChannelId -- used to allocated next channel id
  , hCrypto   :: !(Maybe Crypto)
  , hPaths    :: !(Map.Map PathJson Path)
  , hLast     :: !(Maybe PathJson)
  , hChans    :: !(Map.Map ChannelId Uid)
  , hOnopen   :: !(Maybe TxTelex)
  , hParts    :: !(Maybe Parts)
  , hIsSeed   :: !Bool
  , hLinkAge  :: !(Maybe ClockTime)
  , hLinkChan :: !(Maybe Uid)
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
newHashContainer :: HashName -> HashContainer
newHashContainer hn = H { hHashName = hn
                        , hCsid = ""
                        , hChanOut = nullChannelId
                        , hCrypto = Nothing
                        , hPaths = Map.empty
                        , hLast = Nothing
                        , hChans = Map.empty
                        , hOnopen = Nothing
                        , hParts = Nothing
                        , hIsSeed = False
                        , hLinkAge = Nothing
                        , hLinkChan = Nothing
                        }
type HashDistance = Int

type PathPriority = Int

-- TODO: provide custom Eq instance, checking core vals only
data Path = Path
      { pType       :: !PathType
      , pJson       :: !PathJson -- NOTE: This is the primary key for the path
      , pId         :: !(Maybe HashName) -- local
      , pAtIn       :: !(Maybe ClockTime)
      , pAtOut      :: !(Maybe ClockTime)
      , pBridgeChan :: !(Maybe Uid) -- possibly bridge via the given
                                    -- channel. This field perhaps
                                    -- belongs on the switch, as per
                                    -- thjs
      } deriving (Show,Eq)

{-
typedef struct path_struct
{
  char type[12];
  char *json;
  char *id;
  char ip[46];
  uint16_t port;
  unsigned long atIn, atOut;
} *path_t;
-}


instance Ord Path where
  compare p1 p2 = compare (pJson p1) (pJson p2)

instance Aeson.ToJSON Path where
  toJSON p = Aeson.toJSON (pJson p)

-- ---------------------------------------------------------------------

pathFromPathJson :: PathJson -> Path
pathFromPathJson pj
  = Path { pType = pjsonType pj
         , pJson = pj
         , pId = Nothing
         , pAtIn = Nothing
         , pAtOut = Nothing
         , pBridgeChan = Nothing
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

-- nullPath = pathFromPathJson (PWebRtc (PathWebRtc "*null*"))

-- nullPathId = PId (-1) -- horrible, need better way of doing this
nullPathJson :: PathJson
nullPathJson = PNone

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

type ChannelHandler = (Uid -> TeleHash ())

data CArg = CArgNone | CArgTx TxTelex | CArgRx RxTelex | CArgChatR ChatRId
          | CArgSeek Seek | CArgPath PathJson

          deriving (Show)

instance Show ChannelHandler where
  show _ = "ChannelHandler"

data TChan = TChan
  { chId       :: !ChannelId -- also hexid,uid
  , chUid      :: !Uid -- Switch wide unique Id. pk.
  , chTo       :: !HashName
  , chType     :: !String
  , chReliable :: !Int
  , chState    :: !ChannelState
  , chLast     :: !(Maybe PathJson)
  , chNext     :: !(Maybe ChannelId)
  , chIn       :: ![RxTelex] -- queue of incoming messages
  , chNotes    :: ![TxTelex]
  , chHandler  :: !(Maybe ChannelHandler) -- auto-fire callback
  , chArg      :: !CArg
  , chSeq      :: !(Maybe Seq)
  , chMiss     :: !(Maybe Miss)
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

data Seq = Seq
  { seId     :: !Int
  , seNextIn :: !Int
  , seSeen   :: !Int
  , seAcked  :: !Int
  , seIn     :: !(Map.Map Int RxTelex) -- indexed by seq
  } deriving Show

{-
typedef struct seq_struct
{
  uint32_t id, nextin, seen, acked;
  packet_t *in;
} *seq_t;
-}

data Miss = Miss
  { mNextAck :: !Int
  , mOut     :: !(Map.Map Int TxId)
  , mPackets :: !(Map.Map TxId TxTelex)
  } deriving Show
{-
typedef struct miss_struct
{
  uint32_t nextack;
  packet_t *out;
} *miss_t;
-}

-- |channel id is a positive number from 1 to 4,294,967,295 (UINT32)
data ChannelId = CID Int deriving (Eq,Show,Ord)

nullChannelId :: ChannelId
nullChannelId = CID 0

unChannelId :: ChannelId -> Int
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

instance Aeson.FromJSON Parts where
  parseJSON (Aeson.Object v) = do
    return $ map (\(k,String val) -> (Text.unpack k,Text.unpack val)) $ HM.toList v
  parseJSON j = error $ "no parseJSON (Parts instance) for " ++ show j

instance Aeson.ToJSON Parts where
  -- toJSON from = Aeson.String $ Text.pack $ "{" ++ (intercalate "," $ map (\(k,v) ->  "\"" ++ k ++ "\":\"" ++ v ++ "\"") from) ++ "}"
  toJSON from = Aeson.Object $ HM.fromList $ map (\(k,v) -> (Text.pack k,Aeson.String $ Text.pack v)) from

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
                    } deriving Show

instance ToJSON OpenizeInner where
  toJSON (OpenizeInner (TOD at _) to from line)
    = object ["at" .= at
             ,"to" .= unHN to
             ,"from" .= from
             ,"line" .= line
             ]

instance FromJSON OpenizeInner where
  parseJSON (Aeson.Object v) = do
    atVal <- v .: "at"
    to <-    v .: "to"
    from <-  v .: "from"
    line <-  v .: "line"
    let jsAt = jsAtToClockTime atVal
    return $ OpenizeInner jsAt (HN to) from line
  parseJSON _ = mzero

jsAtToClockTime :: Aeson.Value -> ClockTime
jsAtToClockTime (Aeson.Number atVal)
  = TOD (round (atVal / 1000)) (((round  atVal) `mod` 1000) * (10::Integer) ^ (9::Integer))
jsAtToClockTime v = error $ "jsAtToClockTime expecting Aeson.Number, got" ++ show v

-- ---------------------------------------------------------------------

data DeOpenizeResult = DeOpenizeVerifyFail
                     | DeOpenize { doLinePub :: !PublicKey
                                 , doKey     :: !BC.ByteString
                                 , doJs      :: !Aeson.Value
                                 , doCsid    :: !String
                                 }
                     deriving (Show)

-- ---------------------------------------------------------------------

-- |The information carried in a link reply
data LinkReply = LinkReplyEnd
               | LinkReplyErr String
               | LinkReplyKeepAlive !Bool
               | LinkReplyNormal
                    { lrSeed :: !Bool
                    , lrSee  :: ![String]
                    } deriving Show

-- | Test ToJSON LinkReply
--
-- >>> Aeson.encode (LinkReplyEnd)
-- "{\"end\":true}"
--
-- >>> Aeson.encode (LinkReplyErr "we have an error")
-- "{\"err\":\"we have an error\"}"
--
-- >>> Aeson.encode (LinkReplyKeepAlive True)
-- "{\"seed\":true}"
--
-- >>> Aeson.encode (LinkReplyNormal False ["hasha","hashb"])
-- "{\"seed\":false,\"see\":[\"hasha\",\"hashb\"]}"

instance ToJSON LinkReply where
  toJSON (LinkReplyEnd)
    = object ["end" .= True
             ]
  toJSON (LinkReplyErr err)
    = object ["err" .= err
             ]
  toJSON (LinkReplyKeepAlive isSeed)
    = object ["seed" .= isSeed
             ]
  toJSON (LinkReplyNormal isSeed see)
    = object ["seed" .= isSeed
             ,"see"  .= see
             ]

-- | Test FromJSON LinkReply
--
-- >>> let test s = Aeson.decode $ cbsTolbs $ BC.pack s :: Maybe LinkReply
--
-- >>> test "{\"end\":\"problem\", \"c\":1,\"seed\":true,\"see\":[\"blah\",\"baz\"]}"
-- Just LinkReplyEnd
--
-- >>> test "{\"err\":\"problem\", \"c\":1,\"seed\":true,\"see\":[\"blah\",\"baz\"]}"
-- Just (LinkReplyErr "problem")
--
-- >>> test "{\"c\":1,\"seed\":false}"
-- Just (LinkReplyKeepAlive False)
--
-- >>> test "{\"c\":1,\"seed\":true,\"see\":[\"blah\",\"baz\"]}"
-- Just (LinkReplyNormal {lrSeed = True, lrSee = ["blah","baz"]})

instance FromJSON LinkReply where
  parseJSON (Aeson.Object v) = do
    if HM.member "err" v
      then do
        errVal <- (v .: "err")
        return $ LinkReplyErr errVal
      else if HM.member "end" v
        then return LinkReplyEnd
        else do
          seedVal <- v .: "seed"
          if HM.member "see" v
            then do
              seeVal  <- v .: "see"
              return $ LinkReplyNormal seedVal seeVal
            else return $ LinkReplyKeepAlive seedVal
  parseJSON _ = mzero

-- ---------------------------------------------------------------------

data LinedState = LineNone | Lined | LineReset
                deriving (Eq,Show)

data Crypto = Crypto
  { cCsid      :: !String
  , cPart      :: !Hash
  , cIsPrivate :: !Bool
  , cLined     :: !LinedState
  , cKeyLen    :: !Int
  , cAtOut     :: !ClockTime
  , cAtIn      :: !(Maybe ClockTime)
  , cLineOut   :: !BC.ByteString
  , cLineIn    :: !BC.ByteString
  , cLineHex   :: !String
  , cKey       :: !BC.ByteString
  , cCs        :: !Crypt1a
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

data Crypt1a = Crypt1a
          { cs1aIdPrivate   :: !(Maybe PrivateKey)
          , cs1aIdPublic    :: !PublicKey
          , cs1aLinePrivate :: !PrivateKey    -- was hEcc
          , cs1aLinePublic  :: !PublicKey     -- was hEcc
          , cs1aSeq         :: !Word32        -- was hLineIV
          , cs1aKeyOut      :: !(Maybe BC.ByteString) -- was hEncKey
          , cs1aKeyIn       :: !(Maybe BC.ByteString) -- was hDecKey
          } deriving Show

{-
typedef struct crypt_1a_struct
{
  uint8_t id_private[uECC_BYTES], id_public[uECC_BYTES *2], line_private[uECC_BYTES], line_public[uECC_BYTES *2];
  uint32_t seq;
  unsigned char keyOut[16], keyIn[16];
} *crypt_1a_t;

-}

-- =====================================================================
-- Extension related types

data Thtp = Thtp { thIndex :: !(Map.Map String TxTelex)
                 -- , thGlob  :: !(Map.Map String TxTelex)
                 , thGlob  :: ![TxTelex]
                 } deriving (Show)

data OkFail = Ok | Fail deriving (Show,Eq,Ord)

-- ---------------------------------------------------------------------
-- Chat

data ChatId = ChatId { ciEndpoint   :: !String
                     , ciOriginator :: !(Maybe HashName)
                     } deriving (Eq,Show,Ord)

chatIdToString :: ChatId -> String
chatIdToString (ChatId ep Nothing) = ep
chatIdToString (ChatId ep (Just (HN o))) = ep ++ "@" ++ o

data ChatHash = CH String deriving (Eq,Show,Ord)

unCH :: ChatHash -> String
unCH (CH s) = s

data ChatPerm = PermBlocked | PermReadOnly | PermAllowed
     deriving (Eq,Show,Ord)

data Chat = Chat
     { ecEp     :: !String
     , ecId     :: !ChatId
     , ecIdHash :: !ChatHash
     , ecOrigin :: !HashName
     , ecHub    :: !Uid -- hub channel
     , ecRHash  :: !ChatHash
     , ecLocal  :: !Bool
     , ecSeed   :: !ChatHash
     , ecSeq    :: !Word16
     , ecRoster :: !(Map.Map String String) -- name (possibly hashname) to id
     , ecConn   :: !(Map.Map HashName Uid) -- For channels
     , ecLog    :: !(Map.Map String TxTelex)
     , ecMsgs   :: ![RxTelex]
     , ecJoin   :: !(Maybe String)
     , ecSent   :: !(Maybe String)
     , ecAfter  :: !(Maybe String)
     } deriving Show

{-
typedef struct chat_struct
{
  char ep[32+1], id[32+1+64+1], idhash[9];
  hn_t origin;
  switch_t s;
  chan_t hub;
  char rhash[9];
  uint8_t local, seed[4];
  uint16_t seq;
  packet_t roster;
  xht_t conn, log;
  packet_t msgs;
  char *join, *sent, *after;
} *chat_t;
-}

data ChatRId = CR Int deriving (Show,Eq,Ord)

data ChatR = ChatR
      { ecrId     :: !ChatRId
      , ecrChat   :: !ChatId
      , ecrIn     :: !RxTelex
      , ecrJoined :: !Bool
      , ecrOnline :: !Bool
      , ecrChan   :: !Uid -- convenience/tracking
      } deriving Show

{-
// chatr is just per-chat-channel state holder
typedef struct chatr_struct
{
  chat_t chat;
  packet_t in;
  int joined;
  int online;
} *chatr_t;
-}

-- ---------------------------------------------------------------------
-- Link

data Link = Link
  { lMeshing :: !Bool
  , lMeshed  :: !Bucket
  -- only used when seeding
  , lSeeding :: !Bool
  , lBuckets :: ![Bucket]
  , lLinks   :: !(Map.Map String String)
  } deriving Show

{-
typedef struct link_struct
{
  // only used when meshing
  int meshing;
  bucket_t meshed;
  // only used when seeding
  int seeding;
  bucket_t *buckets;
  xht_t links;
} *link_t;
-}

-- ---------------------------------------------------------------------
-- Seek

data Seek = Seek
  { seekId     :: !HashName
  , seekActive :: !Int
  , seekNote   :: !(Maybe RxTelex)
  , seekSentAt :: !(Maybe ClockTime)
  } deriving Show

{-
typedef struct seek_struct
{
  hn_t id;
  int active;
  packet_t note;
} *seek_t;
-}

-- =====================================================================
-- ---------------------------------------------------------------------
-- This stuff should probably be thrown away
-- ---------------------------------------------------------------------
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
{-
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
-}
data PublicKey = Public1a ECDSA.PublicKey deriving Show
data PrivateKey = Private1a ECDSA.PrivateKey deriving Show

-- ---------------------------------------------------------------------
{-
data CSet = CS
  { csLoadkey   :: String -> Maybe String -> TeleHash (Maybe HashCrypto)
  , csOpenize   :: HashContainer -> OpenizeInner -> TeleHash LinePacket
  , csDeopenize :: NetworkPacket -> TeleHash DeOpenizeResult
  , csOpenLine  :: HashContainer ->  DeOpenizeResult -> TeleHash ()
  , csDelineize :: HashContainer -> NetworkTelex -> TeleHash (Either String RxTelex)
  }
-}

-- ---------------------------------------------------------------------

data SeedInfo = SI
  { sId       :: !String
  , sAdmin    :: !String
  , sPaths    :: ![PathJson]
  , sParts    :: !Parts
  , sKeys     :: ![(String,String)] -- crypto scheme name, crypto key
  , sIsBridge :: !Bool
  } deriving Show

-- emptySeed = SI "" "" [] [] [] False

instance Aeson.FromJSON SeedInfo where
  parseJSON (Aeson.Object v) = do
     admin <- v .: "admin"
     -- let admin = "blah"

     paths <- v .: "paths"
     -- let paths = []

     parts <- v .: "parts"
     -- let parts = []

     keys  <- v .: "keys"
     -- let keys = []

     return SI { sId = "foo"
               , sAdmin = admin
               , sPaths = paths
               , sParts = parts
               , sKeys = keys
               , sIsBridge = False
               }
  parseJSON x = error $ "parseJSON (SeedInfo instance) unexpected Aeson value:" ++ show x

instance Aeson.FromJSON [SeedInfo] where
  parseJSON (Aeson.Object v) = do
     let (idval,_vv) = head $ HM.toList v
     seed <- v .: idval
     return [seed]
{-
    if HM.size v > 0
      then do
        let (idval,vv) = head $ HM.toList v
        -- parseJSON vv
        fail $ "AZ got :" ++ show vv
      else mzero
-}
  parseJSON _ = mzero

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
    SocketHandle {slSocket :: !Socket
                 --, slAddress :: SockAddr
                 } deriving (Eq,Show)

instance Show (LinePacket -> SockAddr -> TeleHash ()) where
  show _ = "(LinePacket -> SockAddr -> TeleHash ())"

