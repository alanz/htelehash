{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TeleHash.New.Chan
  (
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

-- Based on the telehash-c version

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

data Chan = Chan
  { chId       :: !ChannelId -- also hexid,uid
  -- need pointer back to the switch
  , chTo       :: !HashName
  , chType     :: !String
  , chReliable :: !Bool
  , chState    :: !ChannelState
  , chPath     :: !PathId
  , chNext     :: !ChannelId
  , chIn       :: !(Maybe Packet)
  , chInEnd    :: !(Maybe Packet)
  , chNote     :: !(Maybe Packet)
  , chHandler  :: !ChannelHandler -- auto-fire callback
  } deriving Show

-- ---------------------------------------------------------------------
{-
// kind of a macro, just make a reliable channel of this type to this hashname
chan_t chan_start(struct switch_struct *s, char *hn, char *type);
-}

-- |kind of a macro, just make a reliable channel of this type to this hashname
chan_start :: HashName -> String -> TeleHash ()
chan_start hn typ = do
  c <- chan_new hn typ 0
  window <- gets swWindow
  chan_reliable c window

{-
// kind of a macro, just make a reliable channel of this type to this hashname
chan_t chan_start(switch_t s, char *hn, char *type)
{
  chan_t c;
  if(!s || !hn) return NULL;
  c = chan_new(s, hn_gethex(s->index,hn), type, 0);
  return chan_reliable(c, s->window);
}
-}

-- ---------------------------------------------------------------------

{-
// new channel, pass id=0 to create an outgoing one
chan_t chan_new(struct switch_struct *s, struct hn_struct *to, char *type, uint32_t id);
void chan_free(chan_t c);

// configures channel as a reliable one, must be in STARTING state, is max # of packets to buffer before backpressure
chan_t chan_reliable(chan_t c, int window);

// resets channel state for a hashname
void chan_reset(struct switch_struct *s, struct hn_struct *to);

// returns existing or creates new and adds to from
chan_t chan_in(struct switch_struct *s, struct hn_struct *from, packet_t p);

// create a packet ready to be sent for this channel, returns NULL for backpressure
packet_t chan_packet(chan_t c);

// pop a packet from this channel to be processed, caller must free
packet_t chan_pop(chan_t c);

// flags channel as gracefully ended, optionally adds end to packet
chan_t chan_end(chan_t c, packet_t p);

// immediately fails/removes channel, if err tries to send message
chan_t chan_fail(chan_t c, char *err);

// get the next incoming note waiting to be handled
packet_t chan_notes(chan_t c);

// stamp or create (if NULL) a note as from this channel
packet_t chan_note(chan_t c, packet_t note);

// send the note back to the creating channel, frees note
int chan_reply(chan_t c, packet_t note);

// internal, receives/processes incoming packet
void chan_receive(chan_t c, packet_t p);

// smartly send based on what type of channel we are
void chan_send(chan_t c, packet_t p);

// optionally sends reliable channel ack-only if needed
void chan_ack(chan_t c);

// add/remove from switch processing queue
void chan_queue(chan_t c);
void chan_dequeue(chan_t c);

// just add ack/miss
packet_t chan_seq_ack(chan_t c, packet_t p);

// new sequenced packet, NULL for backpressure
packet_t chan_seq_packet(chan_t c);

// buffers packets until they're in order, 1 if some are ready to pop
int chan_seq_receive(chan_t c, packet_t p);

// returns ordered packets for this channel, updates ack
packet_t chan_seq_pop(chan_t c);

void chan_seq_init(chan_t c);
void chan_seq_free(chan_t c);

// tracks packet for outgoing, eventually free's it, 0 ok or 1 for full/backpressure
int chan_miss_track(chan_t c, uint32_t seq, packet_t p);

// buffers packets to be able to re-send
void chan_miss_send(chan_t c, packet_t p);

// looks at incoming miss/ack and resends or frees
void chan_miss_check(chan_t c, packet_t p);

void chan_miss_init(chan_t c);
void chan_miss_free(chan_t c);

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

