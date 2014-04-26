{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TeleHash.New.Chan
  (
    chan_start
  , chan_new
  , chan_free
  , chan_reliable
  , chan_reset
  , 
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
import TeleHash.New.Switch
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

-- Based on the telehash-c version

-- ---------------------------------------------------------------------

-- |kind of a macro, just make a reliable channel of this type to this hashname
chan_start :: HashName -> String -> TeleHash ()
chan_start hn typ = do
  c <- chan_new hn typ Nothing
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

-- |new channel, pass Nothing for channel id to create an outgoing one
chan_new :: HashName -> String -> Maybe ChannelId -> TeleHash TChan
chan_new toHn typ mcid = do
  to <- getHN toHn
  case hChanOut to of
    CID 0 -> return ()
    CID _ -> chan_reset toHn

  -- use new id if none given
  let (cid,cout) =
        case mcid of
          Nothing -> (c,co)
            where c = hChanOut to
                  co = c + 2
          Just c -> (c,hChanOut to)

  logT $ "channel new " ++ show (cid,typ)
  uid <- getNextUid
  let chan = TChan
              { chId       = cid
              , chUid      = uid
              , chTo       = toHn
              , chType     = typ
              , chReliable = False
              , chState    = ChanStarting
              , chPath     = Nothing
              , chNext     = Nothing
              , chIn       = []
              , chInEnd    = Nothing
              , chNotes    = []
              , chHandler  = Nothing
              }
  withHN toHn (\hc -> hc { hChans = Map.insert uid chan (hChans hc) })

  putChan chan
  return chan


{-
chan_t chan_new(switch_t s, hn_t to, char *type, uint32_t id)
{
  chan_t c;
  if(!s || !to || !type) return NULL;

  // use new id if none given
  if(!to->chanOut) chan_reset(s, to);
  if(!id)
  {
    id = to->chanOut;
    to->chanOut += 2;
  }

  DEBUG_PRINTF("channel new %d %s",id,type);
  c = malloc(sizeof (struct chan_struct));
  memset(c,0,sizeof (struct chan_struct));
  c->type = malloc(strlen(type)+1);
  memcpy(c->type,type,strlen(type)+1);
  c->s = s;
  c->to = to;
  c->state = STARTING;
  c->id = id;
  util_hex((unsigned char*)&(s->uid),4,(unsigned char*)c->uid); // switch-wide unique id
  s->uid++;
  util_hex((unsigned char*)&(c->id),4,(unsigned char*)c->hexid);
  if(!to->chans) to->chans = xht_new(17);
  xht_set(to->chans,(char*)c->hexid,c);
  xht_set(s->index,(char*)c->uid,c);
  return c;
}


-}
-- ---------------------------------------------------------------------

chan_free :: TChan -> TeleHash ()
chan_free chan = do
  logT $ "chan_free " ++ show (chId chan)
  -- remove references
  chan_dequeue chan

  rmChanFromHn (chTo chan) (chUid chan)
  rmChan (chUid chan)

  if (chReliable chan)
    then do
      chan_seq_free chan
      chan_miss_free chan
    else return ()

  if null (chIn chan)
    then return ()
    else do
      logT $ "unused packets on channel " ++ show (chId chan)

  if null (chNotes chan)
    then return ()
    else do
      logT $ "unused notes on channel " ++ show (chId chan)

  assert False undefined

{-
void chan_free(chan_t c)
{
  packet_t p;
  // remove references
  DEBUG_PRINTF("channel free %d",c->id);
  chan_dequeue(c);
  if(xht_get(c->to->chans,(char*)c->hexid) == c) xht_set(c->to->chans,(char*)c->hexid,NULL);
  xht_set(c->s->index,(char*)c->uid,NULL);
  if(c->reliable)
  {
    chan_seq_free(c);
    chan_miss_free(c);
  }
  while(c->in)
  {
    DEBUG_PRINTF("unused packets on channel %d",c->id);
    p = c->in;
    c->in = p->next;
    packet_free(p);
  }
  while(c->notes)
  {
    DEBUG_PRINTF("unused notes on channel %d",c->id);
    p = c->notes;
    c->notes = p->next;
    packet_free(p);
  }
  free(c->type);
  free(c);
}
-}


-- ---------------------------------------------------------------------
{-
// configures channel as a reliable one, must be in STARTING state, is max # of packets to buffer before backpressure
chan_t chan_reliable(chan_t c, int window);
-}
chan_reliable = assert False undefined

-- ---------------------------------------------------------------------
-- |resets channel state for a hashname
chan_reset :: HashName -> TeleHash ()
chan_reset toHn = do
  assert False undefined
{-
void chan_reset(switch_t s, hn_t to)
{
  uint8_t base = (strncmp(s->id->hexname,to->hexname,64) > 0) ? 1 : 2;
  if(!to->chanOut) to->chanOut = base;
  // fail any existing chans from them
  xht_walk(to->chans, &walkend, (void*)&base);
}

-}


-- ---------------------------------------------------------------------
{-
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

// add to switch processing queue
void chan_queue(chan_t c);
-}

-- ---------------------------------------------------------------------
{-
// remove from switch processing queue
void chan_dequeue(chan_t c);
-}

-- |remove from switch processing queue
chan_dequeue = assert False undefined

-- ---------------------------------------------------------------------
{-
// just add ack/miss
packet_t chan_seq_ack(chan_t c, packet_t p);

// new sequenced packet, NULL for backpressure
packet_t chan_seq_packet(chan_t c);

// buffers packets until they're in order, 1 if some are ready to pop
int chan_seq_receive(chan_t c, packet_t p);

// returns ordered packets for this channel, updates ack
packet_t chan_seq_pop(chan_t c);

void chan_seq_init(chan_t c);
-}

-- ---------------------------------------------------------------------
{-
void chan_seq_free(chan_t c);
-}
chan_seq_free = assert False undefined

-- ---------------------------------------------------------------------
{-
// tracks packet for outgoing, eventually free's it, 0 ok or 1 for full/backpressure
int chan_miss_track(chan_t c, uint32_t seq, packet_t p);

// buffers packets to be able to re-send
void chan_miss_send(chan_t c, packet_t p);

// looks at incoming miss/ack and resends or frees
void chan_miss_check(chan_t c, packet_t p);

void chan_miss_init(chan_t c);
-}

-- ---------------------------------------------------------------------
{-
void chan_miss_free(chan_t c);
-}

chan_miss_free = assert False undefined

-- ---------------------------------------------------------------------



