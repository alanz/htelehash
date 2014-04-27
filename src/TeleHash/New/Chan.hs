{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TeleHash.New.Chan
  (
    chan_start
  , chan_new
  , chan_free
  , chan_reliable
  , chan_reset
  , chan_in
  , chan_packet
  , chan_pop
  , chan_end
  , chan_fail
  , chan_notes
  , chan_note
  , chan_reply
  , chan_receive
  , chan_send
  , chan_ack
  , chan_queue
  , chan_dequeue
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
import TeleHash.New.Path
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
              , chLast     = Nothing
              , chNext     = Nothing
              , chIn       = []
              , chNotes    = []
              , chHandler  = Nothing
              }
  withHN toHn (\hc -> hc { hChans = Map.insert cid chan (hChans hc) })

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

  rmChanFromHn (chTo chan) (chId chan)
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
  sw <- get
  to1 <- getHN toHn
  let base = if (swId sw) < (hHashName to1)
               then 1 else 2
  withHN toHn $ \hc -> if (hChanOut hc == CID 0)
                        then hc {hChanOut = CID base}
                        else hc
  -- fail any existing chans from them
  withHNM toHn $ \hc -> do
    forM_ (Map.elems $ hChans hc) $ \ch -> do
      if channelSlot (chId ch) == channelSlot (CID base)
        then return ()
        else void $ chan_fail ch Nothing
    hc' <- getHN (hHashName hc)
    return hc'
{-
void chan_reset(switch_t s, hn_t to)
{
  uint8_t base = (strncmp(s->id->hexname,to->hexname,64) > 0) ? 1 : 2;
  if(!to->chanOut) to->chanOut = base;
  // fail any existing chans from them
  xht_walk(to->chans, &walkend, (void*)&base);
}
void walkend(xht_t h, const char *key, void *val, void *arg)
{
  uint8_t base = *(uint8_t*)arg;
  chan_t c = (chan_t)val;
  if(c->id % 2 != base % 2) chan_fail(c,NULL);
}

-}

-- ---------------------------------------------------------------------

-- |returns existing or creates new and adds to from
chan_in :: HashName -> RxTelex -> TeleHash (Maybe TChan)
chan_in hn p = do
  case getRxTelexChannelId p of
    Nothing -> do
      logT $ "chan_in:no channel id"
      assert False undefined
    Just cid -> do
      mchan <- getChanFromHn hn cid
      case mchan of
        Just chan -> return (Just chan)
        Nothing -> do
          from <- getHN hn
          let mtyp = getRxTelexType p
          if (mtyp == Nothing
             || channelSlot cid == channelSlot (hChanOut from))
            then return Nothing
            else do
              chan <- chan_new hn (gfromJust "chan_in" mtyp) (Just cid)
              return (Just chan)

{-
chan_t chan_in(switch_t s, hn_t from, packet_t p)
{
  chan_t c;
  unsigned long id;
  char hexid[9], *type;
  if(!from || !p) return NULL;

  id = strtol(packet_get_str(p,"c"), NULL, 10);
  util_hex((unsigned char*)&id,4,(unsigned char*)hexid);
  c = xht_get(from->chans, hexid);
  if(c) return c;

  type = packet_get_str(p, "type");
  if(!type || id % 2 == from->chanOut % 2) return NULL;

  return chan_new(s, from, type, id);
}

-}

-- ---------------------------------------------------------------------

-- |create a packet ready to be sent for this channel, returns Nothing for backpressure
chan_packet :: TChan -> TeleHash (Maybe TxTelex)
chan_packet chan = do
  if chState chan == ChanEnded
    then return Nothing
    else do
      mp <- if chReliable chan
              then chan_seq_packet chan
              else return $ Just (packet_new (chTo chan))
      case mp of
        Nothing -> return Nothing
        Just p -> do
          let p1 = p { tTo = chTo chan }
          alive <- path_alive (chLast chan)
          let p2 = if alive
                     then p1 { tOut = gfromJust "chan_packet" $ chLast chan }
                     else p1
              p3 = if chState chan == ChanStarting
                     then packet_set_str p2 "type" (chType chan)
                     else p2
              p4 = packet_set_int p3 "c" (chId chan)
          return (Just p4)

{-
// create a packet ready to be sent for this channel
packet_t chan_packet(chan_t c)
{
  packet_t p;
  if(!c || c->state == ENDED) return NULL;
  p = c->reliable?chan_seq_packet(c):packet_new();
  if(!p) return NULL;
  p->to = c->to;
  if(path_alive(c->last)) p->out = c->last;
  if(c->state == STARTING)
  {
    packet_set_str(p,"type",c->type);
  }
  packet_set_int(p,"c",c->id);
  return p;
}

-}
-- ---------------------------------------------------------------------

{-
// pop a packet from this channel to be processed, caller must free
packet_t chan_pop(chan_t c);
-}
-- TODO: use channel uid, rather than direct
chan_pop :: TChan -> TeleHash (Maybe RxTelex)
chan_pop chan = do
  if (chReliable chan)
    then chan_seq_pop chan
    else do
      if null (chIn chan)
        then return Nothing
        else do
          let p = head (chIn chan)
          let c2 = chan { chIn = tail (chIn chan) }
          putChan c2
          return (Just p)

{-
packet_t chan_pop(chan_t c)
{
  packet_t p;
  if(!c) return NULL;
  if(c->reliable) return chan_seq_pop(c);
  if(!c->in) return NULL;
  p = c->in;
  c->in = p->next;
  if(!c->in) c->inend = NULL;
  return p;
}

-}
-- ---------------------------------------------------------------------

-- flags channel as gracefully ended, optionally adds end to packet
chan_end :: TChan -> Maybe TxTelex -> TeleHash (TChan,Maybe TxTelex)
chan_end chan p = do
  logT $ "channel end " ++ show (chId chan)
  let
    pret = case p of
             Nothing -> Nothing
             Just pkt -> Just (packet_set pkt "end" True)
    c2 = chan {chState = ChanEnded }
  putChan c2
  chan_queue c2
  return (c2,pret)

{-
chan_t chan_end(chan_t c, packet_t p)
{
  DEBUG_PRINTF("channel end %d",c->id);
  if(p) packet_set(p,"end","true",4);
  // if(c->reliable) TODO set to ENDING, add timer for cleanup and then queue for free
  c->state = CHAN_ENDED;
  chan_queue(c);
  return c;
}
-}

-- ---------------------------------------------------------------------

-- |immediately fails/removes channel, if err tries to send message
chan_fail :: TChan -> Maybe String -> TeleHash TChan
chan_fail c merr = do
  logT $ "channel fail " ++ show (chId c,merr)
  case merr of
    Just err -> do
      if chState c /= ChanEnded
        then do
          e <- chan_packet c
          e2 <- packet_set_str e "err" err
          chan_send c e2
        else return ()
    Nothing -> return ()
  assert False undefined

{-
// immediately fails/removes channel, if err tries to send message
chan_t chan_fail(chan_t c, char *err)
{
  packet_t e;
  DEBUG_PRINTF("channel fail %d %s",c->id,err);
  if(err && c->state != CHAN_ENDED && (e = chan_packet(c)))
  {
    packet_set_str(e,"err",err);
    chan_send(c,e);
  }
  // no grace period for reliable
  c->state = CHAN_ENDED;
  xht_set(c->to->chans,(char*)c->hexid,NULL);
  chan_queue(c);
  return c;
}

-}

-- ---------------------------------------------------------------------

-- |get the next incoming note waiting to be handled
chan_notes :: TChan -> TeleHash (Maybe RxTelex)
chan_notes c = do
  if null (chNotes c)
    then return Nothing
    else do
      let r = head (chNotes c)
          c2 = c {chNotes = tail (chNotes c)}
      putChan c2
      return (Just r)

{-
// get the next incoming note waiting to be handled
packet_t chan_notes(chan_t c)
{
  packet_t note;
  if(!c) return NULL;
  note = c->notes;
  if(note) c->notes = note->next;
  return note;
}

-}

-- ---------------------------------------------------------------------

-- |stamp or create (if Nothing) a note as from this channel
chan_note :: TChan -> Maybe RxTelex -> TeleHash RxTelex
chan_note c mnote = do
  let r = case mnote of
           Just n -> n
           Nothing -> packet_new_rx
  let r2 = packet_set_str r ".from" (chUid c)
  return r2

{-
// create a new note tied to this channel
packet_t chan_note(chan_t c, packet_t note)
{
  if(!note) note = packet_new();
  packet_set_str(note,".from",(char*)c->uid);
  return note;
}
-}
-- ---------------------------------------------------------------------

-- |send the note back to the creating channel, frees note
chan_reply :: TChan -> RxTelex -> TeleHash Int
chan_reply c note = do
  assert False undefined

{-
// send this note back to the sender
int chan_reply(chan_t c, packet_t note)
{
  char *from;
  if(!c || !(from = packet_get_str(note,".from"))) return -1;
  packet_set_str(note,".to",from);
  packet_set_str(note,".from",(char*)c->uid);
  return switch_note(c->s,note);
}
-}

-- ---------------------------------------------------------------------

-- |internal, receives/processes incoming packet
chan_receive :: TChan -> RxTelex -> TeleHash ()
chan_receive c p = do
  logT $ "channel in " ++ show (chId c,p)
  if chState c == ChanEnded
    then return ()
    else do
      let
        c2 = if chState c == ChanStarting
               then c {chState = ChanOpen}
               else c
        c3 = case packet_get_str p "end" of
               Nothing -> c2
               Just _  -> c2 {chState = ChanEnding }
        c4 = case packet_get_str p "err" of
               Nothing -> c3
               Just _  -> c3 {chState = ChanEnding }
      putChan c4
      if (chReliable c4)
        then do
          chan_miss_check c4 p
          r <- chan_seq_receive c4 p
          if r
            then return () -- queued, nothing more to do
            else chan_queue c4
          assert False undefined
        else do
          -- add to the end of the raw packet queue
          let c5 = c4 { chIn = (chIn c4) ++ [p]}
          putChan c5
          -- queue for processing
          chan_queue c5
{-
// internal, receives/processes incoming packet
void chan_receive(chan_t c, packet_t p)
{
  if(!c || !p) return;
  DEBUG_PRINTF("channel in %d %.*s",c->id,p->json_len,p->json);
  if(c->state == CHAN_ENDED) return (void)packet_free(p);
  if(c->state == CHAN_STARTING) c->state = CHAN_OPEN;
  if(util_cmp(packet_get_str(p,"end"),"true") == 0) c->state = CHAN_ENDING;
  if(packet_get_str(p,"err")) c->state = CHAN_ENDED;

  if(c->reliable)
  {
    chan_miss_check(c,p);
    if(!chan_seq_receive(c,p)) return; // queued, nothing to do
  }else{
    // add to the end of the raw packet queue
    if(c->inend)
    {
      c->inend->next = p;
      c->inend = p;
      return;
    }
    c->inend = c->in = p;
  }

  // queue for processing
  chan_queue(c);
}
-}

-- ---------------------------------------------------------------------

-- |smartly send based on what type of channel we are
chan_send :: TChan -> TxTelex -> TeleHash ()
chan_send c p = do
  logT $ "channel out " ++ show (chId c,p)
  p2 <- if chReliable c
          then do
            p' <- packet_copy p
            return p'
          else return p
  switch_send p2

{-
// smartly send based on what type of channel we are
void chan_send(chan_t c, packet_t p)
{
  if(!p) return;
  if(!c) return (void)packet_free(p);
  DEBUG_PRINTF("channel out %d %.*s",c->id,p->json_len,p->json);
  if(c->reliable) p = packet_copy(p); // miss tracks the original p = chan_packet()
  switch_send(c->s,p);
}
-}

-- ---------------------------------------------------------------------

-- |optionally sends reliable channel ack-only if needed
chan_ack :: TChan -> TeleHash ()
chan_ack c = do
  if not (chReliable c)
    then return ()
    else do
      mp <- chan_seq_ack c Nothing
      case mp of
        Nothing -> return ()
        Just p -> do
          logT $ "channel ack " ++ show (chId c,p)
          switch_send p

{-
// optionally sends reliable channel ack-only if needed
void chan_ack(chan_t c)
{
  packet_t p;
  if(!c || !c->reliable) return;
  p = chan_seq_ack(c,NULL);
  if(!p) return;
  DEBUG_PRINTF("channel ack %d %.*s",c->id,p->json_len,p->json);
  switch_send(c->s,p);
}
-}

-- ---------------------------------------------------------------------

-- |add to switch processing queue
chan_queue :: TChan -> TeleHash ()
chan_queue c = do
  -- add to switch queue
  queueChan c

{-
// add to processing queue
void chan_queue(chan_t c)
{
  chan_t step;
  // add to switch queue
  step = c->s->chans;
  if(c->next || step == c) return;
  while(step && (step = step->next)) if(step == c) return;
  c->next = c->s->chans;
  c->s->chans = c;
}
-}

-- ---------------------------------------------------------------------
{-
// remove from switch processing queue
void chan_dequeue(chan_t c);
-}

-- |remove from switch processing queue
chan_dequeue :: TChan -> TeleHash ()
chan_dequeue c = do
  dequeueChan c

{-
// remove from processing queue
void chan_dequeue(chan_t c)
{
  chan_t step = c->s->chans;
  if(step == c)
  {
    c->s->chans = c->next;
    c->next = NULL;
    return;
  }
  step = c->s->chans;
  while(step) step = (step->next == c) ? c->next : step->next;
  c->next = NULL;
}
-}
-- ---------------------------------------------------------------------

-- |add ack, miss to any packet
chan_seq_ack :: TChan -> Maybe TxTelex -> TeleHash (Maybe TxTelex)
chan_seq_ack = assert False undefined

{-
// add ack, miss to any packet
packet_t chan_seq_ack(chan_t c, packet_t p)
{
  char *miss;
  int i,max;
  seq_t s = (seq_t)c->seq;

  // determine if we need to ack
  if(!s->nextin) return p;
  if(!p && s->acked && s->acked == s->nextin-1) return NULL;

  if(!p) p = chan_packet(c); // ack-only packet
  s->acked = s->nextin-1;
  packet_set_int(p,"ack",(int)s->acked);

  // check if miss is not needed
  if(s->seen < s->nextin || s->in[0]) return p;
  
  // create miss array, c sux
  max = (c->reliable < 10) ? c->reliable : 10;
  miss = malloc(3+(max*11)); // up to X uint32,'s
  memcpy(miss,"[\0",2);
  for(i=0;i<max;i++) if(!s->in[i]) sprintf(miss+strlen(miss),"%d,",(int)s->nextin+i);
  if(miss[strlen(miss)-1] == ',') miss[strlen(miss)-1] = 0;
  memcpy(miss+strlen(miss),"]\0",2);
  packet_set(p,"miss",miss,strlen(miss));
  free(miss);
  return p;
}
-}
-- ---------------------------------------------------------------------
{-
// new sequenced packet, NULL for backpressure
packet_t chan_seq_packet(chan_t c);
-}
-- |new sequenced packet, NULL for backpressure
chan_seq_packet :: TChan -> TeleHash (Maybe TxTelex)
chan_seq_packet = assert False undefined

-- ---------------------------------------------------------------------
{-
// buffers packets until they're in order, 1 if some are ready to pop
int chan_seq_receive(chan_t c, packet_t p);
-}
-- |buffers packets until they're in order, 1 if some are ready to pop
chan_seq_receive :: TChan -> RxTelex -> TeleHash Bool
chan_seq_receive = assert False undefined

-- ---------------------------------------------------------------------
{-
// returns ordered packets for this channel, updates ack
packet_t chan_seq_pop(chan_t c);
-}
chan_seq_pop :: TChan -> TeleHash (Maybe RxTelex)
chan_seq_pop = assert False undefined

-- ---------------------------------------------------------------------
{-
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
-}

-- ---------------------------------------------------------------------
{-
// looks at incoming miss/ack and resends or frees
void chan_miss_check(chan_t c, packet_t p);
-}
-- |looks at incoming miss/ack and resends or frees
chan_miss_check :: TChan -> RxTelex -> TeleHash ()
chan_miss_check = assert False undefined

-- ---------------------------------------------------------------------
{-
void chan_miss_init(chan_t c);
-}

-- ---------------------------------------------------------------------
{-
void chan_miss_free(chan_t c);
-}

chan_miss_free = assert False undefined

-- ---------------------------------------------------------------------



