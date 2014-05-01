module TeleHash.New.SwitchApi
  (
   -- * Telehash-c api
     switch_send
  , switch_sending
  , switch_receive
  , switch_open

  -- * chan api
  , chan_start
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

import TeleHash.New.Convert
import TeleHash.New.Crypt
import TeleHash.New.Hn
import TeleHash.New.Packet
import TeleHash.New.Path
import TeleHash.New.Paths
import TeleHash.New.Types
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


switch_sending :: TeleHash (Maybe TxTelex)
switch_sending = do
  sw <- get
  case (swOut sw) of
    [] -> return Nothing
    (p:ps) -> do
      put $ sw { swOut = ps
               , swLast = if null ps then Nothing else (swLast sw)
               }
      return (Just p)

{-
packet_t switch_sending(switch_t s)
{
  packet_t p;
  if(!s || !s->out) return NULL;
  p = s->out;
  s->out = p->next;
  if(!s->out) s->last = NULL;
  return p;
}
-}

-- ---------------------------------------------------------------------

switch_receive :: NetworkPacket -> Path -> ClockTime -> TeleHash ()
switch_receive rxPacket path timeNow = do
  -- counterVal <- incPCounter
  let packet = NetworkTelex
                { ntSender = path
                , ntId     = 0 -- counterVal
                , ntAt     = timeNow
                , ntPacket = rxPacket
                }

  case rxPacket of
    OpenPacket b bs -> do
      -- process open packet
      open <- crypt_deopenize rxPacket
      logT $ "DEOPEN " ++ show open
      case open of
        DeOpenizeVerifyFail -> do
          return ()
        _ -> do
          logT $ "receive.deopenize verified ok " -- ++ show open
          let minner = parseJsVal (doJs open) :: Maybe OpenizeInner
          case minner of
            Nothing -> do
              logT $ "switch_receive:invalid inner js:" ++ (BC.unpack $ lbsTocbs $ Aeson.encode $ doJs open)
              return ()
            Just inner -> do
              logT $ "switch_receive:openize:inner=" ++ show inner
              mfrom <- hn_getparts (oiFrom inner)
              case mfrom of
                Nothing -> do
                  logT $ "switch_receive:openize:invalid from" ++ show (oiFrom inner)
                  return ()
                Just hfrom -> do
                  from <- getHN hfrom
                  mlineCrypto <- case hCrypto from of
                                   Nothing -> return Nothing
                                   Just c  -> crypt_line open c inner
                  case mlineCrypto of
                    Nothing -> do
                      -- not new/valid, ignore
                      logT $ "old or invalid open, ignoring"
                      return ()
                    Just lineCrypto -> do
                      -- line is open
                      logT $ "line in "
                      let from2 = from { hCrypto = Just lineCrypto }
                      putHN from2
                      if cLined lineCrypto == LineReset
                        then chan_reset (hHashName from2)
                        else return ()
                      -- xht_set(s->index, (const char*)from->c->lineHex, (void*)from);
                      putHexLine (cLineHex lineCrypto) (hHashName from)
                      inVal <- hn_path (hHashName from) (pJson path)
                      switch_open (hHashName from) inVal -- in case
                      case hOnopen from of
                        Nothing ->  do
                          return ()
                        Just onopen -> do
                          putHN $ from { hOnopen = Nothing }
                          switch_send (onopen { tOut = pJson (gfromJust "onopen" inVal) })
                          return ()
    _ -> do
      logT $ "switch_receive:not processing:" ++ show rxPacket
      assert False undefined

{-

void switch_receive(switch_t s, packet_t p, path_t in)
{
  hn_t from;
  packet_t inner;
  crypt_t c;
  chan_t chan;
  char hex[3];
  char lineHex[33];

  if(!s || !p || !in) return;

  // handle open packets
  if(p->json_len == 1)
  {
    util_hex(p->json,1,(unsigned char*)hex);
    c = xht_get(s->index,hex);
    if(!c) return (void)packet_free(p);
    inner = crypt_deopenize(c, p);
    DEBUG_PRINTF("DEOPEN %d",inner);
    if(!inner) return (void)packet_free(p);

    from = hn_frompacket(s->index, inner);
    if(crypt_line(from->c, inner) != 0) return; // not new/valid, ignore

    // line is open!
    DEBUG_PRINTF("line in %d %s %d %s",from->c->lined,from->hexname,from,from->c->lineHex);
    if(from->c->lined == 1) chan_reset(s, from);
    xht_set(s->index, (const char*)from->c->lineHex, (void*)from);
    in = hn_path(from, in);
    switch_open(s, from, in); // in case we need to send an open
    if(from->onopen)
    {
      packet_t last = from->onopen;
      from->onopen = NULL;
      last->out = in;
      switch_send(s, last);
    }
    return;
  }

  // handle line packets
  if(p->json_len == 0)
  {
    util_hex(p->body, 16, (unsigned char*)lineHex);
    from = xht_get(s->index, lineHex);
    if(from)
    {
      in = hn_path(from, in);
      p = crypt_delineize(from->c, p);
      if(!p)
      {
        DEBUG_PRINTF("invlaid line from %s %s",path_json(in),from->hexname);
        return;
      }

      // route to the channel
      if((chan = chan_in(s, from, p)))
      {
        // if new channel w/ seq, configure as reliable
        if(chan->state == CHAN_STARTING && packet_get_str(p,"seq")) chan_reliable(chan, s->window);
        return chan_receive(chan, p);
      }

      // bounce it!
      if(!packet_get_str(p,"err"))
      {
        packet_set_str(p,"err","unknown channel");
        p->to = from;
        p->out = in;
        switch_send(s, p);
      }else{
        packet_free(p);
      }
      return;
    }
  }

  // handle valid pong responses, start handshake
  if(util_cmp("pong",packet_get_str(p,"type")) == 0 && util_cmp(xht_get(s->index,"ping"),packet_get_str(p,"trace")) == 0 && (from = hn_fromjson(s->index,p)) != NULL)
  {
    DEBUG_PRINTF("pong from %s",from->hexname);
    in = hn_path(from, in);
    switch_open(s,from,in);
    packet_free(p);
    return;
  }

  // handle pings, respond if local only or dedicated seed
  if(util_cmp("ping",packet_get_str(p,"type")) == 0 && (s->isSeed || path_local(in)))
  {
    switch_pong(s,p,in);
    packet_free(p);
    return;
  }

  // nothing processed, clean up
  packet_free(p);
}

-}

-- ---------------------------------------------------------------------

switch_send :: TxTelex -> TeleHash ()
switch_send p = do
  sw <- get
  -- require recipient at least, and not us
  if (tTo p) == (swId sw)
    then do
      logT $ "switch_send:to is us, dropping packet"
      return ()
    else do
      -- encrypt the packet to the line, chains together
      hc <- getHN (tTo p)
      case (hCrypto hc) of
        Nothing -> do
          logT $ "switch_send:crypto not set up"
          return ()
        Just crypto -> do

          (crypto1,mlined) <- crypt_lineize crypto p
          putHN $ hc { hCrypto = Just crypto1 }
          case mlined of
            Just lined -> do
              switch_sendingQ $ p { tChain = Just lined}
            Nothing -> do
              -- queue most recent packet to be sent after opened
              hc2 <- getHN (tTo p)
              putHN $ hc2 { hOnopen = Just p }

              -- no line, so generate open instead
              switch_open (tTo p) Nothing

{-
void switch_send(switch_t s, packet_t p)
{
  packet_t lined;

  if(!p) return;

  // require recipient at least, and not us
  if(!p->to || p->to == s->id) return (void)packet_free(p);

  // encrypt the packet to the line, chains together
  lined = crypt_lineize(p->to->c, p);
  if(lined) return switch_sendingQ(s, lined);

  // queue most recent packet to be sent after opened
  if(p->to->onopen) packet_free(p->to->onopen);
  p->to->onopen = p;

  // no line, so generate open instead
  switch_open(s, p->to, NULL);
}
-}

-- ---------------------------------------------------------------------

switch_open :: HashName -> Maybe Path -> TeleHash ()
switch_open hn direct = do
  hc <- getHN hn
  case hCrypto hc of
    Nothing -> do
      logT $ "switch_open: can't open, no key for " ++ (unHN (hHashName hc))
      sw <- get
      case (swHandler sw) of
        Just handle -> handle hn
        Nothing -> return ()
    Just crypto -> do
      -- actually send the open
      sw <- get
      let inner1 = packet_new (hHashName hc)
          inner2 = packet_set_str inner1 "to" (unHN $ hHashName hc)
          inner3 = packet_set inner2 "from" (swParts sw)
          inner = OpenizeInner { oiAt = cAtOut crypto
                               , oiTo = hHashName hc
                               , oiFrom = swParts sw
                               , oiLine = cLineHex crypto
                               }
      case Map.lookup "1a" (swIndexCrypto sw) of
        Nothing -> do
          logT $ "switch_open: missing crypto"
          assert False undefined
        Just cryptoSelf -> do
          mopen <- crypt_openize cryptoSelf crypto inner
          logT $ "opening to " ++ show ("1a",hHashName hc)
          case mopen of
            Nothing -> do
              logT $ "switch_open: could not openize, discarding"
              return ()
            Just open -> do
              switch_sendingQ $ inner3 { tChain = Just open }

{-
// tries to send an open if we haven't
void switch_open(switch_t s, hn_t to, path_t direct)
{
  packet_t open, inner;

  if(!to) return;
  if(!to->c)
  {
    DEBUG_PRINTF("can't open, no key for %s",to->hexname);
    if(s->handler) s->handler(s,to);
    return;
  }

  // actually send the open
  inner = packet_new();
  packet_set_str(inner,"to",to->hexname);
  packet_set(inner,"from",(char*)s->parts->json,s->parts->json_len);
  open = crypt_openize((crypt_t)xht_get(s->index,to->hexid), to->c, inner);
  DEBUG_PRINTF("opening to %s %hu %s",to->hexid,packet_len(open),to->hexname);
  if(!open) return;
  open->to = to;
  if(direct) open->out = direct;
  switch_sendingQ(s, open);
}
-}

-- ---------------------------------------------------------------------

--  |internally adds to sending queue
switch_sendingQ :: TxTelex -> TeleHash ()
switch_sendingQ p = do
  -- logT $ "switch_sendingQ " ++ show p
  -- if there's no path, find one or copy to many
  mp <- if tOut p == PNone
          then do
            -- just being paranoid
            case packet_get_str p "to" of
              Nothing -> do
                logT $ "switch_sendingQ:trying to send without dest:" ++ show p
                return Nothing
              Just toVal -> do
                -- if the last path is alive, just use that
                to <- getHN (HN toVal)
                (done,p2) <- do
                  case hLast to of
                    Just lastJson -> do
                      lastPath <- getPath (hHashName to) lastJson
                      -- logT $ "switch_sendingQ:lastPath=" ++ show lastPath
                      alive <- path_alive lastPath
                      -- logT $ "switch_sendingQ:alive=" ++ show alive
                      if alive
                        then return (True, p { tOut = lastJson })
                        else return (False,p)
                    Nothing -> return (False,p)
                if done
                  then return (Just p2)
                  else do
                    -- try sending to all paths
                    forM_ (Map.elems (hPaths to)) $ \path1 -> do
                      -- logT $ "switch_sendingQ:processing path=" ++ show path1
                      switch_sendingQ $ p { tOut = pJson path1 }
                    return Nothing
          else do
            return (Just p)
  -- logT $ "switch_sendingQ:mp=" ++ show mp
  case mp of
    Nothing -> return ()
    Just p3 -> do
      timeNow <- io getClockTime
      path <- getPath (tTo p3) (tOut p3)
      putPath (tTo p3) (path { pAtOut = Just timeNow })
      sw <- get
      put $ sw { swOut = (swOut sw) ++ [p3]
               , swLast = Just p3
               }
      return ()

{-
// internally adds to sending queue
void switch_sendingQ(switch_t s, packet_t p)
{
  packet_t dup;
  if(!p) return;

  // if there's no path, find one or copy to many
  if(!p->out)
  {
    // just being paranoid
    if(!p->to)
    {
      packet_free(p);
      return;
    }

    // if the last path is alive, just use that
    if(path_alive(p->to->last)) p->out = p->to->last;
    else{
      int i;
      // try sending to all paths
      for(i=0; p->to->paths[i]; i++)
      {
        dup = packet_copy(p);
        dup->out = p->to->paths[i];
        switch_sendingQ(s, dup);
      }
      packet_free(p);
      return;
    }
  }

  // update stats
  p->out->atOut = platform_seconds();

  // add to the end of the queue
  if(s->last)
  {
    s->last->next = p;
    s->last = p;
    return;
  }
  s->last = s->out = p;
}

-}

-- ---------------------------------------------------------------------

-- =====================================================================
-- chan api

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
          hc <- getHN (chTo chan)
          alive <- case chLast chan of
                       Nothing -> return False
                       Just lpj -> do
                         lp <- getPath (chTo chan) lpj
                         path_alive lp
          let p2 = if alive
                     then p1 { tOut = gfromJust "chan_packet" $ chLast chan }
                     else p1
              p3 = if chState chan == ChanStarting
                     then packet_set_str p2 "type" (chType chan)
                     else p2
              p4 = packet_set_int p3 "c" (unChannelId $ chId chan)
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
          case e of
            Nothing -> return ()
            Just e1 -> do
              let e2 = packet_set_str e1 "err" err
              chan_send c e2
        else return ()
    Nothing -> return ()
  -- no grace period for reliable
  let c2 = c { chState = ChanEnded }
  putChan c2
  to <- getHN $ chTo c2
  putHN $ to { hChans = Map.delete (chId c2) (hChans to) }
  chan_queue c2
  return c2

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
  let r2 = packet_set_str r ".from" (show $ chUid c)
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



