module Network.TeleHash.SwitchApi
  (
   -- * Telehash-c api
    switch_send
  , switch_sending
  , switch_receive
  , switch_open
  , switch_pop
  , switch_seed
  , switch_loop

  -- * chan api
  , chan_start
  , chan_new
  , chan_free
  , chan_reliable
  , chan_reset
  , chan_in
  , chan_packet
  , chan_pop
  , chan_pop_all
  , chan_end
  , chan_fail
  , chan_notes
  , chan_notes_all
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

import Network.TeleHash.Convert
import Network.TeleHash.Crypt
import Network.TeleHash.Hn
import Network.TeleHash.Packet
import Network.TeleHash.Path
import Network.TeleHash.Paths
import Network.TeleHash.Types
import Network.TeleHash.Utils

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
      logT $ "DEOPEN " ++ showJson (doJs open)
      case open of
        DeOpenizeVerifyFail -> do
          return ()
        deOpenizeResult -> do
          logT $ "receive.deopenize verified ok " -- ++ show open
          let minner = parseJsVal (doJs open) :: Maybe OpenizeInner
          case minner of
            Nothing -> do
              logT $ "switch_receive:invalid inner js:" ++ (BC.unpack $ lbsTocbs $ Aeson.encode $ doJs open)
              return ()
            Just inner -> do
              -- logT $ "switch_receive:openize:inner=" ++ show inner
              mfrom <- hn_frompacket inner deOpenizeResult
              -- mfrom <- hn_getparts (oiFrom inner)
              case mfrom of
                Nothing -> do
                  logT $ "switch_receive:openize:invalid from" ++ show (oiFrom inner)
                  return ()
                Just from -> do
                  logT $ "switch_receive:openize:from=" ++ show (hHashName from)
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
                      logT $ "line in " ++ show (cLined lineCrypto,(hHashName from),cLineHex lineCrypto)
                      -- DEBUG_PRINTF("line in %d %s %d %s",from->c->lined,from->hexname,from,from->c->lineHex);
                      let from2 = from { hCrypto = Just lineCrypto }
                      putHN from2
                      if cLined lineCrypto == LineReset
                        then chan_reset (hHashName from2)
                        else return ()
                      -- xht_set(s->index, (const char*)from->c->lineHex, (void*)from);
                      putHexLine (cLineHex lineCrypto) (hHashName from)
                      -- logT $ "switch_receive:openize:calling hn_path for path" ++ showJson (pJson path)
                      inVal <- hn_path (hHashName from) (pJson path)
                      from3 <- getHN (hHashName from2)
                      -- logT $ "switch_receive:openize:hn_path returned" ++ showJson inVal
                      switch_open (hHashName from) inVal -- in case
                      case hOnopen from3 of
                        Nothing ->  do
                          logT $ "switch_receive:openize:no onopen"
                          return ()
                        Just onopen -> do
                          -- logT $ "switch_receive:openize:processing onopen"
                          putHN $ from3 { hOnopen = Nothing }
                          switch_send (onopen { tOut = pJson (gfromJust "onopen" inVal) })
                          return ()

    LinePacket pbody -> do
      -- its a line
      -- logT $ "receive:got line msg"
      let lineID = BC.unpack $ B16.encode $ BC.take 16 pbody
      -- logT $ "receive:lineID=" ++ lineID
      mfrom <- getHexLineMaybe lineID
      case mfrom of
        Nothing -> do
          logT $ "switch_receive: no line found for " ++ lineID
          return ()
        Just fromHn -> do
          from <- getHN fromHn
          inVal <- hn_path (hHashName from) (pJson path)
          p <- crypt_delineize (gfromJust "switch_receive" $ hCrypto from) packet
          -- logT $ "crypt_delineize result:" ++ show p
          case p of
            Left err -> do
              -- DEBUG_PRINTF("invlaid line from %s %s",path_json(in),from->hexname);
              logT $ "invalid line from " ++ show (inVal,hHashName from)
              return ()
            Right rx -> do
              mchan <- chan_in (hHashName from) rx
              case mchan of
                Just chan -> do
                  sw <- get
                  -- if new channel w/ seq, configure as reliable
                  logT $ "switch_receive (chan,rx) :" ++ show (chan,rx)
                  logT $ "switch_receive (chState,seq) :" ++ show (chState chan,packet_has_key rx "seq")
                  chan2 <- if (chState chan == ChanStarting && packet_has_key rx "seq")
                             then chan_reliable chan (swWindow sw)
                             else return chan
                  chan_receive chan2 rx
                Nothing -> do
                  -- bounce it
                  if packet_has_key rx "err"
                    then return ()
                    else do
                      let txp = packet_new fromHn
                          tx = txp { tOut = pJson path }
                          tx2 = packet_set_str tx "err" "unknown channel"
                      switch_send tx2
                      {-
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
                     -}
    PingPongPacket p -> do
      -- handle valid pong responses, start handshake

     {-
       if(util_cmp("pong",packet_get_str(p,"type")) == 0
          && util_cmp(xht_get(s->index,"ping"),packet_get_str(p,"trace")) == 0
          && (from = hn_fromjson(s->index,p)) != NULL)
       {
         DEBUG_PRINTF("pong from %s",from->hexname);
         in = hn_path(from, in);
         switch_open(s,from,in);
         packet_free(p);
         return;
       }

     -}
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
      -- logT $ "switch_send:p=" ++ show p
      -- insert the JS into the packet head
      p2 <- telexToPacket p
      logT $ "switch_send:p2=" ++ show p2
      (mcrypto1,mlined) <- crypt_lineize (hCrypto hc) p2
      putHN $ hc { hCrypto = mcrypto1 }
      case mlined of
        Just lined -> do
          switch_sendingQ $ p2 { tLp = Just lined}
        Nothing -> do
          -- queue most recent packet to be sent after opened
          logT $ "switch_send:queueing packet until line:" ++ show (tTo p2) ++ "," ++ showJson (tJs p2)
          hc2 <- getHN (tTo p2)
          putHN $ hc2 { hOnopen = Just p2 }

          -- no line, so generate open instead
          switch_open (tTo p2) Nothing

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
        Just handler -> handler hn
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
              let inner4 = case direct of
                             Just path -> inner3 {tOut = pJson path}
                             Nothing   -> inner3
              logT $ "switch_open:sending " ++ show inner4
              switch_sendingQ $ inner4 { tLp = Just open }

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
            let toVal = tTo p
            -- if the last path is alive, just use that
            to <- getHN toVal
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

-- |Returns Uid of channels requiring processing
switch_pop :: TeleHash (Maybe Uid)
switch_pop = do
  sw <- get
  case Set.elems (swChans sw) of
    [] -> do
      return Nothing
    (c:_) -> do
      chan_dequeue c
      return $ Just c

{-
chan_t switch_pop(switch_t s)
{
  chan_t c;
  if(!s->chans) return NULL;
  c = s->chans;
  chan_dequeue(c);
  return c;
}
-}

-- ---------------------------------------------------------------------

switch_seed :: HashName -> TeleHash ()
switch_seed hn = do
  sw <- get
  put $ sw { swSeeds = Set.insert hn (swSeeds sw)}
{-
void switch_seed(switch_t s, hn_t hn)
{
  if(!s->seeds) s->seeds = bucket_new();
  bucket_add(s->seeds, hn);
}
-}

-- ---------------------------------------------------------------------

-- |give all channels a chance
switch_loop :: TeleHash ()
switch_loop = return ()
{-
void switch_loop(switch_t s)
{
  // give all channels a chance
}
-}

-- ---------------------------------------------------------------------

-- =====================================================================
-- chan api

-- Based on the telehash-c version

-- ---------------------------------------------------------------------

-- |kind of a macro, just make a reliable channel of this type to this hashname
chan_start :: HashName -> String -> TeleHash TChan
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
  logT $ "chan_new:" ++ show (toHn,typ,mcid)
  to <- getHN toHn
  case hChanOut to of
    CID 0 -> chan_reset toHn
    CID _ -> return ()

  to2 <- getHN toHn
  -- use new id if none given
  cid <- case mcid of
           Nothing -> do
             let c = hChanOut to2
                 co = c + 2
             withHN toHn $ \hc -> hc {hChanOut = co}
             return c
           Just c -> do
             logT $ "chan_new:using cid:" ++ show c
             return c

  logT $ "chan_new:channel new " ++ show (cid,typ)
  uid <- getNextUid
  let chan = TChan
              { chId       = cid
              , chUid      = uid
              , chTo       = toHn
              , chType     = typ
              , chReliable = 0
              , chState    = ChanStarting
              , chLast     = Nothing
              , chNext     = Nothing
              , chIn       = []
              , chNotes    = []
              , chHandler  = Nothing
              , chArg       = CArgNone
              , chSeq       = Nothing
              , chMiss     = Nothing
              }
  withHN toHn (\hc -> hc { hChans = Map.insert cid (chUid chan) (hChans hc) })

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
  logT $ "chan_free " ++ show (chId chan,chUid chan)
  -- remove references
  chan_dequeue (chUid chan)

  rmChanFromHn (chTo chan) (chId chan)

  if (chReliable chan /= 0)
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

  -- must be last, remove from the index
  rmChan (chUid chan)


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
chan_reliable :: TChan -> Int -> TeleHash TChan
chan_reliable c window = do
  logT $ "chan_reliable (c,window):" ++ show (c,window)
  if window == 0 || chState c /= ChanStarting || chReliable c /= 0
    then return c
    else do
      let c2 = c { chReliable = window }
      putChan c2
      chan_seq_init c2
      chan_miss_init c2
      c3 <- getChan (chUid c2)
      return c3

{-
chan_t chan_reliable(chan_t c, int window)
{
  if(!c || !window || c->state != CHAN_STARTING || c->reliable) return c;
  c->reliable = window;
  chan_seq_init(c);
  chan_miss_init(c);
  return c;
}
-}

-- ---------------------------------------------------------------------

-- |resets channel state for a hashname
chan_reset :: HashName -> TeleHash ()
chan_reset toHn = do
  logT $ "chan_reset:" ++ show toHn
  sw <- get
  to1 <- getHN toHn
  let base = if (swId sw) > (hHashName to1)
               then 1 else 2
  withHN toHn $ \hc -> if (hChanOut hc == CID 0)
                        then hc {hChanOut = CID base}
                        else hc
  -- fail any existing chans from them
  withHNM toHn $ \hc -> do
    forM_ (Map.elems $ hChans hc) $ \chUid -> do
      ch <- getChan chUid
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
          logT $ "chan_in:p=" ++ show p
          let mtyp = getRxTelexType p
          logT $ "chan_in:mtyp=" ++ show mtyp
          logT $ "chan_in:cid,hChanout from=" ++ show (cid,hChanOut from)
          if (mtyp == Nothing
             || channelSlot cid == channelSlot (hChanOut from))
            then return Nothing
            else do
              logT $ "chan_in:making new chan"
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
chan_packet chIn = do
  chan <- getChan (chUid chIn)
  if chState chan == ChanEnded
    then return Nothing
    else do
      mp <- if chReliable chan /= 0
              then chan_seq_packet chan
              else return $ Just (packet_new (chTo chan))
      chan2 <- getChan (chUid chIn)
      case mp of
        Nothing -> return Nothing
        Just p -> do
          let p1 = p { tTo = chTo chan2 }
          alive <- case chLast chan2 of
                       Nothing -> return False
                       Just lpj -> do
                         lp <- getPath (chTo chan2) lpj
                         path_alive lp
          let p2 = if alive
                     then p1 { tOut = gfromJust "chan_packet" $ chLast chan }
                     else p1
              p3 = if chState chan2 == ChanStarting
                     then packet_set_str p2 "type" (chType chan2)
                     else p2
              p4 = packet_set_int p3 "c" (unChannelId $ chId chan2)
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

-- |pop a packet from this channel to be processed, caller must free
chan_pop :: Uid -> TeleHash (Maybe RxTelex)
chan_pop chanUid = do
  mchan <- getChanMaybe chanUid
  case mchan of
    Nothing -> return Nothing
    Just chan -> do
      logT $ "chan_pop:(uid,id,chReliable)=" ++ show (chanUid,chId chan,chReliable chan)
      if (chReliable chan /= 0)
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

chan_pop_all :: Uid -> TeleHash [RxTelex]
chan_pop_all uid = do
  let
    go acc = do
      mrx <- chan_pop uid
      case mrx of
        Nothing -> return acc
        Just rx -> go (acc ++ [rx])
  msgs <- go []
  return msgs

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
  logT $ "channel fail " ++ show (chId c,chUid c,merr)
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
chan_notes cIn = do
  c <- getChan (chUid cIn)
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

-- |get all the incoming notes waiting to be handled
chan_notes_all :: TChan -> TeleHash [RxTelex]
chan_notes_all cIn = do
  c <- getChan (chUid cIn)
  let r = chNotes c
  putChan $ c {chNotes = []}
  return r

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
chan_reply :: TChan -> TxTelex -> TeleHash Int
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
  -- logT $ "channel in " ++ show (chId c,p)
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
      if (chReliable c4 /= 0)
        then do
          chan_miss_check c4 p
          r <- chan_seq_receive c4 p
          logT $ "chan_receive:chan_seq_receive returned " ++ show r
          if not r
            then return () -- queued, nothing more to do
            else chan_queue c4
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
  logT $ "chan_send:channel out " ++ show (chId c,p)
  p2 <- if chReliable c /= 0
          then do
            let p' = packet_copy p
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
chan_ack cIn = do
  mc <- getChanMaybe (chUid cIn)
  case mc of
    Nothing -> do
      logT $ "chan_ack:channel dead:" ++ show (chId cIn,chUid cIn)
      return ()
    Just c -> do
      if not (chReliable c /= 0)
        then return ()
        else do
          logT $ "chan_ack about to call chan_seq_ack"
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

-- |remove channel id from switch processing queue
chan_dequeue :: Uid -> TeleHash ()
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
chan_seq_ack cIn mp = do
  c <- getChan (chUid cIn)
  case chSeq c of
    Nothing -> do
      logT $ "chan_seq_ack:chSeq not populated"
      assert False undefined
    Just s -> do
      -- detemine if we need to ack
      if seNextIn s == 0
        then return mp
        else do
          if isJust mp && seAcked s /= 0 && seAcked s == (seNextIn s) - 1
            then return Nothing
            else do
              mp2 <- case mp of
                Nothing -> do
                  -- ack-only packet
                  mcp <- chan_packet c
                  return mcp
                Just pp -> return (Just pp)
              case mp2 of
                Nothing -> do
                  logT $ "chan_seq_ack: no packet available"
                  return Nothing
                Just p -> do
                  let s2 = s { seAcked = (seNextIn s) - 1 }
                      p2 = packet_set_int p "ack" (seAcked s2)
                      c2 = c { chSeq = Just s2 }
                  putChan c2

                  -- check if miss is not needed
                  if seSeen s2 < seNextIn s2 || Map.member 0 (seIn s2)
                    then return (Just p2)
                    else do
                      -- create miss array, up to 10 ids of missing packets
                      let misses = concatMap (\i -> if Map.member i (seIn s2) then [i] else [])
                                             $ [1 .. min 10 (chReliable c2)]
                          missVal = show misses
                      logT $ "chan_seq_ack:missVal=" ++ missVal
                      return $ Just (packet_set_str p2 "miss" missVal)

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
chan_seq_packet cIn = do
  c <- getChan (chUid cIn)
  case chSeq c of
    Nothing -> do
      logT $ "chan_seq_packet:no chSeq structure"
      return Nothing
    Just s -> do
      let p = packet_new (chTo c)
      -- make sure there's tracking space
      miss <- chan_miss_track c (seId s) p
      if miss /= 0
        then return Nothing
        else do
          -- set seq and add any acks
          let p2 = packet_set_int p "seq" (seId s)
              s2 = s { seId = (seId s) + 1 }
          putChan $ c { chSeq = Just s2 }
          logT $ "chan_seq_packet about to call chan_seq_ack"
          r <- chan_seq_ack c (Just p2)
          return r

{-
// new channel sequenced packet
packet_t chan_seq_packet(chan_t c)
{
  packet_t p = packet_new();
  seq_t s = (seq_t)c->seq;

  // make sure there's tracking space
  if(chan_miss_track(c,s->id,p)) return NULL;

  // set seq and add any acks
  packet_set_int(p,"seq",(int)s->id++);
  return chan_seq_ack(c, p);
}
-}
-- ---------------------------------------------------------------------

-- |buffers packets until they're in order, returns True if some are ready to pop
chan_seq_receive :: TChan -> RxTelex -> TeleHash Bool
chan_seq_receive cIn p = do
  c <- getChan (chUid cIn)
  logT $ "chan_seq_receive:" ++ show (c,p)
  case chSeq c of
    Nothing -> do
      logT $ "chan_seq_receive: no chSeq struct for " ++ show (c,p)
      return False
    Just s -> do
      -- drop or cache incoming packet
      let mseq = packet_get_int p "seq"
      let idVal = case mseq of
                    Nothing -> 0
                    Just v -> v
          offset = idVal - (seNextIn s)
      if mseq == Nothing || offset < 0 || offset >= chReliable c
         || (Map.member offset (seIn s))
        then do
          logT $ "chan_seq_receive:nothing to do:" ++ show (mseq,offset,chReliable c,seIn s)
          return False
        else do
          let -- track highest seen
              seen = if idVal > seSeen s then idVal else seSeen s
              s2 = s { seIn = Map.insert offset p (seIn s)
                     , seSeen = seen
                     }
              c2 = c { chSeq = Just s2 }
          putChan c2
          return $ Map.member 0 (seIn s2)

{-
// buffers packets until they're in order
int chan_seq_receive(chan_t c, packet_t p)
{
  int offset;
  uint32_t id;
  char *seq;
  seq_t s = (seq_t)c->seq;

  // drop or cache incoming packet
  seq = packet_get_str(p,"seq");
  id = seq?(uint32_t)strtol(seq,NULL,10):0;
  offset = id - s->nextin;
  if(!seq || offset < 0 || offset >= c->reliable || s->in[offset])
  {
    packet_free(p);
  }else{
    s->in[offset] = p;
  }

  // track highest seen
  if(id > s->seen) s->seen = id;

  return s->in[0] ? 1 : 0;
}
-}

-- ---------------------------------------------------------------------

-- |returns ordered packets for this channel, updates ack

chan_seq_pop :: TChan -> TeleHash (Maybe RxTelex)
chan_seq_pop cIn = do
  c <- getChan (chUid cIn)
  case chSeq c of
    Nothing -> do
      logT $ "chan_seq_pop:missing chSeq structure" ++ show c
      return Nothing
    Just s -> do
      case Map.lookup 0 (seIn s) of
        Nothing -> return Nothing
        Just p -> do
          -- pop off the first, slide any others back, and return
          let inNew = Map.fromList $ map (\(k,v) -> (k - 1,v)) (Map.toList $ seIn s)
              sNew = s { seNextIn = (seNextIn s) + 1
                       , seIn = inNew
                       }
          putChan $ c { chSeq = Just sNew }
          return (Just p)

{-
// returns ordered packets for this channel, updates ack
packet_t chan_seq_pop(chan_t c)
{
  packet_t p;
  seq_t s = (seq_t)c->seq;
  if(!s->in[0]) return NULL;
  // pop off the first, slide any others back, and return
  p = s->in[0];
  memmove(s->in, s->in+1, (sizeof (packet_t)) * (c->reliable - 1));
  s->in[c->reliable-1] = 0;
  s->nextin++;
  return p;
}

-}
-- ---------------------------------------------------------------------

chan_seq_init :: TChan -> TeleHash ()
chan_seq_init cIn = do
  c <- getChan (chUid cIn)
  let seqVal = Seq
             { seId     = 0
             , seNextIn = 0
             , seSeen   = 0
             , seAcked  = 0
             , seIn     = Map.empty
             }
      c2 = c { chSeq = Just seqVal }
  putChan c2

{-
void chan_seq_init(chan_t c)
{
  seq_t s = malloc(sizeof (struct seq_struct));
  memset(s,0,sizeof (struct seq_struct));
  s->in = malloc(sizeof (packet_t) * c->reliable);
  memset(s->in,0,sizeof (packet_t) * c->reliable);
  c->seq = (void*)s;
}


-}

-- ---------------------------------------------------------------------

chan_seq_free :: TChan -> TeleHash ()
chan_seq_free cIn = do
  c <- getChan (chUid cIn)
  putChan $ c { chSeq = Nothing }

{-
void chan_seq_free(chan_t c)
{
  int i;
  seq_t s = (seq_t)c->seq;
  for(i=0;i<c->reliable;i++) packet_free(s->in[i]);
  free(s->in);
  free(s);
}
-}

-- ---------------------------------------------------------------------

-- |tracks packet for outgoing, eventually free's it, 0 ok or 1 for full/backpressure
chan_miss_track :: TChan -> Int -> TxTelex -> TeleHash Int
chan_miss_track cIn seq p = do
  c <- getChan (chUid cIn)
  case (chMiss c) of
    Nothing -> do
      logT $ "chan_miss_track:chMiss not populated"
      assert False undefined
    Just m -> do
      if seq - (mNextAck m) > (chReliable c) - 1
        then return 1
        else do
          let m2 = m { mOut = Map.insert (seq - (mNextAck m)) p (mOut m) }
          putChan $ c { chMiss = Just m2 }
          return 0

{-
// 1 when full, backpressure
int chan_miss_track(chan_t c, uint32_t seq, packet_t p)
{
  miss_t m = (miss_t)c->miss;
  if(seq - m->nextack > (uint32_t)(c->reliable - 1))
  {
    packet_free(p);
    return 1;
  }
  m->out[seq - m->nextack] = p;
  return 0;
}
-}

-- ---------------------------------------------------------------------
{-
// buffers packets to be able to re-send
void chan_miss_send(chan_t c, packet_t p);
-}

-- ---------------------------------------------------------------------

-- |looks at incoming miss/ack and resends or frees
chan_miss_check :: TChan -> RxTelex -> TeleHash ()
chan_miss_check cIn p = do
  logT $ "chan_miss_check for " ++ show (cIn,p)
  c <- getChan (chUid cIn)
  let miss = packet_get_packet p "miss"
  case chMiss c of
    Nothing -> do
      logT $ "chan_miss_check: no chMiss structure for " ++ show c
    Just m -> do
      case packet_get_int p "ack" of
        Nothing -> do
          logT $ "chan_miss_check: no ack field"
          return () -- grow some
        Just ack -> do
          let offset = ack - (mNextAck m)
          if offset < 0 || offset >= (chReliable c)
            then do
              logT $ "chan_miss_check:offset check:" ++ show (offset,chReliable c)
              return ()
            else do
              -- free and shift up to the ack
              assert False undefined

{-
// looks at incoming miss/ack and resends or frees
void chan_miss_check(chan_t c, packet_t p)
{
  uint32_t ack;
  int offset, i;
  char *id, *sack;
  packet_t miss = packet_get_packet(p,"miss");
  miss_t m = (miss_t)c->miss;

  sack = packet_get_str(p,"ack");
  if(!sack) return; // grow some
  ack = (uint32_t)strtol(sack, NULL, 10);
  // bad data
  offset = ack - m->nextack;
  if(offset < 0 || offset >= c->reliable) return;

  // free and shift up to the ack
  while(m->nextack <= ack)
  {
    // TODO FIX, refactor check into two stages
//    packet_free(m->out[0]);
    memmove(m->out,m->out+1,(sizeof (packet_t)) * (c->reliable - 1));
    m->out[c->reliable-1] = 0;
    m->nextack++;
  }

  // track any miss packets if we have them and resend
  if(!miss) return;
  for(i=0;(id = packet_get_istr(miss,i));i++)
  {
    ack = (uint32_t)strtol(id,NULL,10);
    offset = ack - m->nextack;
    if(offset >= 0 && offset < c->reliable && m->out[offset]) switch_send(c->s,m->out[offset]);
  }
}

-}
-- ---------------------------------------------------------------------

chan_miss_init :: TChan -> TeleHash ()
chan_miss_init cIn = do
  c <- getChan (chUid cIn)
  let miss = Miss { mNextAck = 0
                  , mOut = Map.empty
                  }
      c2 = c {chMiss = Just miss }
  putChan c2

{-
void chan_miss_init(chan_t c);
void chan_miss_init(chan_t c)
{
  miss_t m = (miss_t)malloc(sizeof (struct miss_struct));
  memset(m,0,sizeof (struct miss_struct));
  m->out = (packet_t*)malloc(sizeof (packet_t) * c->reliable);
  memset(m->out,0,sizeof (packet_t) * c->reliable);
  c->miss = (void*)m;
}
-}


-- ---------------------------------------------------------------------

chan_miss_free :: TChan -> TeleHash ()
chan_miss_free cIn = do
  c <- getChan (chUid cIn)
  putChan $ c { chMiss = Nothing }

{-
void chan_miss_free(chan_t c)
{
  int i;
  miss_t m = (miss_t)c->miss;
  for(i=0;i<c->reliable;i++) packet_free(m->out[i]);
  free(m->out);
  free(m);
}
-}
-- ---------------------------------------------------------------------



