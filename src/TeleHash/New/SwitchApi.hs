module TeleHash.New.SwitchApi
  (
   -- * Telehash-c api
     switch_send
  , switch_sending
  , switch_receive
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
import TeleHash.New.Path
import TeleHash.New.Paths
import TeleHash.New.Packet
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
      assert False undefined
    _ -> do
      logT $ "switch_receive:not processing:" ++ show rxPacket
      assert False undefined
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

