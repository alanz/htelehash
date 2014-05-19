module Network.TeleHash.Ext.Seek
  (
    seek_auto
  , peer_send
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.State
import Data.List
import Data.List.Split
import Data.Maybe
import Prelude hiding (id, (.), head, either)

import Network.TeleHash.Ext.Path
import Network.TeleHash.Packet
import Network.TeleHash.Paths
import Network.TeleHash.SwitchApi
import Network.TeleHash.Types
import Network.TeleHash.Utils

import qualified Data.Map as Map
import qualified Data.Set as Set

-- ---------------------------------------------------------------------

{-


typedef struct seek_struct
{
  hn_t id;
  int active;
  packet_t note;
} *seek_t;

typedef struct seeks_struct
{
  xht_t active;
} *seeks_t;
-}

-- ---------------------------------------------------------------------

seeks_get :: TeleHash (Map.Map HashName Seek)
seeks_get = do
  sw <- get
  return $ swIndexSeeks sw

{-
seeks_t seeks_get(switch_t s)
{
  seeks_t sks;
  sks = xht_get(s->index,"seeks");
  if(sks) return sks;

  sks = malloc(sizeof (struct seeks_struct));
  memset(sks,0,sizeof (struct seeks_struct));
  sks->active = xht_new(11);
  xht_set(s->index,"seeks",sks);
  return sks;
}
-}

-- ---------------------------------------------------------------------

seek_get :: HashName -> TeleHash Seek
seek_get hn = do
  sks <- seeks_get
  case Map.lookup hn sks of
    Just sk -> return sk
    Nothing -> do
      let sk = Seek { seekId = hn
                    , seekActive = 0
                    , seekNote = Nothing
                    }
          sks2 = Map.insert hn sk sks
      sw <- get
      put $ sw { swIndexSeeks = sks2 }
      return sk

{-
seek_t seek_get(switch_t s, hn_t id)
{
  seek_t sk;
  seeks_t sks = seeks_get(s);
  sk = xht_get(sks->active,id->hexname);
  if(sk) return sk;

  sk = malloc(sizeof (struct seek_struct));
  memset(sk,0,sizeof (struct seek_struct));
  sk->id = id;
  xht_set(sks->active,id->hexname,sk);
  return sk;
}
-}

-- ---------------------------------------------------------------------

peer_handler :: Uid -> TeleHash ()
peer_handler cid = do
  c <- getChan cid
  -- remove the NAT punch path if any
  case chArg c of
    CArgPath path -> do
      path_free path
      putChan $ c { chArg = CArgNone }
      return ()
    _ -> return ()

  logT $ "peer_handler:" ++ show (chTo c)
  rxs <- chan_pop_all cid
  forM_ rxs $ \p -> do
    -- logT $ "peer_handler:processing " ++ show p
    let mrelayp = fromNetworkPacket (LP $ unBody $ paBody $ rtPacket p)
    logT $ "peer_handler:tunneled packet " -- ++ show mrelayp
    case mrelayp of
      Nothing -> do
        logT $ "peer_handler:discarding bad tunneled packet:" ++ show p
      Just relayp -> do
        logT $ "peer_handler:calling switch_receive for tunneled packet:" -- ++ show relayp
        let path = (pathFromPathJson $ rtSender p)
            path2 = path { pBridgeChan = Just cid }
        switch_receive relayp path2 (rtAt p)
  -- TODO: process relayed packets

{-
void peer_handler(chan_t c)
{
  // remove the nat punch path if any
  if(c->arg)
  {
    path_free((path_t)c->arg);
    c->arg = NULL;
  }

  DEBUG_PRINTF("peer handler %s",c->to->hexname);
  // TODO process relay'd packets
}
-}

-- ---------------------------------------------------------------------

-- csid may be address format
peer_send :: HashName -> [String] -> TeleHash ()
peer_send to address = do
  logT $ "peer_send:" ++ show (to,address)
  if length address /= 2 && length address /= 4
    then do
      logT $ "peer_send: malformed address " ++ show address
      return ()
    else do
      let (hn,csid,mipp) = case address of
            [hn1,csid1]         -> (hn1,csid1,Nothing)
            [hn1,csid1,ip,port] -> (hn1,csid1,Just (ip,port))
            xs                -> error $ "peer_send:invalid address:" ++ show xs
      mcrypto <- getCrypto csid
      case mcrypto of
        Nothing -> do
          logT $ "peer_send:no cipher set for " ++ csid
          return ()
        Just cs -> do
          -- new peer channel
          c <- chan_new to "peer" Nothing
          let c2 = c {chHandler = Just peer_handler }
          putChan c2
          mp <- chan_packet (chUid c2) True
          case mp of
            Nothing -> do
              logT $ "peer_send:cannot create packet for " ++ show c2
              return ()
            Just p -> do
              let p2 = packet_set_str p "peer" hn
                  p3 = packet_body p2 (cKey cs)

              -- Send the NAT punch if ip,port given
              case mipp of
                Nothing -> return ()
                Just (ipStr,portStr) -> do
                  logT $ "peer_send:must still send NAT punch to" ++ show mipp
                  let punch = packet_new (HN hn)
                      path = PathIPv4 (read ipStr) (read portStr)
                      punch2 = punch { tOut = PIPv4 path }
                      punch3 = punch2 { tLp = Just (toLinePacket newPacket) }
                  putPath (HN hn) (Path PtIPv4 (PIPv4 path) Nothing Nothing Nothing Nothing)
                  switch_sendingQ punch3
              chan_send (chUid c2) p3

{-
// csid may be address format
void peer_send(switch_t s, hn_t to, char *address)
{
  char *csid, *ip = NULL, *port;
  packet_t punch = NULL;
  crypt_t cs;
  chan_t c;
  packet_t p;

  if(!address) return;
  if(!(csid = strchr(address,','))) return;
  *csid = 0;
  csid++;
  // optional address ,ip,port for punch
  if((ip = strchr(csid,',')))
  {
    *ip = 0;
    ip++;
  }
  if(!(cs = xht_get(s->index,csid))) return;

  // new peer channel
  c = chan_new(s, to, "peer", 0);
  c->handler = peer_handler;
  p = chan_packet(c);
  packet_set_str(p,"peer",address);
  packet_body(p,cs->key,cs->keylen);

  // send the nat punch packet if ip,port is given
  if(ip && (port = strchr(ip,',')))
  {
    *port = 0;
    port++;
    punch = packet_new();
    c->arg = punch->out = path_new("ipv4"); // free path w/ peer channel cleanup
    path_ip(punch->out,ip);
    path_port(punch->out,atoi(port));
    switch_sendingQ(s,punch);
  }

  chan_send(c, p);
}
-}

-- ---------------------------------------------------------------------

seek_handler :: Uid -> TeleHash ()
seek_handler cid = do
  c <- getChan cid
  case chArg c of
    CArgSeek sk -> do
      mp <- chan_pop (chUid c)
      case mp of
        Nothing -> do
          logT $ "seek_handler:no message popped for :" ++ show (chId c,chUid c)
          return ()
        Just p -> do
          logT $ "seek_handler:seek response for " ++ show (seekId sk) ++ "," ++ showJson (rtJs p)
          -- process see array and end channel
          let msee = packet_get p "see"
          logT $ "seek_handler:msee=" ++ show msee
          case msee of
            Nothing -> do
              logT $ "seek_handler:no see field for :" ++ show (chId c,chUid c)
              return ()
            Just seeValue -> do
              logT $ "seek_handler:seeValue=" ++ show seeValue
              let msee2 = parseJsVal seeValue :: Maybe [String]
              case msee2 of
                Nothing -> do
                  logT $ "seek_handler:invalid see field for :" ++ show (chId c,chUid c)
                  return ()
                Just see2 -> do
                  sw <- get
                  forM_ see2 $ \see -> do
                    let address = splitOn "," see
                    case address of
                      (hn:_) -> do
                        if (HN hn /= swId sw)
                          then peer_send (chTo c) address
                          else return ()
                      _ -> do
                        logT $ "seek_handler:cannot process see " ++ see
                        return ()
                  -- TODO sk->active-- and check to return note
                  return ()

    arg -> do
      logT $ "seek_handler:unexpected arg:" ++ show arg
      return ()
{-
void seek_handler(chan_t c)
{
  int i = 0;
  char *address;
  seek_t sk = (seek_t)c->arg;
  packet_t see, p = chan_pop(c);
  if(!sk || !p) return;
  DEBUG_PRINTF("seek response for %s of %.*s",sk->id->hexname,p->json_len,p->json);

  // process see array and end channel
  see = packet_get_packet(p,"see");
  while((address = packet_get_istr(see,i)))
  {
    i++;
    if(strncmp(address,sk->id->hexname,64) == 0) peer_send(c->s, c->to, address);
    // TODO maybe recurse others
  }
  packet_free(see);
  packet_free(p);
  // TODO sk->active-- and check to return note
}
-}

-- ---------------------------------------------------------------------

seek_send :: Seek -> HashName -> TeleHash ()
seek_send sk to = do
  logT $ "seek_send entered"
  c <- chan_new to "seek" Nothing
  let sk2 = sk { seekActive = (seekActive sk) + 1 }
      c2 = c { chHandler = Just seek_handler
             , chArg = CArgSeek sk2
             }
  putChan c2
  mp <- chan_packet (chUid c2) True
  case mp of
    Nothing -> do
      logT $ "seek_send:failed to make channel packet"
      return ()
    Just p -> do
      let p2 = packet_set_str p "seek" (unHN $ seekId sk2) -- TODO make a prefix
      logT $ "seek_send about to send on " ++ show c2
      chan_send (chUid c2) p2
{-
void seek_send(switch_t s, seek_t sk, hn_t to)
{
  chan_t c;
  packet_t p;
  sk->active++;
  c = chan_new(s, to, "seek", 0);
  c->handler = seek_handler;
  c->arg = sk;
  p = chan_packet(c);
  packet_set_str(p,"seek",sk->id->hexname); // TODO make a prefix
  chan_send(c, p);
}
-}

-- ---------------------------------------------------------------------

-- create a seek to this hn and initiate connect
_seek_auto :: HashName -> TeleHash ()
_seek_auto hn = do
  sk <- seek_get hn
  logT $ "_seek_auto:seek connecting " ++ show sk
  -- TODO get near from somewhere
  sw <- get
  let seed = ghead "seek_auto" $ Set.toList (swSeeds sw)

  seek_send sk seed


{-
// create a seek to this hn and initiate connect
void _seek_auto(switch_t s, hn_t hn)
{
  seek_t sk = seek_get(s,hn);
  DEBUG_PRINTF("seek connecting %s",sk->id->hexname);
  // TODO get near from somewhere
  seek_send(s, sk, bucket_get(s->seeds, 0));
}
-}

-- ---------------------------------------------------------------------

seek_auto :: TeleHash ()
seek_auto = do
  sw <- get
  put $ sw {swHandler = Just _seek_auto}

{-
void seek_auto(switch_t s)
{
  s->handler = _seek_auto;
}

void seek_free(switch_t s)
{
  seeks_t sks = seeks_get(s);
  // TODO xht_walk active and free each one
  free(sks);
}
-}

-- ---------------------------------------------------------------------

{-
// just call back note instead of auto-connect
void seek_note(switch_t s, hn_t h, packet_t note)
{

}
-}
