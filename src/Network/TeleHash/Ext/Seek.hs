module Network.TeleHash.Ext.Seek
  (
    ext_seek
  , seek_auto
  , peer_send
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.State
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import System.Time

import Network.TeleHash.Dht
import Network.TeleHash.Ext.Path
import Network.TeleHash.Packet
import Network.TeleHash.Paths
import Network.TeleHash.SwitchApi
import Network.TeleHash.SwitchUtils
import Network.TeleHash.Types
import Network.TeleHash.Utils

import qualified Data.Map as Map
import qualified Data.Set as Set

-- ---------------------------------------------------------------------

ext_seek :: TChan -> TeleHash ()
ext_seek c = do
  logT $ "ext_seek entered with for " ++ showChan c

  let
    respFunc p = do
      logT $ "ext_seek:respFunc:processing " ++ showJson (rtJs p)
      c2 <- getChan (chUid c)
      if packet_has_key p "err"
        then do
          logT $ "ext_seek:err: " ++ showJson (rtJs p)
          return ()
        else do
          let seeVal = packet_get_str_always p "see"
          if length seeVal == 0 || not (all isHexDigit seeVal)
            then do
              logT $ "ext_seek:invalid seek of :" ++ seeVal
            else do
              distance <- dhtBucket (HN seeVal)
              bucket <- getBucketContents distance
              let sorted = sortBucketByAge bucket
              logT $ "ext_seek:sorted=" ++ show (map hHashName sorted)
              assert False undefined
              return ()

  util_chan_popall c (Just respFunc)

-- Seek for node 7766e761afb226d7b398379ea1bf12c53dc02580c683b173568b0c6cc3a09c00
-- >>>>:(Just "(chan:(31,CID 168,0,ChanStarting,\"seek\"))","{ type: 'ipv4', ip: '71.171.17.108', port: 50461}","Packet HeadJson {\"seek\":\"776\",\"type\":\"seek\",\"c\":168} 0 bytes")

{-
-- javascript version

// return a see to anyone closer
function inSeek(err, packet, chan)
{
  if(err) return;
  if(!isHEX(packet.js.seek)) return warn("invalid seek of ", packet.js.seek, "from:", packet.from.hashname);
  var self = packet.from.self;
  var seek = packet.js.seek;

  var see = [];
  var seen = {};

  // see if we have any seeds to add
  var bucket = dhash(self.hashname, packet.js.seek);
  var links = self.buckets[bucket] ? self.buckets[bucket] : [];

  // first, sort by age and add the most wise one
  links.sort(function(a,b){ return a.age - b.age}).forEach(function(seed){
    if(see.length) return;
    if(!seed.seed) return;
    see.push(seed.address(packet.from));
    seen[seed.hashname] = true;
  });

  // sort by distance for more
  links.sort(function(a,b){ return dhash(seek,a.hashname) - dhash(seek,b.hashname)}).forEach(function(link){
    if(seen[link.hashname]) return;
    if(link.seed || link.hashname.substr(0,seek.length) == seek)
    {
      see.push(link.address(packet.from));
      seen[link.hashname] = true;
    }
  });

  var answer = {end:true, see:see.filter(function(x){return x}).slice(0,8)};
  chan.send({js:answer});
}

-}

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
                    , seekSentAt = Nothing
                    , seekQueue = [hn]
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

put_seek :: Seek -> TeleHash ()
put_seek sk = do
  sks <- seeks_get
  sw <- get
  put $ sw {swIndexSeeks = Map.insert (seekId sk) sk sks }


-- ---------------------------------------------------------------------

seek_handler :: Uid -> TeleHash ()
seek_handler cid = do
  c <- getChan cid
  logT $ "seek_handler entered for " ++ showChan c ++ "," ++ show (chArg c)
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
  -- Do not retry the seek if it is inactive and too soon.
  -- thjs uses 5000ms for the timeout
  now <- io $ getClockTime
  if seekActive sk > 0
    || (isJust (seekSentAt sk) && isTimeOut now (seekSentAt sk) param_seek_wait_secs)
    then do
      logT $ "seek_send:seek already active or too soon:" ++ show (sk,now)
      return ()
    else do
      c <- chan_new to "seek" Nothing
      let sk2 = sk { seekActive = (seekActive sk) + 1 }
          c2 = c { chHandler = Just seek_handler
                 , chArg = CArgSeek sk2
                 }
      putChan c2
      put_seek sk2
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

-- |Create a seek to this hn and initiate connect This is called as
-- the default channel handler for when there is no crypto when trying
-- to send to a hashname.
_seek_auto :: HashName -> TeleHash ()
_seek_auto hn = do
  logT $ "_seek_auto entered for " ++ show hn
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
