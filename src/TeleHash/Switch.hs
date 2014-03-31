{-# LANGUAGE OverloadedStrings #-}
module TeleHash.Switch
  (
    Defaults(..)
  , defaults
  , switch
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Crypto.Random
import Data.Aeson
import Data.Bits
import Data.Char
import Data.List
import Data.Maybe
import Data.String.Utils
import Network.BSD
import qualified Network.Socket as NS
import Prelude hiding (id, (.), head, either, catch)
import System.IO
import System.Log.Handler.Simple
import System.Log.Logger
import System.Time
-- import System.Directory
import TeleHash.Utils

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Network.Socket.ByteString as SB
import qualified System.Random as R

-- ---------------------------------------------------------------------

--

-- ---------------------------------------------------------------------

data Id = Id { id1a :: String
             , id1a_secret :: String
             } deriving Show


instance FromJSON Id where
     parseJSON (Object v) = Id <$>
                            v .: "1a" <*>
                            v .: "1a_secret"
     -- A non-Object value is of the wrong type, so fail.
     parseJSON _          = mzero

testId :: Maybe Id
testId = r
  where
    v = "{\"1a\":\"o0UL/D6qQ+dcSX7hCoyMjLDYeA6dNScZ+YY/fcX4fyCtsSO2u9L5Lg==\",\"1a_secret\":\"iollyIcHaGeD/JpUNn/7ef1QAzE=\"}"
    r = decode v


testSeeds = do
  fc <- BL.readFile "../data/seeds.json"
  let mv = decode fc :: Maybe Value
  putStrLn $ "seeds=" ++ show mv


-- ---------------------------------------------------------------------

data Msg = Msg String

relayPid = PId 1


initialSeeds :: [SeedInfo]
initialSeeds =
 [ SI
    { sId = "89a4cbc6c27eb913c1bcaf06bac2d8b872c7cbef626b35b6d7eaf993590d37de"
    , sAdmin = "http://github.com/quartzjer"
    , sPaths =
       [ Path { pType = PathType"ipv4"
              , pIp = "208.68.164.253"
              , pPort = 42424
              , pHttp = ""
              , pLastIn = Nothing
              , pLastOut = Nothing
              }
       , Path { pType = PathType "ipv6"
              , pIp = "2605:da00:5222:5269:230:48ff:fe35:6572"
              , pPort = 42424
              , pHttp = ""
              , pLastIn = Nothing
              , pLastOut = Nothing
              }
       , Path { pType = PathType "http"
              , pIp = ""
              , pPort = 42424
              , pHttp = "http://208.68.164.253:42424"
              , pLastIn = Nothing
              , pLastOut = Nothing
              }
       ]
    , sParts =
        [("2a", "beb07e8864786e1d3d70b0f537e96fb719ca2bbb4a2a3791ca45e215e2f67c9a")
        ,("1a", "6c0da502755941a463454e9d478b16bbe4738e67")
        ]
    , sKeys =
        [ ("2a", "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAvJlhpi2pZZRrnf+bmnnRRAQHfzMzDwOV+s+JzamyL0X9wwJK8m2DHCFcpJQSLFIzv3v+e102+pZlIWAU6vvO5s6J60C+9UwQoKj9L3cxUL/XmEBjAnbwfs+61HGSjf8yS8Uon/0oDXxssJyJjnrzAJT7K5G+Nqf5N5IJiEfhYkfa9wkhWR4fU1ZiC3PZZoMrGurGxWAwIs4No2LlBkZXUtAC31jISWODBAahGcaRjPgHFVaDxQ+gEQFse0Aa0mWA7KCRiSbBC89Brx837AREWFa7t14UAi01BLcPSkbAIbIv1SmM+E3k7HdN6rXTjz2h7Na5DOCS4z1LgujXW460WQIDAQAB")
        , ("1a", "hhzKwBDYADu6gQdDrP2AgQJmAE5iIbEqpPmLLZI9w72rLTCCqob9sw==")
        ]
    , sIsBridge = True
    }
 ]

-- ---------------------------------------------------------------------

data Defaults = Defaults
  { chanTimeout :: Int
  , seekTimeout :: Int
  , chanAutoAck :: Int
  , chanResend :: Int
  , chanOutBuf :: Int
  , chanInBuf :: Int
  , natTimeout :: Int
  , idleTimeout :: Int
  , linkTimer :: Int
  , linkMax :: Int
  , linkK :: Int
  }

-- ---------------------------------------------------------------------

defaults :: Defaults
defaults = Defaults
  {
  chanTimeout = 10000, -- how long before for ending durable channels w/ no acks
  seekTimeout = 3000, -- shorter tolerance for seeks, is far more lossy
  chanAutoAck = 1000, -- is how often we auto ack if the app isn't generating responses in a durable channel
  chanResend  = 2000, -- resend the last packet after this long if it wasn't acked in a durable channel
  chanOutBuf  = 100, -- max size of outgoing buffer before applying backpressure
  chanInBuf   = 50, -- how many incoming packets to cache during processing/misses
  natTimeout  = 60*1000, -- nat timeout for inactivity
  idleTimeout = 5*60*1000, -- defaults.nat_timeout; -- overall inactivity timeout
  linkTimer   = 55*1000, -- defaults.nat_timeout - (5*1000); -- how often the DHT link maintenance runs
  linkMax     = 256,  -- maximum number of links to maintain overall (minimum one packet per link timer)
  linkK       = 8  -- maximum number of links to maintain per bucket
  }

-- ---------------------------------------------------------------------

switch :: Defaults -> TeleHash Switch
switch def = do
  rng <- io $ initRNG
  let
    sw = Switch
      { swSeeds = []
      , swLocals = []
      , swLines = []
      , swBridges = []
      , swBridgeLine = []
      , swAll = []
      , swBuckets = []
      , swCapacity = []
      , swRels = []
      -- , swRaws = []
      , swPaths = Map.empty
      , swBridgeCache = []

      , swId = Map.empty
      , swCs = Map.empty
      , swKeys = Map.empty

      , swCSets = Map.empty
      , swParts = []

      , swLoad = load
      , swMake = keysgen

      -- configure defaults
      , swNat = False
      , swSeed = True

      , swLanToken = Nothing

      -- udp socket stuff
      , swPcounter = 1
      , swReceive = receive

      -- outgoing packets to the network
      , swDeliver = deliver
      , swNetworks = Map.fromList [(PathType "relay", (relayPid,relay))]
      , swSend = send
      , swPathSet = pathset

      -- need some seeds to connect to, addSeed({ip:"1.2.3.4", port:5678, public:"PEM"})
      , swAddSeed = addSeed


      --  map a hashname to an object, whois(hashname)
      , swWhois = whois
      , swWhokey = whokey

      , swStart = start

      -- connect to the network, online(callback(err))
      , swIsOnline = False
      , swOnline = online

      -- handle new reliable channels coming in from anyone
      , swListen = listen
      , swRaw = raw

      --  internal listening unreliable channels
      , swRaws = Map.fromList
               [ ("peer",inPeer)
               , ("connect",inConnect)
               , ("seek",inSeek)
               , ("path",inPath)
               , ("bridge",inBridge)
               , ("link",inLink)
               ]

      -- primarily internal, to seek/connect to a hashname
      , swSeek = seek
      , swBridge = bridge

      -- for modules
      , swPencode = pencode
      , swPdecode = pdecode
      , swIsLocalIP = isLocalIP
      , swRandomHEX = randomHEX
      , swUriparse = uriparse

      -- for modules
      , swIsHashname = isHashName
      , swWraps = channelWraps
      , swWaits = []
      , swWaiting = Nothing
      , swWait = wait

      -- crypto
      , swRNG = rng
      }
  put sw
  linkLoop -- should never return
  sw' <- get
  return sw'

initRNG :: IO SystemRNG
initRNG = do
  pool <- createEntropyPool
  return $ cprgCreate pool

-- ---------------------------------------------------------------------
{-
  self.load = function(id)
  {
    if(!id || !id.parts) return "bad keys";
    self.parts = id.parts;
    self.id = id;
    var err = loadkeys(self);
    if(err) return err;
    if(Object.keys(self.cs).length == 0) return "missing cipher sets";
    self.hashname = parts2hn(self.parts);
    return false;
  }
-}

load :: String -> Bool
load s = undefined

-- ---------------------------------------------------------------------

receive :: Packet -> Path -> IO ()
receive = undefined
{-
// self.receive, raw incoming udp data
function receive(msg, path)
{
  var self = this;
  var packet = pdecode(msg);
  if(!packet) return warn("failed to decode a packet from", path, msg.toString());
  if(packet.length == 2) return; // empty packets are NAT pings

  packet.sender = path;
  packet.id = self.pcounter++;
  packet.at = Date.now();
  debug(">>>>",Date(),msg.length, packet.head_length, packet.body_length,[path.type,path.ip,path.port,path.id].join(","));

  // handle any LAN notifications
  if(packet.js.type == "lan") return inLan(self, packet);
  if(packet.js.type == "seed") return inLanSeed(self, packet);

  // either it's an open
  if(packet.head.length == 1)
  {
    var open = deopenize(self, packet);
    if (!open || !open.verify) return warn("couldn't decode open",open);
    if (!isHEX(open.js.line, 32)) return warn("invalid line id enclosed",open.js.line);
    if(open.js.to !== self.hashname) return warn("open for wrong hashname",open.js.to);
    var csid = partsMatch(self.parts,open.js.from);
    if(csid != open.csid) return warn("open with mismatch CSID",csid,open.csid);

    var from = self.whokey(open.js.from,open.key);
    if (!from) return warn("invalid hashname", open.js.from);

    // make sure this open is legit
    if (typeof open.js.at != "number") return warn("invalid at", open.js.at);

    // duplicate open and there's newer line packets, ignore it
    if(from.openAt && open.js.at <= from.openAt && from.lineAt == from.openAt) return;

    // open is legit!
    debug("inOpen verified", from.hashname);
    from.recvAt = Date.now();

    // add this path in
    path = from.pathIn(path);

    // if new line id, reset incoming channels
    if(open.js.line != from.lineIn)
    {
      debug("new line");
      Object.keys(from.chans).forEach(function(id){
        if(id % 2 == from.chanOut % 2) return; // our ids
        if(from.chans[id]) from.chans[id].fail({js:{err:"reset"}});
        delete from.chans[id];
      });
    }

    // update values
    var line = {};
    from.openAt = open.js.at;
    from.lineIn = open.js.line;

    // send an open back
    self.send(path,from.open(),from);

    // line is open now!
    from.csid = open.csid;
    self.CSets[open.csid].openline(from, open);
    debug("line open",from.hashname,from.lineOut,from.lineIn);
    self.lines[from.lineOut] = from;

    // resend the last sent packet again
    if (from.lastPacket) {
      var packet = from.lastPacket;
      delete from.lastPacket;
      from.send(packet)
    }

    // if it was a lan seed, add them
    if(from.local && self.locals.indexOf(from) == -1) self.locals.push(from);

    return;
  }

  // or it's a line
  if(packet.head.length == 0)
  {
    var lineID = packet.body.slice(0,16).toString("hex");
    var line = packet.from = self.lines[lineID];

    // a matching line is required to decode the packet
    if(!line) {
      if(!self.bridgeLine[lineID]) return debug("unknown line received", lineID, JSON.stringify(packet.sender));
      debug("BRIDGE",JSON.stringify(self.bridgeLine[lineID]),lineID);
      var id = crypto.createHash("sha256").update(packet.body).digest("hex")
      if(self.bridgeCache[id]) return; // drop duplicates
      self.bridgeCache[id] = true;
      // flat out raw retransmit any bridge packets
      return self.send(self.bridgeLine[lineID],msg);
    }

    // decrypt and process
    var err;
    if((err = self.CSets[line.csid].delineize(line, packet))) return debug("couldn't decrypt line",err,packet.sender);
    line.lineAt = line.openAt;
    line.receive(packet);
    return;
  }

  if(Object.keys(packet.js).length > 0) warn("dropping incoming packet of unknown type", packet.js, packet.sender);
}

-}

-- ---------------------------------------------------------------------
{-
  self.deliver = function(type, callback){ self.networks[type] = callback};
-}

deliver :: String -> () -> ()
deliver = undefined

-- ---------------------------------------------------------------------
{-
  self.networks["relay"] = function(path,msg){
    if(path.relay.ended) return debug("dropping dead relay");
    path.relay.send({body:msg});
  };
-}
relay :: PathType -> Packet -> Maybe To -> TeleHash ()
relay path msg _ = undefined

-- ---------------------------------------------------------------------

{-
  self.send = function(path, msg, to){
    if(!msg) return debug("send called w/ no packet, dropping");
    if(to) path = to.pathOut(path);
    if(!path) return debug("send called w/ no valid network, dropping");
    debug("<<<<",Date(),msg.length,[path.type,path.ip,path.port,path.id].join(","),to&&to.hashname);

    // try to send it via a supported network
    if(self.networks[path.type]) self.networks[path.type](path,msg,to);

    // if the path has been active in or out recently, we're done
    if(Date.now() - path.lastIn < defaults.nat_timeout || Date.now() - path.lastOut < (defaults.chan_timeout / 2)) return;

    // no network support or unresponsive path, try a bridge
    self.bridge(path,msg,to);
  };
-}

send :: PathType -> Packet -> Maybe To -> TeleHash ()
send mpath msg mto = do
  sw <- get
  let path = case mto of
       Just to -> (pathOut to) mpath
       Nothing -> mpath
    -- if(!path) return debug("send called w/ no valid network, dropping");
    -- debug("<<<<",Date(),msg.length,[path.type,path.ip,path.port,path.id].join(","),to&&to.hashname);
  logT $ "<<<<"
  -- try to send it via a supported network
  -- if(self.networks[path.type]) self.networks[path.type](path,msg,to);
  mpid <- case Map.lookup path (swNetworks sw) of
    Nothing -> return Nothing
    Just (pid,sender) -> do
      sender path msg mto
      return $ Just pid


  case mpid of
    Nothing -> do
       -- debug("send called w/ no valid network, dropping");
       logT "send called w/ no valid network, dropping"
       return ()
    Just pid -> do
       -- if the path has been active in or out recently (ideally by the
       -- send process), we're done
       timeNow <- io getClockTime
       sw' <- get -- get a fresh one, send may have updated it
       let Just p = Map.lookup pid (swPaths sw')
       if (isTimeOut timeNow (pLastIn p)  (natTimeout defaults)) ||
          (isTimeOut timeNow (pLastOut p) (chanTimeout defaults))
         then
           -- no network support or unresponsive path, try a bridge
           (swBridge sw') path msg mto
         else
           return ()

{-
    if(to) path = to.pathOut(path);
    if(!path) return debug("send called w/ no valid network, dropping");
    debug("<<<<",Date(),msg.length,[path.type,path.ip,path.port,path.id].join(","),to&&to.hashname);

    // try to send it via a supported network
    if(self.networks[path.type]) self.networks[path.type](path,msg,to);

    // if the path has been active in or out recently, we're done
    if(Date.now() - path.lastIn < defaults.nat_timeout || Date.now() - path.lastOut < (defaults.chan_timeout / 2)) return;

    // no network support or unresponsive path, try a bridge
    self.bridge(path,msg,to);
  };
-}


-- ---------------------------------------------------------------------

{-
  self.pathSet = function(path)
  {
    var updated = (self.paths[path.type] && JSON.stringify(self.paths[path.type]) == JSON.stringify(path));
    self.paths[path.type] = path;
    // if ip4 and local ip, set nat mode
    if(path.type == "ipv4") self.nat = isLocalIP(path.ip);
    // trigger pings if our address changed
    if(self.isOnline && !updated)
    {
      debug("local network updated, checking links")
      linkMaint(self);
    }
  }

-}

pathset :: Path -> IO ()
pathset path = undefined

-- ---------------------------------------------------------------------

{-
function addSeed(arg) {
  var self = this;
  if(!arg.parts) return warn("invalid args to addSeed",arg);
  var seed = self.whokey(arg.parts,false,arg.keys);
  if(!seed) return warn("invalid seed info",arg);
  if(Array.isArray(arg.paths)) arg.paths.forEach(function(path){
    path = seed.pathGet(path);
    path.seed = true;
  });
  seed.isSeed = true;
  self.seeds.push(seed);
}
-}

addSeed :: SeedInfo -> TeleHash ()
addSeed args = do
  sw <- get
  -- seed <- (swWhoKey sw) 
  return ()

-- ---------------------------------------------------------------------

whois :: HashName -> TeleHash (Maybe HashContainer)
whois = undefined

{-
// this creates a hashname identity object (or returns existing)
function whois(hashname)
{
  var self = this;
  // validations
  if(!hashname) { warn("whois called without a hashname", hashname, new Error().stack); return false; }
  if(typeof hashname != "string") { warn("wrong type, should be string", typeof hashname,hashname); return false; }
  hashname = hashname.split(",")[0]; // convenience if an address is passed in
  if(!isHEX(hashname, 64)) { warn("whois called without a valid hashname", hashname); return false; }

  // never return ourselves
  if(hashname === self.hashname) { debug("whois called for self"); return false; }

  var hn = self.all[hashname];
  if(hn) return hn;

  // make a new one
  hn = self.all[hashname] = {hashname:hashname, chans:{}, self:self, paths:[], isAlive:0};
  hn.at = Date.now();
  hn.bucket = dhash(self.hashname, hashname);
  if(!self.buckets[hn.bucket]) self.buckets[hn.bucket] = [];

  // to create a new channels to this hashname
  var sort = [self.hashname,hashname].sort();
  hn.chanOut = (sort[0] == self.hashname) ? 2 : 1;
  hn.start = channel;
  hn.raw = raw;

  hn.pathGet = function(path)
  {
    if(["ipv4","ipv6","http","relay","webrtc","local"].indexOf(path.type) == -1)
    {
      warn("unknown path type", JSON.stringify(path));
      return path;
    }

    var match = pathMatch(path, hn.paths);
    if(match) return match;

    // preserve original
    if(!path.json) path.json = JSON.parse(JSON.stringify(path));

    debug("adding new path",hn.paths.length,JSON.stringify(path.json));
    info(hn.hashname,path.type,JSON.stringify(path.json));
    hn.paths.push(path);

    // always default to minimum priority
    if(typeof path.priority != "number") path.priority = (path.type=="relay")?-1:0;

    // track overall if they have a public IP network
    if(!isLocalPath(path)) hn.isPublic = true;

    return path;
  }

  hn.pathOut = function(path)
  {
    if(!path) return false;
    path = hn.pathGet(path);
    if(path.type == "relay" && path.relay.ended) return hn.pathEnd(path);
    path.lastOut = Date.now();
    if(!pathValid(hn.to) && pathValid(path)) hn.to = path;
    return path;
  }

  hn.pathEnd = function(path)
  {
    if(path.seed) return false; // never remove a seed-path
    if(hn.to == path) hn.to = false;
    path.gone = true;
    var index = hn.paths.indexOf(path);
    if(index >= 0) hn.paths.splice(index,1);
    debug("PATH END",JSON.stringify(path.json));
    return false;
  }

  // manage network information consistently, called on all validated incoming packets
  hn.pathIn = function(path)
  {
    path = hn.pathGet(path);

    // first time we've seen em
    if(!path.lastIn && !path.lastOut)
    {
      debug("PATH INNEW",JSON.stringify(path.json),hn.paths.map(function(p){return JSON.stringify(p.json)}));
      // for every new incoming path, trigger a sync (delayed so caller can continue/respond first)
      setTimeout(hn.sync,1);

      // update public ipv4 info
      if(path.type == "ipv4" && !isLocalIP(path.ip))
      {
        hn.ip = path.ip;
        hn.port = path.port;
      }

      // cull any invalid paths of the same type
      hn.paths.forEach(function(other){
        if(other == path) return;
        if(other.type != path.type) return;
        if(!pathValid(other)) hn.pathEnd(other);
      });

      // "local" custom paths, we must bridge for
      if(path.type == "local") hn.bridging = true;

      // track overall if we trust them as local
      if(isLocalPath(path)) hn.isLocal = true;
    }

    path.lastIn = Date.now();
    self.recvAt = Date.now();

    // end any active relay
    if(hn.to && hn.to.type == "relay" && path.type != "relay") hn.to.relay.fail();

    // update default if better
    if(!pathValid(hn.to) || pathValid(path)) hn.to = path;
    hn.alive = pathValid(hn.to);

    return path;
  }

  // try to send a packet to a hashname, doing whatever is possible/necessary
  hn.send = function(packet){
    // if there's a line, try sending it via a valid network path!
    if(hn.lineIn)
    {
      debug("line sending",hn.hashname,hn.lineIn);
      var lined = packet.msg || self.CSets[hn.csid].lineize(hn, packet);
      hn.sentAt = Date.now();

      // directed packets are preferred, just dump and done
      if(packet.to) return self.send(packet.to, lined, hn);

      // send to the default best path
      if(hn.to) self.send(hn.to, lined, hn);

      // if it was good, we're done, if not fall through
      if(pathValid(hn.to)) return;
    }

    // we've fallen through, either no line, or no valid paths
    debug("alive failthrough",hn.sendSeek,Object.keys(hn.vias||{}));
    hn.alive = false;
    hn.lastPacket = packet; // will be resent if/when an open is received

    // always send to all known paths, increase resiliency
    hn.paths.forEach(function(path){
      self.send(path, hn.open(), hn);
    });

    // also try using any via informtion to create a new line
    function vias()
    {
      if(!hn.vias) return;
      hn.sentOpen = false; // whenever we send a peer, we'll always need to resend any open regardless
      // try to connect vias
      var todo = hn.vias;
      delete hn.vias; // never use more than once
      Object.keys(todo).forEach(function(via){
        var address = todo[via].split(",");
        if(address.length <= 1) return;
        if(address.length == 4 && address[2].split(".").length == 4 && parseInt(address[3]) > 0)
        {
          // NAT hole punching
          var path = {type:"ipv4",ip:address[2],port:parseInt(address[3])};
          self.send(path,pencode());
          // if possibly behind the same NAT, set flag to allow/ask to relay a local path
          if(self.nat && address[2] == (self.paths.pub4 && self.paths.pub4.ip)) hn.isLocal = true;
        }
        // send the peer request
        self.whois(via).peer(hn.hashname, address[1]);
      });
    }

    // if there's via information, just try that
    if(hn.vias) return vias();


    // never too fast, worst case is to try to seek again
    if(!hn.sendSeek || (Date.now() - hn.sendSeek) > 5000)
    {
      hn.sendSeek = Date.now();
      self.seek(hn, function(err){
        if(!hn.lastPacket) return; // packet was already sent elsewise
        vias(); // process any new vias
      });
    }

  }

  // handle all incoming line packets
  hn.receive = function(packet)
  {
//    if((Math.floor(Math.random()*10) == 4)) return warn("testing dropping randomly!");
    if(!packet.js || typeof packet.js.c != "number") return warn("dropping invalid channel packet",packet.js);

    debug("LINEIN",JSON.stringify(packet.js));
    hn.recvAt = Date.now();
    // normalize/track sender network path
    packet.sender = hn.pathIn(packet.sender);

    // find any existing channel
    var chan = hn.chans[packet.js.c];
    if(chan === false) return; // drop packet for a closed channel
    if(chan) return chan.receive(packet);

    // start a channel if one doesn't exist, check either reliable or unreliable types
    var listening = {};
    if(typeof packet.js.seq == "undefined") listening = self.raws;
    if(packet.js.seq === 0) listening = self.rels;
    if(!listening[packet.js.type])
    {
      // bounce error
      if(!packet.js.end && !packet.js.err)
      {
        warn("bouncing unknown channel/type",packet.js);
        var err = (packet.js.type) ? "unknown type" : "unknown channel"
        hn.send({js:{err:err,c:packet.js.c}});
      }
      return;
    }

    // verify incoming new chan id
    if(packet.js.c % 2 == hn.chanOut % 2) return warn("channel id incorrect",packet.js.c,hn.chanOut)

    // make the correct kind of channel;
    var kind = (listening == self.raws) ? "raw" : "start";
    var chan = hn[kind](packet.js.type, {bare:true,id:packet.js.c}, listening[packet.js.type]);
    chan.receive(packet);
  }

  hn.chanDone = function(id)
  {
    hn.chans[id] = false;
  }

  // track who told us about this hn
  hn.via = function(from, address)
  {
    if(typeof address != "string") return warn("invalid see address",address);
    if(!hn.vias) hn.vias = {};
    if(hn.vias[from.hashname]) return;
    hn.vias[from.hashname] = address;
  }

  // just make a seek request conveniently
  hn.seek = function(hashname, callback)
  {
    var bucket = dhash(hn.hashname, hashname);
    var prefix = hashname.substr(0, Math.ceil((255-bucket)/4)+2);
    hn.raw("seek", {timeout:defaults.seek_timeout, retry:3, js:{"seek":prefix}}, function(err, packet, chan){
      callback(packet.js.err,Array.isArray(packet.js.see)?packet.js.see:[]);
    });
  }

  // return our address to them
  hn.address = function(to)
  {
    if(!to) return "";
    var csid = partsMatch(hn.parts,to.parts);
    if(!csid) return "";
    if(!hn.ip) return [hn.hashname,csid].join(",");
    return [hn.hashname,csid,hn.ip,hn.port].join(",");
  }

  // request a new link to them
  hn.link = function(callback)
  {
    if(!callback) callback = function(){}

    var js = {seed:self.seed};
    js.see = self.buckets[hn.bucket].sort(function(a,b){ return a.age - b.age }).filter(function(a){ return a.seed }).map(function(seed){ return seed.address(hn) }).slice(0,8);
    // add some distant ones if none
    if(js.see.length < 8) Object.keys(self.buckets).forEach(function(bucket){
      if(js.see.length >= 8) return;
      self.buckets[bucket].sort(function(a,b){ return a.age - b.age }).forEach(function(seed){
        if(js.see.length >= 8 || !seed.seed || js.see.indexOf(seed.address(hn)) != -1) return;
        js.see.push(seed.address(hn));
      });
    });

    if(self.bridging || hn.bridging) js.bridges = Object.keys(self.networks).filter(function(type){return (["local","relay"].indexOf(type) >= 0)?false:true});

    if(hn.linked)
    {
      hn.linked.send({js:js});
      return callback();
    }

    hn.raw("link", {retry:3, js:js}, function(err, packet, chan){
      inLink(err, packet, chan);
      callback(packet.js.err);
    });
  }

  // send a simple lossy peer request, don't care about answer
  hn.peer = function(hashname, csid)
  {
    if(!csid || !self.parts[csid]) return;
    var js = {"peer":hashname};
    js.paths = hn.pathsOut();
    hn.raw("peer",{js:js, body:getkey(self,csid)}, function(err, packet, chan){
      if(err) return;
      if(!packet.body) return warn("relay in w/ no body",packet.js,packet.from.hashname);
      // create a network path that maps back to this channel
      var path = {type:"relay",relay:chan,json:{type:"relay",relay:packet.from.hashname}};
      if(packet.js.bridge) path = packet.sender; // sender is offering to bridge, use them!
      self.receive(packet.body, path);
    });
  }

  // return the current open packet
  hn.open = function()
  {
    if(!hn.parts) return false; // can't open if no key
    if(hn.opened) return hn.opened;
    hn.opened = openize(self,hn);
    return hn.opened;
  }

  // generate current paths array to them, for peer and paths requests
  hn.pathsOut = function()
  {
    var paths = [];
    if(self.paths.pub4) paths.push({type:"ipv4", ip:self.paths.pub4.ip, port:self.paths.pub4.port});
    if(self.paths.pub6) paths.push({type:"ipv6", ip:self.paths.pub6.ip, port:self.paths.pub6.port});
    if(self.paths.http)
    {
      if(self.paths.http.http) paths.push({type:"http", http:self.paths.http.http});
      else if(self.paths.pub4) paths.push({type:"http", http:"http://"+self.paths.pub4.ip+":"+self.paths.http.port});
    }
    if(self.paths.webrtc) paths.push({type:"webrtc"});
    if(hn.isLocal)
    {
      if(self.paths.lan4) paths.push({type:"ipv4", ip:self.paths.lan4.ip, port:self.paths.lan4.port});
      if(self.paths.lan6) paths.push({type:"ipv6", ip:self.paths.lan6.ip, port:self.paths.lan6.port});
    }
    return paths;
  }

  // send a full network path sync
  hn.sync = function()
  {
    debug("SYNCING",hn.hashname,hn.paths.map(function(p){return JSON.stringify(p.json)}));

    // compose all of our known paths we can send to them
    var paths = hn.pathsOut();

    // check all paths at once
    hn.paths.forEach(function(path){
      debug("PATHLOOP",hn.paths.length,JSON.stringify(path.json));
      var js = {};
      if(["relay","local"].indexOf(path.type) == -1) js.path = path.json;
      // our outgoing priority of this path
      js.priority = (path.type == "relay") ? 0 : 1;
      if(paths.length > 0) js.paths = paths;
      var lastIn = path.lastIn;
      hn.raw("path",{js:js, timeout:3000, to:path}, function(err, packet){
        // when it actually errored and hasn't been active, invalidate it
        if(err && err !== true && path.lastIn == lastIn) path.lastIn = 0;
        else inPath(true, packet); // handles any response .priority and .paths
      });
    });
  }

  return hn;
}

-}

-- ---------------------------------------------------------------------
{-
function whokey(parts, key, keys)
{
  var self = this;
  if(typeof parts != "object") return false;
  var csid = partsMatch(self.parts,parts);
  if(!csid) return false;
  hn = self.whois(parts2hn(parts));
  if(!hn) return false;
  hn.parts = parts;
  if(keys) key = keys[csid]; // convenience for addSeed
  var err = loadkey(self,hn,csid,key);
  if(err)
  {
    warn("whokey err",hn.hashname,err);
    return false;
  }
  return hn;
}

-}

whokey :: Parts -> String -> Map.Map String String -> TeleHash (Maybe HashName)
whokey parts key keys = do
  sw <- get
  let mcsid = partsMatch (swParts sw) parts
  case mcsid of
    Nothing -> return Nothing
    Just csid -> do
      mhn <- (swWhois sw) (parts2hn parts)
      case mhn of
        Nothing -> return Nothing
        Just hn -> do
          let hn' = hn {hcParts = parts}
              key' = case Map.lookup csid keys of
                Nothing -> key
                Just k -> k
          (hn'',ok) <- loadkey hn' csid key'
          return undefined

-- WIP continue here

-- ---------------------------------------------------------------------

{-
function partsMatch(parts1, parts2)
{
  if(typeof parts1 != "object" || typeof parts2 != "object") return false;
  var ids = Object.keys(parts1).sort();
  var csid;
  while(csid = ids.pop()) if(parts2[csid]) return csid;
  return false;
}

-}

partsMatch :: Parts -> Parts -> Maybe String
partsMatch parts1 parts2 = r
  where
    ids = sort $ map fst parts1
    p2 = Set.fromList $ map fst parts2
    common = filter (\k -> Set.member k p2) ids
    r = if common == [] then Nothing
                        else Just $ head common

-- ---------------------------------------------------------------------

{-
  self.start = function(hashname,type,arg,cb)
  {
    var hn = self.whois(hashname);
    if(!hn) return cb("invalid hashname");
    return hn.start(type,arg,cb);
  }

-}
start :: String -> String -> String -> () -> IO ()
start hashname typ arg cb = undefined

-- ---------------------------------------------------------------------

online :: TeleHash () -> TeleHash ()
online callback = do
  sw <- get
  if swWaits sw /= []
    then put $ sw {swWaiting = Just (online callback)}
    else do
      setIsOnline True
      -- ping lan
      -- self.lanToken = randomHEX(16);
      token <- randomHEX 16
      setLanToken $ token
      (swSend sw) (PathType "lan") (pencode Telex Body) Nothing

      case (swSeeds sw) of
        [] -> do
          logT "no seeds"
          callback
        dones -> do
          let
            -- safely callback only once or when all seeds return
            done = undefined
          -- forM dones $ \ seed -> do
            -- fn = do undefined
            -- (sLink seed) fn
          return undefined
-- WIP here
{-

function online(callback)
{
  var self = this;
  if(self.waits.length > 0) return self.waiting = function(){self.online(callback)};
  self.isOnline = true;
  // ping lan
  self.lanToken = randomHEX(16);
  self.send({type:"lan"}, pencode({type:"lan",lan:self.lanToken,from:self.parts}));

  var dones = self.seeds.length;
  if(!dones) {
    warn("no seeds");
    return callback(null,0);
  }

  // safely callback only once or when all seeds return
  function done()
  {
    if(!dones) return; // already called back
    var alive = self.seeds.filter(function(seed){return seed.alive}).length;
    if(alive)
    {
      callback(null,alive);
      dones = 0;
      return;
    }
    dones--;
    // failed
    if(!dones) callback("offline",0);
  }

  self.seeds.forEach(function(seed){
    seed.link(function(){
      if(seed.alive) seed.sync();
      done();
    });
  });
}

-}
  return ()

-- ---------------------------------------------------------------------

-- | Set the swIsOnline flag
setIsOnline :: Bool -> TeleHash ()
setIsOnline v = do
  sw <- get
  put $ sw {swIsOnline = v}

-- | Set the swLanToken value
setLanToken :: String -> TeleHash ()
setLanToken v = do
  sw <- get
  put $ sw {swLanToken = Just v}

-- ---------------------------------------------------------------------
{-
  self.listen = function(type, callback){
    if(typeof type != "string" || typeof callback != "function") return warn("invalid arguments to listen");
    if(type.substr(0,1) !== "_") type = "_"+type;
    self.rels[type] = callback;
  };
-}

listen :: String -> () -> IO ()
listen typ callback = undefined

-- ---------------------------------------------------------------------
{-
  self.raw = function(type, callback){
    if(typeof type != "string" || typeof callback != "function") return warn("invalid arguments to raw");
    self.raws[type] = callback;
  };

-}

raw :: String -> () -> IO ()
raw typ callback = undefined

-- ---------------------------------------------------------------------

inPeer :: String -> Packet -> Channel -> IO ()
inPeer = undefined
{-
// be the middleman to help NAT hole punch
function inPeer(err, packet, chan)
{
  if(err) return;
  var self = packet.from.self;
  if(chan.relay) return relay(self, chan, chan.relay, packet);

  if(!isHEX(packet.js.peer, 64)) return;
  var peer = self.whois(packet.js.peer);
  if(!peer || !peer.lineIn) return; // these happen often as lines come/go, ignore dead peer requests
  var js = {from:packet.from.parts};

  // sanity on incoming paths array
  if(!Array.isArray(packet.js.paths)) packet.js.paths = [];

  // insert in incoming IP path
  if(packet.sender.type.indexOf("ip") == 0) packet.js.paths.push(packet.sender.json);

  // load/cleanse all paths
  js.paths = [];
  packet.js.paths.forEach(function(path){
    if(typeof path.type != "string") return;
    if(pathMatch(js.paths,path)) return; // duplicate
    if(isLocalPath(path) && !peer.isLocal) return; // don't pass along local paths to public
    js.paths.push(path);
  });

  // must bundle the senders key so the recipient can open them
  chan.relay = peer.raw("connect",{js:js, body:packet.body},function(err, packet, chan2){
    if(err) return;
    relay(self, chan2, chan, packet);
  });
}
-}

-- ---------------------------------------------------------------------

inConnect :: String -> Packet -> Channel -> IO ()
inConnect = undefined

{-
// someone's trying to connect to us, send an open to them
function inConnect(err, packet, chan)
{
  if(err || !packet.body) return;
  var self = packet.from.self;

  // if this channel is acting as a relay
  if(chan.relay)
  {
    // create a virtual network path that maps back to this channel
    var path = {type:"relay",relay:chan,json:{type:"relay",relay:packet.from.hashname}};
    if(packet.js.bridge) path = packet.sender; // sender is offering to bridge, use them!
    self.receive(packet.body, path);
    return;
  }

  var to = chan.relay = self.whokey(packet.js.from,packet.body);
  if(!chan.relay) return warn("invalid connect request from",packet.from.hashname,packet.js);

  // try the suggested paths
  if(Array.isArray(packet.js.paths)) packet.js.paths.forEach(function(path){
    if(typeof path.type != "string") return debug("bad path",JSON.stringify(path));
    self.send(path,to.open(),to);
  });

  // send back an open through the connect too
  chan.send({body:to.open()});
}

-}
-- ---------------------------------------------------------------------

inSeek :: String -> Packet -> Channel -> IO ()
inSeek = undefined
{-

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
    if(link.seed || link.hashname.substr(seek.length) == seek)
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

inPath :: String -> Packet -> Channel -> IO ()
inPath = undefined

{-
// update/respond to network state
function inPath(err, packet, chan)
{
  var self = packet.from.self;

  // check/try any alternate paths
  if(Array.isArray(packet.js.paths)) packet.js.paths.forEach(function(path){
    if(typeof path.type != "string") return; // invalid
    // don't send to ones we know about
    if(pathMatch(path, packet.from.paths)) return;
    // a new one, experimentally send it a path
    packet.from.raw("path",{js:{priority:1},to:path}, inPath);
  });

  // if path info from a seed, update our public ip/port
  if(packet.from.isSeed && typeof packet.js.path == "object" && packet.js.path.type == "ipv4" && !isLocalIP(packet.js.path.ip))
  {
    debug("updating public ipv4",JSON.stringify(self.paths.pub4),JSON.stringify(packet.js.path));
    self.pathSet({type:"pub4", ip:packet.js.path.ip, port:parseInt(packet.js.path.port)})
  }

  // update any optional priority information
  if(typeof packet.js.priority == "number"){
    packet.sender.priority = packet.js.priority;
    if(packet.from.to && packet.sender.priority > packet.from.to.priority) packet.from.to = packet.sender; // make the default!
  }

  if(err) return; // bye bye bye!

  // need to respond, prioritize everything above relay
  var priority = (packet.sender.type == "relay") ? 0 : 2;

  // if bridging, and this path is from the bridge, flag it for lower priority
  if(packet.from.bridge && pathMatch(packet.sender, self.whois(packet.from.bridge).paths)) priority = 1;

  chan.send({js:{end:true, priority:priority, path:packet.sender.json}});
}

-}

-- ---------------------------------------------------------------------

inBridge :: String -> Packet -> Channel -> IO ()
inBridge = undefined

{-
// handle any bridge requests, if allowed
function inBridge(err, packet, chan)
{
  if(err) return;
  var self = packet.from.self;

  // ensure valid request
  if(!isHEX(packet.js.to,32) || !isHEX(packet.js.from,32) || typeof packet.js.path != "object") return warn("invalid bridge request",JSON.stringify(packet.js),packet.from.hashname);

  // must be allowed either globally or per hashname
  if(!self.bridging && !packet.from.bridging) return chan.send({js:{err:"not allowed"}});

  // don't bridge for types we don't know
  if(!self.networks[packet.js.path.type]) return chan.send({js:{err:"bad path"}});

  // ignore fool line ids
  if(self.lines[packet.js.to] || self.lines[packet.js.from]) return chan.send({js:{err:"bad line"}});

  // set up the actual bridge paths
  debug("BRIDGEUP",JSON.stringify(packet.js));
  self.bridgeLine[packet.js.to] = packet.js.path;
  self.bridgeLine[packet.js.from] = packet.sender;

  chan.send({js:{end:true}});
}

-}

-- ---------------------------------------------------------------------

inLink :: String -> Packet -> Channel -> IO ()
inLink = undefined

{-

// accept a dht link
function inLink(err, packet, chan)
{
  if(err) return;
  var self = packet.from.self;
  chan.timeout(defaults.nat_timeout*2); // two NAT windows to be safe

  // add in this link
  if(!packet.from.age) packet.from.age = Date.now();
  packet.from.linked = chan;
  packet.from.seed = packet.js.seed;
  if(self.buckets[packet.from.bucket].indexOf(packet.from) == -1) self.buckets[packet.from.bucket].push(packet.from);

  // send a response if this is a new incoming
  if(!chan.sentAt) packet.from.link();

  // look for any see and check to see if we should create a link
  if(Array.isArray(packet.js.see)) packet.js.see.forEach(function(address){
    if(!address) return; // garbage
    var hn = self.whois(address);
    if(!hn || hn.linked) return;
    if(self.buckets[hn.bucket].length < defaults.link_k) hn.link();
  });

  // check for bridges
  if(Array.isArray(packet.js.bridges)) packet.js.bridges.forEach(function(type){
    if(!self.bridges[type]) self.bridges[type] = {};
    self.bridges[type][packet.from.hashname] = Date.now();
  });

  // let mainteanance handle
  chan.callback = inMaintenance;
}

-}

-- ---------------------------------------------------------------------

seek :: String -> () -> IO ()
seek = undefined

{-
// seek the dht for this hashname
function seek(hn, callback)
{
  var self = this;
  if(typeof hn == "string") hn = self.whois(hn);
  if(!callback) callback = function(){};
  if(!hn) return callback("invalid hashname");

  var did = {};
  var doing = {};
  var queue = [];
  var wise = {};
  var closest = 255;

  // load all seeds and sort to get the top 3
  var seeds = []
  Object.keys(self.buckets).forEach(function(bucket){
    self.buckets[bucket].forEach(function(link){
      if(link.hashname == hn) return; // ignore the one we're (re)seeking
      if(link.seed) seeds.push(link);
    });
  });
  seeds.sort(function(a,b){ return dhash(hn.hashname,a.hashname) - dhash(hn.hashname,b.hashname) }).slice(0,3).forEach(function(seed){
    wise[seed.hashname] = true;
    queue.push(seed.hashname);
  });

  debug("seek starting with",queue,seeds.length);

  // always process potentials in order
  function sort()
  {
    queue = queue.sort(function(a,b){
      return dhash(hn.hashname,a) - dhash(hn.hashname,b)
    });
  }

  // track when we finish
  function done(err)
  {
    // get all the hashnames we used/found and do final sort to return
    Object.keys(did).forEach(function(k){ if(queue.indexOf(k) == -1) queue.push(k); });
    Object.keys(doing).forEach(function(k){ if(queue.indexOf(k) == -1) queue.push(k); });
    sort();
    while(cb = hn.seeking.shift()) cb(err, queue.slice());
  }

  // track callback(s);
  if(!hn.seeking) hn.seeking = [];
  hn.seeking.push(callback);
  if(hn.seeking.length > 1) return;

  // main loop, multiples of these running at the same time
  function loop(onetime){
    if(!hn.seeking.length) return; // already returned
    debug("SEEK LOOP",queue);
    // if nothing left to do and nobody's doing anything, failed :(
    if(Object.keys(doing).length == 0 && queue.length == 0) return done("failed to find the hashname");

    // get the next one to ask
    var mine = onetime||queue.shift();
    if(!mine) return; // another loop() is still running

    // if we found it, yay! :)
    if(mine == hn.hashname) return done();
    // skip dups
    if(did[mine] || doing[mine]) return onetime||loop();
    var distance = dhash(hn.hashname, mine);
    if(distance > closest) return onetime||loop(); // don't "back up" further away
    if(wise[mine]) closest = distance; // update distance if trusted
    doing[mine] = true;
    var to = self.whois(mine);
    to.seek(hn.hashname, function(err, see){
      see.forEach(function(item){
        var sug = self.whois(item);
        if(!sug) return;
        // if this is the first entry and from a wise one, give them wisdom too
        if(wise[to.hashname] && see.indexOf(item) == 0) wise[sug.hashname] = true;
        sug.via(to, item);
        queue.push(sug.hashname);
      });
      sort();
      did[mine] = true;
      delete doing[mine];
      onetime||loop();
    });
  }

  // start three of them
  loop();loop();loop();

  // also force query any locals
  self.locals.forEach(function(local){loop(local.hashname)});
}

-}

-- ---------------------------------------------------------------------

bridge :: PathType -> Packet -> Maybe To -> TeleHash ()
bridge = undefined

{-
// try finding a bridge
function bridge(path, msg, to)
{
  var self = this;
  var packet = pdecode(msg);
  if(packet.head.length) return; // only bridge line packets
  if(!to) return; // require to for line info

  // check for existing bridge
  var existing = pathMatch(path,to.bridges);
  if(existing)
  {
    if(existing.bridged) return self.send(existing.bridged,msg); // leave off to to prevent loops
    existing.bridgeq = msg; // queue most recent packet;
    return;
  }

  if(!self.bridges[path.type]) return;
  debug("bridging",JSON.stringify(path.json),to.hashname);

  // TODO, better selection of a bridge?
  var via;
  Object.keys(self.bridges[path.type]).forEach(function(id){
    if(id == to.hashname) return; // lolz
    var hn = self.whois(id);
    if(hn.alive) via = hn;
  });

  if(!via) return debug("couldn't find a bridge host");

  // stash this so that any more bridge's don't spam
  if(!to.bridges) to.bridges = [];
  path.bridgeq = msg;
  to.bridges.push(path);

  // create the bridge
  via.raw("bridge", {js:{to:to.lineIn,from:to.lineOut,path:path}}, function(end, packet){
    // TODO we can try another one if failed?
    if(end !== true) return debug("failed to create bridge",end,via.hashname);
    // create our mapping!
    path.bridged = packet.sender;
    self.send(packet.sender,path.bridgeq);
    delete path.bridgeq;
  });
}

-}

-- ---------------------------------------------------------------------

{-
function parts2hn(parts)
{
  var rollup = new Buffer(0);
  Object.keys(parts).sort().forEach(function(id){
    rollup = crypto.createHash("sha256").update(Buffer.concat([rollup,new Buffer(id)])).digest();
    rollup = crypto.createHash("sha256").update(Buffer.concat([rollup,new Buffer(parts[id])])).digest();
  });
  return rollup.toString("hex");
}
-}

parts2hn :: Parts -> HashName
parts2hn parts = HN r
  where
    sp = sort parts
    ctx = SHA256.init
    vals = concatMap (\(a,b) -> [BC.pack a,BC.pack b]) sp
    _ = SHA256.updates ctx vals
    bsfinal = SHA256.finalize ctx

    r = BU.toString $ B16.encode bsfinal

testParts2hn = parts2hn (sParts $ head initialSeeds)

-- ---------------------------------------------------------------------

pencode :: Telex -> Body -> Packet
pencode = undefined

{-
// encode a packet
function pencode(js, body)
{
  var head = (typeof js == "number") ? new Buffer(String.fromCharCode(js)) : new Buffer(js?JSON.stringify(js):"", "utf8");
  if(typeof body == "string") body = new Buffer(body, "binary");
  body = body || new Buffer(0);
  var len = new Buffer(2);
  len.writeInt16BE(head.length, 0);
  return Buffer.concat([len, head, body]);
}
-}

-- ---------------------------------------------------------------------

pdecode :: Packet -> (Telex,Body)
pdecode = undefined

{-
// packet decoding
function pdecode(packet)
{
  if(!packet) return undefined;
  var buf = (typeof packet == "string") ? new Buffer(packet, "binary") : packet;

  // read and validate the json length
  var len = buf.readUInt16BE(0);
  if(len > (buf.length - 2)) return undefined;
  var head = buf.slice(2, len+2);
  var body = buf.slice(len + 2);

  // parse out the json
  var js = {};
  if(len > 1)
  {
    try {
      js = JSON.parse(head.toString("utf8"));
    } catch(E) {
      console.log("couldn't parse JS",head.toString("hex"),E,packet.sender);
      return undefined;
    }
  }
  return {js:js, length:buf.length, head:head.toString("binary"), body:body};
}


-}

-- ---------------------------------------------------------------------

{-

function getkey(id, csid)
{
  return id.cs && id.cs[csid] && id.cs[csid].key;
}
-}
-- ---------------------------------------------------------------------

loadkeys :: TeleHash ()
loadkeys = do
  sw <- get
  put $ sw { swCs = Map.empty, swKeys = Map.empty }

  let
    doOne (csid,v) = do
      sw <- get
      let cs' = Map.insert csid Map.empty (swCs sw)
      return ()
      -- sw {swCs
-- WIP carry on here
  mapM_ doOne (swParts sw)


  return ()

{-
function loadkeys(self)
{
  self.cs = {};
  self.keys = {};
  var err = false;
  Object.keys(self.parts).forEach(function(csid){
    self.cs[csid] = {};
    if(!self.CSets[csid]) err = csid+" not supported";
    err = err||self.CSets[csid].loadkey(self.cs[csid], self.id[csid], self.id[csid+"_secret"]);
    self.keys[csid] = self.id[csid];
  });
  return err;
}
-}

-- ---------------------------------------------------------------------

{-
function loadkey(self, id, csid, key)
{
  id.csid = csid;
  return self.CSets[csid].loadkey(id, key);
}
-}

loadkey :: HashContainer -> String -> String -> TeleHash (HashContainer,Bool)
loadkey id1 csid key = do
  sw <- get
  let id1' = id1 { hcCsid = csid }
  let set = Map.lookup csid (swCSets sw)
  case set of
    Nothing -> do
      logT $ "missing CSet for " ++ csid
      return (id1,False)
    Just cs -> do
      (csLoadkey cs) id1' key Nothing


-- ---------------------------------------------------------------------

{-
function keysgen(cbDone,cbStep)
{
  var self = this;
  var ret = {parts:{}};
  var todo = Object.keys(self.CSets);
  if(todo.length == 0) return cbDone("no sets supported");
  function pop(err)
  {
    if(err) return cbDone(err);
    var csid = todo.pop();
    if(!csid){
      self.load(ret);
      return cbDone(null, ret);
    }
    self.CSets[csid].genkey(ret,pop,cbStep);
  }
  pop();
}
-}
keysgen :: () -> () -> IO ()
keysgen cbDone cbStep = undefined


-- ---------------------------------------------------------------------

isLocalIP :: String -> Bool
isLocalIP = undefined

{-
// return if an IP is local or public
function isLocalIP(ip)
{
  // ipv6 ones
  if(ip.indexOf(":") >= 0)
  {
    if(ip.indexOf("::") == 0) return true; // localhost
    if(ip.indexOf("fc00") == 0) return true;
    if(ip.indexOf("fe80") == 0) return true;
    return false;
  }

  var parts = ip.split(".");
  if(parts[0] == "0") return true;
  if(parts[0] == "127") return true; // localhost
  if(parts[0] == "10") return true;
  if(parts[0] == "192" && parts[1] == "168") return true;
  if(parts[0] == "172" && parts[1] >= 16 && parts[1] <= 31) return true;
  if(parts[0] == "169" && parts[1] == "254") return true; // link local
  return false;
}

-}

-- ---------------------------------------------------------------------

randomHEX :: Int -> TeleHash String
randomHEX len = do
  sw <- get
  let (bytes,newRNG) = cprgGenerate len (swRNG sw)
  put $ sw {swRNG = newRNG}
  return $ BU.toString $ B16.encode bytes 
{-
// return random bytes, in hex
function randomHEX(len)
{
  return crypto.randomBytes(len).toString("hex");
}
-}

-- ---------------------------------------------------------------------

uriparse :: String -> String
uriparse = undefined

{-
var urllib = require("url");
function uriparse(uri)
{
  // node's uri parser enforces dns max 63 chars per label, grr!
  if(typeof uri !== "string") uri = "";
  var hashname = uri.match(/[0-9A-Fa-f]{64}/);
  if(!hashname) return urllib.parse(uri);
  var full = hashname[0];
  var part = full.substr(0,32);
  var u = urllib.parse(uri.replace(full,part));
  if(u.hostname != part) return urllib.parse(uri); // hashname was not the hostname
  Object.keys(u).forEach(function(k){
    if(typeof u[k] != "string") return;
    u[k] = u[k].replace(part,full);
  });
  return u;
}


-}

-- ---------------------------------------------------------------------

isHashName :: String -> String
isHashName = undefined

{-
  self.isHashname = function(hex){return isHEX(hex, 64)};
-}

-- ---------------------------------------------------------------------

{-
/* CHANNELS API
hn.channel(type, arg, callback)
  - used by app to create a reliable channel of given type
  - arg contains .js and .body for the first packet
  - callback(err, arg, chan, cbDone)
    - called when any packet is received (or error/fail)
    - given the response .js .body in arg
    - cbDone when arg is processed
    - chan.send() to send packets
    - chan.wrap(bulk|stream) to modify interface, replaces this callback handler
      - chan.bulk(str, cbDone) / onBulk(cbDone(err, str))
      - chan.read/write
hn.raw(type, arg, callback)
  - arg contains .js and .body to create an unreliable channel
  - callback(err, arg, chan)
    - called on any packet or error
    - given the response .js .body in arg
    - chan.send() to send packets

self.channel(type, callback)
  - used to listen for incoming reliable channel starts
  - callback(err, arg, chan, cbDone)
    - called for any answer or subsequent packets
    - chan.wrap() to modify
self.raw(type, callback)
  - used to listen for incoming unreliable channel starts
  - callback(err, arg, chan)
    - called for any incoming packets
*/
-}

channelWraps :: IO ()
channelWraps = undefined

{-
// these are called once a reliable channel is started both ways to add custom functions for the app
var channelWraps = {
  "bulk":function(chan){
    // handle any incoming bulk flow
    var bulkIn = "";
    chan.callback = function(end, packet, chan, cb)
    {
      cb();
      if(packet.body) bulkIn += packet.body;
      if(!chan.onBulk) return;
      if(end) chan.onBulk(end!==true?end:false, bulkIn);
    }
    // handle (optional) outgoing bulk flow
    chan.bulk = function(data, callback)
    {
      // break data into chunks and send out, no backpressure yet
      while(data)
      {
        var chunk = data.substr(0,1000);
        data = data.substr(1000);
        var packet = {body:chunk};
        if(!data) packet.callback = callback; // last packet gets confirmed
        chan.send(packet);
      }
      chan.end();
    }
  }
}

-}

-- ---------------------------------------------------------------------

wait :: Bool -> IO ()
wait = undefined

{-
  self.wait = function(bool){
    if(bool) return self.waits.push(true);
    self.waits.pop();
    if(self.waiting && self.waits.length == 0) self.waiting();
  }

-}

-- ---------------------------------------------------------------------

--  do the maintenance work for links
linkLoop :: TeleHash ()
linkLoop = do
  sw <- get
  put sw {swBridgeCache = []} -- reset cache for any bridging
  hnReap
  linkMaint -- ping all of them
  io $ threadDelay $ milliToMicro (linkTimer defaults)
  linkLoop

-- | convert a millisecond value to a microsecond one
milliToMicro :: Num a => a -> a
milliToMicro x = 1000 * x

-- ---------------------------------------------------------------------

hnReap :: TeleHash ()
hnReap = do return ()
{-
// delete any defunct hashnames!
function hnReap(self)
{
  var hn;
  function del(why)
  {
    if(hn.lineOut) delete self.lines[hn.lineOut];
    delete self.all[hn.hashname];
    debug("reaping ", hn.hashname, why);
  }
  Object.keys(self.all).forEach(function(h){
    hn = self.all[h];
    debug("reap check",hn.hashname,Date.now()-hn.sentAt,Date.now()-hn.recvAt,Object.keys(hn.chans).length);
    if(hn.isSeed) return;
    if(Object.keys(hn.chans).length > 0) return; // let channels clean themselves up
    if(Date.now() - hn.at < hn.timeout()) return; // always leave n00bs around for a while
    if(!hn.sentAt) return del("never sent anything, gc");
    if(!hn.recvAt) return del("sent open, never received");
    if(Date.now() - hn.sentAt > hn.timeout()) return del("we stopped sending to them");
    if(Date.now() - hn.recvAt > hn.timeout()) return del("they stopped responding to us");
  });
}
-}

-- ---------------------------------------------------------------------

{-
// every link that needs to be maintained, ping them
function linkMaint(self)
{
  // process every bucket
  Object.keys(self.buckets).forEach(function(bucket){
    // sort by age and send maintenance to only k links
    var sorted = self.buckets[bucket].sort(function(a,b){ return a.age - b.age });
    if(sorted.length) debug("link maintenance on bucket",bucket,sorted.length);
    sorted.slice(0,defaults.link_k).forEach(function(hn){
      if(!hn.linked || !hn.alive) return;
      if((Date.now() - hn.linked.sentAt) < Math.ceil(defaults.link_timer/2)) return; // we sent to them recently
      hn.linked.send({js:{seed:self.seed}});
    });
  });
}
-}

-- every link that needs to be maintained, ping them
linkMaint :: TeleHash ()
linkMaint = do
  sw <- get
  -- process every bucket
  forM (swBuckets sw) $ \bucket -> do
    -- sort the bucket contents on age
    let sorted = sortBy sf bucket
        sf a b = compare (lineAge a) (lineAge b)
    when (not $ null sorted) $ logT $ "link maintenance on bucket " ++ show (bucket,length sorted)
    forM (take (linkK defaults) sorted) $ \hn -> do
      if (lineLinked hn == Nothing) || (not $ lineAlive hn)
        then return ()
        else do
          timeNow <- io getClockTime
          -- if (timeNow - (lineSentat hn) < ((linkTimer defaults) `div` 2))
          if isTimeOut timeNow (lineSentat hn) ((linkTimer defaults) `div` 2)
            then return () -- we sent to them recently
            else send (pType $ fromJust $ lineLinked hn) (seedMsg (swSeed sw)) Nothing
  return ()

-- ---------------------------------------------------------------------

isTimeOut :: ClockTime -> Maybe ClockTime -> Int -> Bool
isTimeOut (TOD secs _picos) mt millis
 = case mt of
     Nothing -> True
     Just (TOD s _) -> (secs - s) < (fromIntegral millis `div` 1000)

-- ---------------------------------------------------------------------

seedMsg :: Bool -> Packet
seedMsg = undefined



