module Network.TeleHash.Ext.Path
  (
    ext_path
  , path_send
  , path_sync
  ) where


import Control.Exception
import Control.Monad
import Control.Monad.State

import Data.List
import Data.Maybe

import Network.TeleHash.Dht
import Network.TeleHash.Hn
import Network.TeleHash.Path
import Network.TeleHash.Paths
import Network.TeleHash.SwitchApi
import Network.TeleHash.SwitchUtils
import Network.TeleHash.Types
import Network.TeleHash.Utils

import System.Time

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map

{-

Note: there are two path scenarios, in the one we are the initiator,
in the other the responder.

Where we have initiated it the message is processed via `path_handler`.

-}


-- ---------------------------------------------------------------------

-- |Process incoming requests from other nodes to validate their paths.
ext_path :: Uid -> TeleHash ()
ext_path cid = do
  c <- getChan cid
  logT $ "ext_path entered for:" ++ showChan c
  let
    respFunc p = do
        if packet_has_key p "err"
          then do
            logT $ "ext_path:err in path packet:" ++ showJson (rtJs p)
          else do
              -- path check from the remote side
              let pathsJsValue = packet_get p "paths"
                  mps = parseJsVal (gfromJust "ext_path" pathsJsValue) :: Maybe [PathJson]
              logT $ "ext_path:mps=" ++ show mps
              case mps of
                Nothing -> do
                  logT $ "exp_path:could not parse paths:" ++ showJson (rtJs p)
                Just ps -> do
                  let ps1 = filter (\pa -> pjsonType pa == PtIPv4) ps
                  -- TODO: more detailed filtering of these paths
                  forM_ ps1 $ \ path -> do
                    mp1 <- chan_packet (chUid c) True
                    let p3 = packet_set (gfromJust "ext_path" mp1) "path" path
                        p4 = p3 { tOut = path }
                    void $ path_get (chTo c) path
                    logT $ "ext_path:p4=" ++ showJson (tJs p4)
                    logT $ "ext_path:tOut p4=" ++ show (tOut p4)
                    chan_send (chUid c) p4

  util_chan_popall c (Just respFunc)

-- ---------------------------------------------------------------------

-- |Send a path request message to the given HashName
path_send :: HashName -> TeleHash ()
path_send to = do
  logT $ "path_send " ++ show to
  c <- chan_new to "path" Nothing
  let c2 = c { chHandler = Just path_handler }
  putChan c2
  mp <- chan_packet (chUid c2) True
  case mp of
    Nothing -> do
      logT $ "path_send:failed to make channel packet"
      return ()
    Just p -> do
      ownHn <- getOwnHN
      ownHc <- getHN ownHn
      let p2 = if Map.size (hPaths ownHc) > 0
                 then packet_set p "paths" (Map.keys (hPaths ownHc))
                 else p
      logT $ "path_send:sending: " ++ show (to,showJson (tJs p2))
      -- sw <- get
      if False -- TODO:why False?
        then do
          -- if no default route, and our external IP is not known
          -- yet, send to all known external ones
          hc <- getHN to
          forM_ (Map.keys (hPaths hc)) $ \path -> do
            cp <- chan_new to "path" Nothing
            let cp2 = cp { chHandler = Just path_handler }
            putChan cp2
            mp3 <- chan_packet (chUid cp2) True
            case mp3 of
              Nothing -> return ()
              Just p3 -> do
                let p4 = if Map.size (hPaths ownHc) > 0
                           then packet_set p3 "paths" (Map.keys (hPaths ownHc))
                           else p3
                logT $ "path_send:sending to path " ++ showPathJson path
                chan_send (chUid cp) (p4 { tOut = path })
        else chan_send (chUid c2) p2

-- ---------------------------------------------------------------------

-- |Process responses from the remote node to our path validation request
path_handler :: Uid -> TeleHash ()
path_handler cid = do
  c <- getChan cid
  logT $ "path_handler entered for " ++ showChan c

  let
    -- TODO: make respFunc a first class func to be called from switch_receive
    respFunc p = do
      logT $ "path_handler:respFunc:processing " ++ showJson (rtJs p)
      hc <- getHN (chTo c)
      let ps = hPathSync hc
      -- always respond/ack, except if there is an error or end
      let merr = packet_get_str p "err"
          mend = packet_get_str p "end"
      if any isJust [merr,mend]
        then do
          logT $ "path_handler:err or end in path packet:" ++ showJson (rtJs p)
          void $ withHN (chTo c) $ \hc1 -> hc1 { hPathSync = ps { psSyncing = PathSyncDone }}
          return ()
        else do
          let mv = packet_get p "path"
              mlp = case mv of
                Just v  -> parseJsVal v :: Maybe PathJson
                Nothing -> Nothing
          case mlp of
            Nothing -> return ()
            Just lp@(PIPv4 pipv4) -> do
              case pjsonIp lp of
                Nothing -> return ()
                Just ip -> do
                  self <- getOwnHC
                  logT $ "path_handler:checking ip:" ++ show ip
                  -- logT $ "path_handler:current known own ips" ++ show (Map.keys (hPaths self))
                  if isLocalIP ip || not (hIsSeed hc)
                                  || not (Map.member lp (hPaths self))
                    then do
                      logT $ "path_handler:(isLocalIP ip,not (hIsSeed hc),known paths)=" ++ show (isLocalIP ip,not (hIsSeed hc),Map.keys (hPaths self))
                      return ()
                    else do
                      logR $ "path_handler:got our remote ip:" ++ show ip
                      sw <- get
                      put $ sw {swExternalIPP = Just pipv4 }
                      -- update our own HN to have the new path
                      hnSelf <- getOwnHN
                      putPath hnSelf (pathFromPathJson lp)
                      return ()
            Just lp -> do
              logT $ "path_handler:unexpected path type :" ++ show lp

          --  add to all answers and update best default from active ones

          let ps2 = ps { psAlive = (psAlive ps) ++ [rtSender p]}
              check_best best path
                 = if isLocalPath best
                     then best
                     else if getPathShareOrder path > getPathShareOrder best
                       then path
                       else best
              bestPath = foldl' check_best (rtSender p) (psAlive ps2)
          logT $ "path_handler:" ++ showChan c ++ ",best=" ++ showPathJson bestPath
          hc2 <- getHN (hHashName hc)
          now <- io $ getClockTime
          -- putHN $ hc2 { hPathSync = ps2 { psLastAt = Just now }
          --             , hLast = Just bestPath
          --             }
          withHN (hHashName hc) $ \hc ->
                   hc { hPathSync = ps2 { psLastAt = Just now }
                      , hLast = Just bestPath
                      }
          return ()
  util_chan_popall c (Just respFunc)


getPathShareOrder :: PathJson -> Integer
getPathShareOrder path
  = case Map.lookup (pjsonType path) pathShareOrder of
      Nothing -> 0
      Just order -> order

pathShareOrder :: Map.Map PathType Integer
pathShareOrder = Map.fromList [(PtBlueTooth,1),(PtWebRtc,2),(PtIPv6,3),(PtIPv4,4),(PtHttp,5)];
-- pathShareOrder = Map.fromList [("bluetooth",1),("webrtc",2),("ipv6",3),("ipv4",4),("http",5)];

-- ---------------------------------------------------------------------


path_sync :: HashName -> TeleHash ()
path_sync hn = do
  logT $ "path_sync:" ++ show hn
  ownHn <- getOwnHN
  if hn == ownHn
    then do
      logT $ "path_sync:cannot sync to ourselves"
      return ()
    else do
      -- TODO: only sync if currently online
      hc <- getHN hn
      let ps = hPathSync hc
      case psSyncing ps of
        PathSyncDone -> do
          return ()
        PathSyncBusy -> do
          now <- io $ getClockTime
          if isTimeOut now (psLastAt ps) param_path_sync_timeout_secs
            then do
              void $ withHN hn $ \hc1 -> hc1 { hPathSync = ps { psSyncing = PathSyncDone }}
            else return ()
        PathSyncOff -> do
          logT $ "path_sync:starting new sync"
          path_send hn
          now <- io $ getClockTime
          void $ withHN hn $ \hc2 ->
              hc2 { hPathSync = (newPathSync hn) { psSyncing = PathSyncBusy
                                                 , psLastAt = Just now
                                                 }
                 }


{-
  // send a path sync
  hn.pathSync = function()
  {
    if(hn.pathSyncing) return;
    hn.pathSyncing = true;
    debug("pathSync",hn.hashname);
    var js = {};
    var paths = hn.pathsOut();
    if(paths.length > 0) js.paths = paths;
    var alive = [];
    hn.raw("path",{js:js, timeout:10*1000}, function(err, packet) {
      if(err)
      {
        hn.pathSyncing = false;
        return;
      }

      // if path answer is from a seed, update our public ip/port in case we're behind a NAT
      if(packet.from.isSeed && typeof packet.js.path == "object" && packet.js.path.type == "ipv4" && !isLocalIP(packet.js.path.ip))
      {
        debug("updating public ipv4",JSON.stringify(self.pub4),JSON.stringify(packet.js.path));
        self.pathSet(self.pub4,true);
        self.pub4 = {type:"ipv4", ip:packet.js.path.ip, port:parseInt(packet.js.path.port)};
        self.pathSet(self.pub4);
      }

      if(!packet.sender) return; // no sender path is bad

      // add to all answers and update best default from active ones
      alive.push(packet.sender);
      var best = packet.sender;
      alive.forEach(function(path){
        if(pathShareOrder.indexOf(path.type) > pathShareOrder.indexOf(path.type)) return;
        if(isLocalPath(best)) return; // always prefer (the first) local paths
        best = path;
      });
      debug("pathSync best",hn.hashname,JSON.stringify(best.json));
      hn.to = best;
    });
  }


// network preference order for paths
var pathShareOrder = ["bluetooth","webrtc","ipv6","ipv4","http"];

-}

-- EOF


