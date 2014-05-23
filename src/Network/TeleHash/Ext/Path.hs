module Network.TeleHash.Ext.Path
  (
    ext_path
  , path_send
  , path_free
  , path_sync
  ) where


import Control.Exception
import Control.Monad
import Control.Monad.State

import Data.Maybe

import Network.TeleHash.Dht
import Network.TeleHash.Hn
import Network.TeleHash.Path
import Network.TeleHash.Paths
import Network.TeleHash.SwitchApi
import Network.TeleHash.SwitchUtils
import Network.TeleHash.Types
import Network.TeleHash.Utils

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map


-- ---------------------------------------------------------------------

ext_path :: TChan -> TeleHash ()
ext_path c = do
  logT $ "ext_path entered for:" ++ showChan c
  let
    respFunc p = do
      if packet_has_key p "err"
        then do
          logT $ "ext_path:err in path packet:" ++ showJson (rtJs p)
        else do
          logT $ "ext_path:TODO path packet:" ++ showJson (rtJs p)
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
                path_get (chTo c) path
                logT $ "ext_path:p3=" ++ showJson (tJs p3)
                chan_send (chUid c) p3

  util_chan_popall c (Just respFunc)

-- ---------------------------------------------------------------------

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
      chan_send (chUid c2) p2

-- ---------------------------------------------------------------------

path_handler :: Uid -> TeleHash ()
path_handler cid = do
  logT $ "path_handler entered for " ++ show cid
  c <- getChan cid

  let
    -- TODO: make respFunc a first class func to be called from switch_receive
    respFunc p = do
      logT $ "ext_link:respFunc:processing " ++ showJson (rtJs p)
      -- always respond/ack, except if there is an error or end
      let merr = packet_get_str p "err"
          mend = packet_get_str p "end"
      if any isJust [merr,mend]
        then return ()
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
                  logT $ "path_handler:checking ip:" ++ show ip
                  if isLocalIP ip
                    then return ()
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

  util_chan_popall c (Just respFunc)

-- ---------------------------------------------------------------------

path_free :: PathJson -> TeleHash ()
path_free _path = return ()

{-
void path_free(path_t p)
{
  if(p->id) free(p->id);
  if(p->json) free(p->json);
  free(p);
}
-}

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
      hc <- getHN hn
      let ps = hPathSync hc
      if psSyncing ps
        then return ()
        else do
          logT $ "path_sync:starting new sync"
          path_send hn
          -- assert False undefined
          putHN $ hc { hPathSync = (newPathSync hn) { psSyncing = True} }


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
    hn.raw("path",{js:js, timeout:10*1000}, function(err, packet){
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


