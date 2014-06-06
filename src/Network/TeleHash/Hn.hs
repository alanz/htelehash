module Network.TeleHash.Hn
  (
    hn_fromjson
  , hn_frompacket
  , hn_getparts
  , hn_path
  , hn_get
  , hn_address
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.State
import Data.List
import Data.Maybe
import Prelude hiding (id, (.), head, either)
import System.Time

import Network.TeleHash.Crypt
import Network.TeleHash.Packet
import Network.TeleHash.Path
import Network.TeleHash.Paths
import Network.TeleHash.Types
import Network.TeleHash.Utils

import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map
import qualified Data.Set as Set

-- ---------------------------------------------------------------------

hn_fromjson :: RxTelex -> TeleHash (Maybe HashName)
hn_fromjson p = do
  -- get/gen the hashname
  let mpp1 = packet_get_packet p "from"
      mpp2 = if mpp1 == Nothing
               then packet_get_packet p "parts"
               else mpp1
  case mpp2 of
    Nothing -> return Nothing
    Just pp -> do
      let mparts = parseJsVal pp :: Maybe Parts
      case mparts of
        Nothing -> return Nothing
        Just parts -> do
          mhn <- hn_getparts parts
          case mhn of
            Nothing -> return Nothing
            Just hn -> do
              -- if any paths are stored, associate them
              let mpp4 = packet_get_packet p "paths"
              case mpp4 of
                Nothing -> return ()
                Just pp4 -> do
                  let mpaths = parseJsVal pp4 :: Maybe [PathJson]
                  case mpaths of
                    Nothing -> return ()
                    Just paths -> do
                      forM_ paths $ \path -> do
                        mpath2 <- hn_path hn path
                        case mpath2 of
                          Nothing -> return ()
                          Just path2 -> do
                            putPath hn $ path2 { pAtIn = Nothing }
                      -- already have crypto
                      hc <- getHN hn
                      case (hCrypto hc) of
                        Just _ -> return ()
                        Nothing -> do
                          if (BC.length (unBody $ paBody $ rtPacket p) /= 0)
                            then do
                              c <- crypt_new (hCsid hc) Nothing (Just (unBody $ paBody $ rtPacket p))
                              -- putHN $ hc { hCrypto = c}
                              withHN hn $ \hc -> hc { hCrypto = c}
                              return ()
                            else do
                              let mpp = packet_get_packet p "keys"
                              logT $ "hn_fromjson:pp=" ++ show mpp
                              case mpp of
                                Nothing -> return ()
                                Just pp1 -> do
                                  let mkey = packet_get_str (packet_from_val pp1) (hCsid hc)
                                  logT $ "hn_fromjson:mkey=" ++ show mkey
                                  case mkey of
                                    Nothing -> return ()
                                    Just key -> do
                                      c <- crypt_new (hCsid hc) (Just key) Nothing
                                      -- putHN $ hc { hCrypto = c}
                                      withHN hn $ \hc -> hc { hCrypto = c}
                                      return ()
              hcFinal <- getHN hn
              if isNothing (hCrypto hcFinal)
                then return Nothing
                else return (Just hn)

{-
// derive a hn from json seed or connect format
hn_t hn_fromjson(xht_t index, packet_t p)
{
  char *key;
  hn_t hn = NULL;
  packet_t pp, next;
  path_t path;

  if(!p) return NULL;

  // get/gen the hashname
  pp = packet_get_packet(p,"from");
  if(!pp) pp = packet_get_packet(p,"parts");
  hn = hn_getparts(index, pp); // frees pp
  if(!hn) return NULL;

  // if any paths are stored, associte them
  pp = packet_get_packets(p, "paths");
  while(pp)
  {
    path = hn_path(hn, path_parse((char*)pp->json, pp->json_len));
    if(path) path->atIn = 0; // don't consider this path alive
    next = pp->next;
    packet_free(pp);
    pp = next;
  }

  // already have crypto
  if(hn->c) return hn;

  if(p->body_len)
  {
    hn->c = crypt_new(hn->csid, p->body, p->body_len);
  }else{
    pp = packet_get_packet(p, "keys");
    key = packet_get_str(pp,hn->hexid);
    if(key) hn->c = crypt_new(hn->csid, (unsigned char*)key, strlen(key));
    packet_free(pp);
  }

  return (hn->c) ? hn : NULL;
}

-}

-- ---------------------------------------------------------------------

hn_frompacket :: OpenizeInner -> DeOpenizeResult -> TeleHash (Maybe HashContainer)
hn_frompacket _ DeOpenizeVerifyFail = return Nothing
hn_frompacket inner deopen = do
  -- get/gen the hashname
  mhn <- hn_getparts (oiFrom inner)
  case mhn of
    Nothing -> do
      logT $ "hn_frompacket:cannot get hashname " ++ show inner
      return Nothing
    Just hn -> do
      hc <- getHN hn
      -- load key from packet body
      hc2 <- if isNothing (hCrypto hc)
              then do
                mcrypt <- crypt_new (doCsid deopen) Nothing (Just $ doKey deopen)
                withHN hn $ \hc3 -> hc3 { hCrypto = mcrypt }
              else return hc
      return (Just hc2)

-- ---------------------------------------------------------------------

hn_getparts :: Parts -> TeleHash (Maybe HashName)
hn_getparts parts = do
  logT $ "hn_getparts: must still match highest cipher set"
  let hashName = parts2hn parts
  mhc <- getHNMaybe hashName
  case mhc of
    Nothing -> do
      -- putHN $ newHashContainer hashName
      void $ newHN hashName
      return ()
    Just _ -> return ()
  -- hc <- getHN hashName
  -- putHN $ hc { hCsid = "1a"
  --            , hParts = Just parts
  --            }
  hc <- withHN hashName $ \hc
     -> hc { hCsid = "1a"
           , hParts = Just parts
           }
  return (Just (hHashName hc))

{-
hn_t hn_getparts(xht_t index, packet_t p)
{
  char *part, csid, csids[16], hex[3]; // max parts of 8
  int i,ids,ri,len;
  unsigned char *rollup, hnbin[32];
  char best = 0;
  hn_t hn;

  if(!p) return NULL;
  hex[2] = 0;

  for(ids=i=0;ids<8 && p->js[i];i+=4)
  {
    if(p->js[i+1] != 2) continue; // csid must be 2 char only
    memcpy(hex,p->json+p->js[i],2);
    memcpy(csids+(ids*2),hex,2);
    util_unhex((unsigned char*)hex,2,(unsigned char*)&csid);
    if(csid > best && xht_get(index,hex)) best = csid; // matches if we have the same csid in index (for our own keys)
    ids++;
  }

  if(!best) return NULL; // we must match at least one
  util_sort(csids,ids,2,csidcmp,NULL);

  rollup = NULL;
  ri = 0;
  for(i=0;i<ids;i++)
  {
    len = 2;
    if(!(rollup = util_reallocf(rollup,ri+len))) return NULL;
    memcpy(rollup+ri,csids+(i*2),len);
    crypt_hash(rollup,ri+len,hnbin);
    ri = 32;
    if(!(rollup = util_reallocf(rollup,ri))) return NULL;
    memcpy(rollup,hnbin,ri);

    memcpy(hex,csids+(i*2),2);
    part = packet_get_str(p, hex);
    if(!part) continue; // garbage safety
    len = strlen(part);
    if(!(rollup = util_reallocf(rollup,ri+len))) return NULL;
    memcpy(rollup+ri,part,len);
    crypt_hash(rollup,ri+len,hnbin);
    memcpy(rollup,hnbin,32);
  }
  memcpy(hnbin,rollup,32);
  free(rollup);
  hn = hn_get(index, hnbin);
  if(!hn) return NULL;

  if(!hn->parts) hn->parts = p;
  else packet_free(p);

  hn->csid = best;
  util_hex((unsigned char*)&best,1,(unsigned char*)hn->hexid);

  return hn;
}

-}

-- ---------------------------------------------------------------------

hn_path :: HashName -> PathJson -> TeleHash (Maybe Path)
hn_path hn p = do
  -- logT $ "hn_path:" ++ show (hn,p,hPaths hc)
  logT $ "hn_path:" ++ show hn ++ "," ++ showPathJson p

  path <- path_get hn p

  -- update public ip if found
  case pJson path of
    lp@(PIPv4 pipv4) -> do
      case pjsonIp lp of
        Nothing -> return ()
        Just ip -> do
          logT $ "hn_path:checking ip:" ++ show ip
          if isLocalIP ip
            then return ()
            else do
              hnOwn <- getOwnHN
              if hn == hnOwn
                then do
                  logR $ "hn_path:got our remote ip:" ++ show ip
                  sw <- get
                  put $ sw {swExternalIPP = Just pipv4 }
                  -- update our own HN to have the new path
                  hnSelf <- getOwnHN
                  putPath hnSelf (pathFromPathJson lp)
                else return ()
              -- update public ipv4 info
              void $ withHN hn $ \hc1 -> hc1 { hExternalIPP = Just pipv4 }

    _ -> return ()

  logT $ "TODO:hn_path:lots more stuff"
  return (Just path)

{- JS version

This is called on a successful open received, and on every line packet received

  // manage network information consistently, called on all validated incoming packets
  hn.pathIn = function(path)
  {
    path = hn.pathGet(path);
    if(!path) return false;

    // first time we've seen em
    if(!path.recvAt && !path.sentAt)
    {
      debug("PATH INNEW",isLocalPath(path)?"local":"public",JSON.stringify(path.json),hn.paths.map(function(p){return JSON.stringify(p.json)}));

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
        if(!pathValid(other)) return hn.pathEnd(other);
        // remove any previous path on the same IP
        if(path.ip && other.ip == path.ip) return hn.pathEnd(other);
        // remove any previous http path entirely
        if(path.type == "http") return hn.pathEnd(other);
      });

      // any custom non-public paths, we must bridge for
      if(pathShareOrder.indexOf(path.type) == -1) hn.bridging = true;

      // track overall if we trust them as local
      if(isLocalPath(path) && !hn.isLocal)
      {
        hn.isLocal = true;
        hn.pathSync();
      }
    }

    // always update default to newest
    path.recvAt = Date.now();
    hn.to = path;

    return path;
  }



-}


-- ---------------------------------------------------------------------

-- |get a HashContainer from the index, creating it if not already present
hn_get :: HashName -> TeleHash HashContainer
hn_get hn = do
  sw <- get
  case Map.lookup hn (swIndex sw) of
    Just hc -> return hc
    Nothing -> do
      -- let hc = newHashContainer hn
      -- putHN hc
      hc <- newHN hn
      return hc

-- ---------------------------------------------------------------------

hn_address :: HashName -> HashName -> TeleHash [String]
hn_address fromHN toHN = do
  hc <- getHN fromHN
  to <- getHN toHN
  if not (isJust (hParts hc) && isJust (hParts to))
    then return []
    else do
      case partsMatch (fromJust $ hParts hc) (fromJust $ hParts to) of
        Nothing -> return []
        Just csid -> do
          case hExternalIPP hc of
            Nothing -> return [unHN fromHN,csid]
            Just ipp -> return [unHN fromHN,csid,show (v4Ip ipp),show (v4Port ipp)]

{-

  // return our address to them
  hn.address = function(to)
  {
    if(!to) return "";
    var csid = partsMatch(hn.parts,to.parts);
    if(!csid) return "";
    if(!hn.ip) return [hn.hashname,csid].join(",");
    return [hn.hashname,csid,hn.ip,hn.port].join(",");
  }
-}

-- ---------------------------------------------------------------------

partsMatch :: Parts -> Parts -> Maybe String
partsMatch parts1 parts2 = r
  where
    ids = sort $ map fst parts1
    p2 = Set.fromList $ map fst parts2
    common = filter (\k -> Set.member k p2) ids
    r = if common == [] then Nothing
                        else Just $ head common

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
-- ---------------------------------------------------------------------

