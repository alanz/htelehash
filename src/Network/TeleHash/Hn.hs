module Network.TeleHash.Hn
  (
    hn_fromjson
  , hn_frompacket
  , hn_getparts
  , hn_path
  , hn_get
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.State
import Data.List
import Data.Maybe
import Prelude hiding (id, (.), head, either)
import System.Time

import Network.TeleHash.Crypt
import Network.TeleHash.Path
import Network.TeleHash.Paths
import Network.TeleHash.Packet
import Network.TeleHash.Types
import Network.TeleHash.Utils

import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map

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
                              putHN $ hc { hCrypto = c}
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
                                      putHN $ hc { hCrypto = c}
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
                let hc1 = hc { hCrypto = mcrypt }
                putHN hc1
                return hc1
              else return hc
      return (Just hc2)

{-
hn_t hn_frompacket(xht_t index, packet_t p)
{
  hn_t hn = NULL;
  if(!p) return NULL;
  
  // get/gen the hashname
  hn = hn_getparts(index, packet_get_packet(p, "from"));
  if(!hn) return NULL;

  // load key from packet body
  if(!hn->c && p->body)
  {
    hn->c = crypt_new(hn->csid, p->body, p->body_len);
    if(!hn->c) return NULL;
  }
  return hn;
}
-}

-- ---------------------------------------------------------------------

hn_getparts :: Parts -> TeleHash (Maybe HashName)
hn_getparts parts = do
  logT $ "hn_getparts: must still match highest cipher set"
  let hashName = parts2hn parts
  mhc <- getHNMaybe hashName
  case mhc of
    Nothing -> do
      putHN $ newHashContainer hashName
      return ()
    Just _ -> return ()
  hc <- getHN hashName
  putHN $ hc { hCsid = "1a"
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
  hc <- getHN hn
  logT $ "hn_path:" ++ show (hn,p,hPaths hc)
  timeNow <- io getClockTime

  let pa = pathFromPathJson p

  let upd pp = pp { pAtIn = Just timeNow}

  -- find existing matching path
  let mp = filter (path_match pa) (Map.elems (hPaths hc))
  ret <- case mp of
    [] -> do
      return pa
    [p1] -> return p1
    _ps -> do
      logT $ "hn_path got multiple path match.:" ++ show(hn,p,hPaths hc)
      assert False undefined
  putHN $ hc { hPaths = Map.insert (pJson ret) (upd ret) (hPaths hc)
             , hLast = Just (pJson ret)
             }
  return (Just ret)

{-
path_t hn_path(hn_t hn, path_t p)
{
  path_t ret = NULL;
  int i;

  if(!p) return NULL;

  // find existing matching path
  for(i=0;hn->paths[i];i++)
  {
    if(path_match(hn->paths[i], p)) ret = hn->paths[i];
  }
  if(!ret && (ret = path_copy(p)))
  {
    // add new path, i is the end of the list from above
    if(!(hn->paths = util_reallocf(hn->paths, (i+2) * (sizeof (path_t))))) return NULL;
    hn->paths[i] = ret;
    hn->paths[i+1] = 0; // null term
  }

  // update state tracking
  if(ret)
  {
    hn->last = ret;
    ret->atIn = platform_seconds();
  }

  return ret;
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
      let hc = newHashContainer hn
      putHN hc
      return hc

{-
hn_t hn_get(xht_t index, unsigned char *bin)
{
  hn_t hn;
  unsigned char hex[65];
  
  if(!bin) return NULL;
  util_hex(bin,32,hex);
  hn = xht_get(index, (const char*)hex);
  if(hn) return hn;

  // init new hashname container
  if(!(hn = malloc(sizeof (struct hn_struct)))) return NULL;
  memset(hn,0,sizeof (struct hn_struct));
  memcpy(hn->hashname, bin, 32);
  memcpy(hn->hexname, hex, 65);
  xht_set(index, (const char*)hn->hexname, (void*)hn);
  if(!(hn->paths = malloc(sizeof (path_t)))) return hn_free(hn);
  hn->paths[0] = NULL;
  return hn;
}
-}

-- ---------------------------------------------------------------------
