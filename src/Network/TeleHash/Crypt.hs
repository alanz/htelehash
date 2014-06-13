module Network.TeleHash.Crypt
  (
    crypt_init
  , crypt_keygen
  , crypt_new
  , crypt_private
  , crypt_lineize
  , crypt_openize
  , crypt_deopenize
  , crypt_line
  , crypt_delineize
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.State
import Data.List
import Data.Maybe

import Network.TeleHash.Crypto1a
import Network.TeleHash.Packet
import Network.TeleHash.Paths
import Network.TeleHash.Types
import Network.TeleHash.Utils

import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map
import qualified Data.Binary as B


-- ---------------------------------------------------------------------
{-
// these functions are all independent of CS, implemented in crypt.c
-}

-- ---------------------------------------------------------------------
{-
// must be called before any
int crypt_init();
-}
crypt_init :: TeleHash ()
crypt_init = do
  crypt_init_1a
{-
int crypt_init()
{
  int ret = 0;
#ifdef CS_1a
  ret = crypt_init_1a();
  if(ret) return ret;
#endif
#ifdef CS_2a
  ret = crypt_init_2a();
  if(ret) return ret;
#endif
#ifdef CS_3a
  ret = crypt_init_3a();
  if(ret) return ret;
#endif
  return ret;
}
-}

-- ---------------------------------------------------------------------

crypt_keygen :: [CSet] -> TeleHash [(String,(String,String))]
crypt_keygen csets = do
  r <- forM csets $ \cs -> do
    (pub,priv) <- cs_keygen cs
    return (cs_id cs,(pub,priv))
  return r

{-

int crypt_keygen(char csid, packet_t p)
{
  if(!p) return 1;

#ifdef CS_1a
  if(csid == 0x1a) return crypt_keygen_1a(p);
#endif
#ifdef CS_2a
  if(csid == 0x2a) return crypt_keygen_2a(p);
#endif
#ifdef CS_3a
  if(csid == 0x3a) return crypt_keygen_3a(p);
#endif

  return 1;
}

-}

-- ---------------------------------------------------------------------

{-
// takes binary or string key format, creates a crypt object
crypt_t crypt_new(char csid, unsigned char *key, int len);
-}
crypt_new :: String -> Maybe String -> Maybe BC.ByteString -> TeleHash (Maybe Crypto)
crypt_new csid mKeyStr mKeyBin = do
  logT $ "crypt_new " ++ show (csid,mKeyStr,mKeyBin)
  sw <- get
  case Map.lookup csid (swIndexCrypto sw) of
    Just cr -> do
      let cn = cs_new (cset (cCs cr))
      c <- cn mKeyStr mKeyBin
      return c
    Nothing -> do
      logT $ "crypt_new: trying to init unsupported cs:" ++ csid
      assert False undefined


-- ---------------------------------------------------------------------

-- |load a private id key, returns !0 if error, can pass (c,NULL,0) to check if private is already loaded too
crypt_private :: Crypto -> String -> TeleHash Crypto
crypt_private c key = do
  if cIsPrivate c
    then return c -- already loaded
    else do
      let cp = cs_private $ cset (cCs c)
      cp c key


-- ---------------------------------------------------------------------

crypt_lineize :: Maybe Crypto -> TxTelex -> TeleHash (Maybe Crypto,Maybe LinePacket)
crypt_lineize mc p = do
  logT $ "crypt_lineize:" ++ showJson (tJs p)
  case mc of
    Nothing -> return (Nothing,Nothing)
    Just c -> do
      if cLined c /= LineNone
        then do
          logP $ "<<<<:" ++ show (tTo p,showPathJson $ tOut p,showPacketShort $ tPacket p)
          let cl = cs_lineize (cset (cCs c))
          (c2,mlp) <- cl c p
          return (Just c2,mlp)
        else return (Just c,Nothing)

-- ---------------------------------------------------------------------

crypt_openize :: Crypto -> Crypto -> OpenizeInner -> TeleHash (Maybe LinePacket)
crypt_openize self c inner = do
  if (cCsid self) /= (cCsid c)
    then do
      logT $ "crypt_openize:csid mismatch:" ++ show (cCsid self,cCsid c)
      return Nothing
    else do
      let co = cs_openize (cset (cCs self)) -- TODO: check self is correct
      co self c inner

-- ---------------------------------------------------------------------

crypt_deopenize :: NetworkPacket -> TeleHash DeOpenizeResult
crypt_deopenize open@(OpenPacket csHex _) = do
  sw <- get
  case Map.lookup (hexCsToStrCs csHex) (swIndexCrypto sw) of
    Just self -> do
      let cd = cs_deopenize (cset (cCs self))
      cd self open
    Nothing -> do
      logT $ "crypt_deopenize:mssing crypto for " ++ show csHex
      return DeOpenizeVerifyFail
crypt_deopenize pkt = do
  logT $ "crypt_deopenize:not an open packet:" ++ show pkt
  return DeOpenizeVerifyFail

hexCsToStrCs :: B.Word8 -> String
hexCsToStrCs 0x0a = "0a"
hexCsToStrCs 0x1a = "1a"
hexCsToStrCs 0x2a = "2a"
hexCsToStrCs 0x3a = "3a"
hexCsToStrCs 0x4a = "4a"
hexCsToStrCs xx = error $ "hexCsToStrCs:unknown csid:" ++ show xx

-- ---------------------------------------------------------------------

crypt_line :: DeOpenizeResult -> Crypto -> OpenizeInner -> TeleHash (Maybe Crypto)
crypt_line open c inner = do
  if (isJust (cAtIn c) && oiAt inner  <= fromJust (cAtIn c))
    || length (oiLine inner) /= 32
    then return Nothing
    else do
      let lineid = b16Tobs (BC.pack $ oiLine inner)
      -- logT $ "crypt_line:lineid=" ++ show (B16.encode lineid)
      let lined = if lineid == cLineIn c
                    then Lined -- same line
                    else LineReset -- new one
      let c2 = c { cLined = lined
                 , cLineIn = lineid
                 }
      let cl = cs_line (cset (cCs c))
      mc3 <- cl open c2
      case mc3 of
        Nothing -> return Nothing
        Just c3 -> do
          return $ Just $ c3 { cAtIn = Just (oiAt inner) }

-- ---------------------------------------------------------------------

crypt_delineize :: Crypto -> NetworkTelex -> TeleHash (Either String RxTelex)
crypt_delineize c p = do
  -- logT $ "crypt_delineize: cLined=" ++ show (cLined c)
  if cLined c == LineNone
    then return (Left "line not open")
    else do
      let cd = cs_delineize (cset (cCs c))
      cd c p

