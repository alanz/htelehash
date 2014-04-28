{-# LANGUAGE OverloadedStrings #-}
module TeleHash.New.Crypto1a
  (
    -- cset_1a
    crypt_init_1a
  , crypt_new_1a
  , crypt_private_1a

  , mkHashFromBS
  , mkHashFromB64
  ) where

-- | Implement Cypher Suite 1a for TeleHash, as per
-- https://github.com/telehash/telehash.org/blob/master/cs/1a.md

{-

    SHA1 - well trusted and performs well on small devices
    ECC secp160r1 - small key sizes, balance of strong crypto (~1024bit) and still supportable with low cpu
    HMAC-SHA1 - common implementations available for embedded environments
    AES-128-CTR - low impact cipher, many implementations including hardware ones

-}


import Control.Exception
import Control.Monad.State
import Crypto.Cipher.AES
import Crypto.MAC.HMAC
import Crypto.Number.Serialize
import Crypto.PubKey.ECC.ECDSA
import Crypto.PubKey.ECC.Generate
import Crypto.Random
import Data.ByteString.Base64
import Data.List
import Data.Maybe
import Data.Word
import System.Time
import TeleHash.New.Convert
import TeleHash.New.Packet
import TeleHash.New.Types
import TeleHash.New.Utils

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.PubKey.DH as DH
import qualified Crypto.PubKey.ECC.Prim as ECC
import qualified Crypto.Types.PubKey.DH as DH
import qualified Crypto.Types.PubKey.ECC as ECC
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text

curve = ECC.getCurveByName ECC.SEC_p160r1

-- ---------------------------------------------------------------------

crypt_init_1a :: TeleHash ()
crypt_init_1a = do
  return ()

{-
int crypt_init_1a()
{
  struct timeval tv;
  unsigned int seed;
  gettimeofday(&tv, NULL);
  seed = (getpid() << 16) ^ tv.tv_sec ^ tv.tv_usec;
  srandom(seed); // srandomdev() is not universal
  uECC_set_rng(&RNG);
  return 0;
}
-}

-- ---------------------------------------------------------------------

mkHashFromBS :: BC.ByteString -> Hash
mkHashFromBS bs =
  let
    -- digest = SHA.sha1 $ BL.fromChunks [bs]
    digest = SHA1.hash bs
  in
   Hash (show digest)

mkHashFromB64 :: String -> Hash
mkHashFromB64 str = r
  where
    Right bs = decode $ BC.pack str
    r = Hash (BC.unpack $ B16.encode $ SHA1.hash $ bs)

-- ---------------------------------------------------------------------

crypt_new_1a :: Maybe String -> Maybe BC.ByteString -> TeleHash (Maybe Crypto)
crypt_new_1a mPubStr mPubBin = do
  mbs <- case mPubStr of
          Just pub -> do
            let mbs = decode $ BC.pack pub
            case mbs of
              Left _err -> do
                logT $ "invalid public key b64decode failed:" ++ pub
                return Nothing
              Right bs -> return (Just bs)

          Nothing -> return mPubBin
  case mbs of
    Nothing -> return Nothing
    Just bs -> do
      if BC.length bs /= 40
        then do logT $ "invalid public key wrong len:" ++ (show bs)
                return Nothing
        else do
          -- create line ephemeral key
          -- uECC_make_key(cs->line_public, cs->line_private);
          sw <- get
          let ((pubk,privk) ,g') = generate (swRNG sw) curve
          put $ sw { swRNG = g' } -- must come before next line, else update lost

          timeNow <- io getClockTime
          randomHexVal <- randomHEX 16
          let
            -- create the public key
              pubkey = PublicKey curve (bsToPoint bs)


              hexName = mkHashFromBS bs
              newParts = [("1a",unHash hexName)]
              cs = Crypt1a
                      { cs1aIdPrivate   = Nothing
                      , cs1aIdPublic    = Public1a pubkey
                      , cs1aLinePrivate = Private1a privk
                      , cs1aLinePublic  = Public1a pubk
                      , cs1aSeq         = 0
                      , cs1aKeyOut      = Nothing
                      , cs1aKeyIn       = Nothing
                      }
              c = Crypto
                      { cCsid      = "1a"
                      , cPart      = hexName
                      , cIsPrivate = False
                      , cLined     = False
                      , cKeyLen    = 40
                      , cAtOut     = timeNow
                      , cAtIn      = Nothing
                      , cLineOut   = randomHexVal
                      , cLineIn    = ""
                      , cKey       = bs
                      , cCs        = cs
                      }

          logT $ "crypt_loadkey_1a:parts=" ++ show (cPart c)
          return (Just c)



{-
int crypt_new_1a(crypt_t c, unsigned char *key, int len)
{
  unsigned char hash[32];
  crypt_1a_t cs;
  
  if(!key || len <= 0) return 1;
  
  c->cs = malloc(sizeof(struct crypt_1a_struct));
  memset(c->cs, 0, sizeof (struct crypt_1a_struct));
  cs = (crypt_1a_t)c->cs;

  if(len == uECC_BYTES*2)
  {
    memcpy(cs->id_public,key,uECC_BYTES*2);
  }else{
    // try to base64 decode in case that's the incoming format
    if(key[len] != 0 || base64_binlength((char*)key,0) != uECC_BYTES*2 || base64dec(cs->id_public,(char*)key,0)) return -1;
  }
  
  // generate fingerprint
  crypt_hash(cs->id_public,uECC_BYTES*2,hash);

  // create line ephemeral key
  uECC_make_key(cs->line_public, cs->line_private);

  // alloc/copy in the public values (free'd by crypt_free)  
  c->part = malloc(32*2+1);
  c->keylen = uECC_BYTES*2;
  c->key = malloc(c->keylen);
  memcpy(c->key,cs->id_public,uECC_BYTES*2);
  util_hex(hash,32,(unsigned char*)c->part);

  return 0;
}
-}

-- ---------------------------------------------------------------------

crypt_private_1a :: Crypto -> String -> TeleHash Crypto
crypt_private_1a c key = do
  let
    privatekey = pk
      where
        mbsp = decode $ BC.pack key
        pk = case mbsp of
          Left _err -> Nothing
          Right bsp -> Just $ Private1a privkey
            where
              i = os2ip bsp
              privkey = PrivateKey curve i
  return $ c { cIsPrivate = True
             , cCs = (cCs c) { cs1aIdPrivate = privatekey }
             }

{-
int crypt_private_1a(crypt_t c, unsigned char *key, int len)
{
  crypt_1a_t cs = (crypt_1a_t)c->cs;
  
  if(!key || len <= 0) return 1;

  if(len == uECC_BYTES)
  {
    memcpy(cs->id_private,key,uECC_BYTES);
  }else{
    // try to base64 decode in case that's the incoming format
    if(key[len] != 0 || base64_binlength((char*)key,0) != uECC_BYTES || base64dec(cs->id_private,(char*)key,0)) return -1;
  }

  c->isprivate = 1;
  return 0;
}
-}

-- ---------------------------------------------------------------------

pointTow8s :: ECC.Point -> B.ByteString
pointTow8s ECC.PointO = B.empty
pointTow8s (ECC.Point i1 i2)= B.append i1_20 i2_20
  where
    (Just i1_20) = i2ospOf 20 i1
    (Just i2_20) = i2ospOf 20 i2

-- ---------------------------------------------------------------------

privToBs p = pbs
  where
    (Just pbs) = i2ospOf 20 p

-- ---------------------------------------------------------------------

bsToPoint :: BC.ByteString -> ECC.Point
bsToPoint bs = (ECC.Point i1 i2)
  where
    (b1,b2) = B.splitAt 20 bs
    i1 = os2ip b1
    i2 = os2ip b2


