module TeleHash.New.Crypt
  (
    crypt_init
  , crypt_new
  , crypt_private
  , crypt_lineize
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

import TeleHash.New.Crypto1a
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

{-
// takes binary or string key format, creates a crypt object
crypt_t crypt_new(char csid, unsigned char *key, int len);
-}
crypt_new :: String -> Maybe String -> Maybe BC.ByteString -> TeleHash (Maybe Crypto)
crypt_new csid mKeyStr mKeyBin = do
  logT $ "crypt_new " ++ show (csid,mKeyStr,mKeyBin)
{-
  let c = Crypto
           { cCsid      = csid
           , cIsPrivate = False
           , cLined     = False
           , cKeyLen    :: !Int
           , cAtOut     :: !ClockTime
           , cAtIn      :: !ClockTime
           , cLineOut   = randomHexVal
           , cLineIn    :: !String
           , cKey       :: !String
           , cCs        :: !String -- TBD, individual crypto structures
-}
  c <- crypt_new_1a mKeyStr mKeyBin
  return c

{-
crypt_t crypt_new(char csid, unsigned char *key, int len)
{
  crypt_t c;
  int err = 1;

  if(!csid || !key || !len) return NULL;
  if(!(c = malloc(sizeof (struct crypt_struct)))) return NULL;
  memset(c, 0, sizeof (struct crypt_struct));
  c->csid = csid;
  sprintf(c->csidHex,"%02x",csid);
  crypt_rand(c->lineOut,16);
  util_hex(c->lineOut,16,c->lineHex);
  c->atOut = platform_seconds();

#ifdef CS_1a
  if(csid == 0x1a) err = crypt_new_1a(c, key, len);
#endif
#ifdef CS_2a
  if(csid == 0x2a) err = crypt_new_2a(c, key, len);
#endif
#ifdef CS_3a
  if(csid == 0x3a) err = crypt_new_3a(c, key, len);
#endif
  
  if(!err) return c;

  crypt_free(c);
  return NULL;
}
-}

-- ---------------------------------------------------------------------

{-

void crypt_free(crypt_t c);


// these exist in the crypt_*_base.c as general crypto routines

// write random bytes, returns s for convenience
unsigned char *crypt_rand(unsigned char *s, int len);

// sha256's the input, output must be [32] from caller
unsigned char *crypt_hash(unsigned char *input, unsigned long len, unsigned char *output);

// last known error for debugging
char *crypt_err();


// the rest of these just use the CS chosen for the crypt_t, crypt.c calls out to crypt_*_XX.c

// adds "XX":"pubkey", "XX_":"secretkey" to the packet, !0 if error
int crypt_keygen(char csid, packet_t p);
-}

-- ---------------------------------------------------------------------

-- |load a private id key, returns !0 if error, can pass (c,NULL,0) to check if private is already loaded too
crypt_private :: Crypto -> String -> TeleHash Crypto
crypt_private c key = do
  if cIsPrivate c
    then return c -- already loaded
    else do
      crypt_private_1a c key

{-
int crypt_private(crypt_t c, unsigned char *key, int len)
{
  int ret;
  if(!c) return 1;
  if(c->isprivate) return 0; // already loaded

#ifdef CS_1a
  if(c->csid == 0x1a && (ret = crypt_private_1a(c,key,len))) return ret;
#endif
#ifdef CS_2a
  if(c->csid == 0x2a && (ret = crypt_private_2a(c,key,len))) return ret;
#endif
#ifdef CS_3a
  if(c->csid == 0x3a && (ret = crypt_private_3a(c,key,len))) return ret;
#endif
  
  c->isprivate = 1;
  return 0;
}
-}

-- ---------------------------------------------------------------------

{-
// try to create a line packet chained to this one
packet_t crypt_lineize(crypt_t c, packet_t p);

// decrypts or NULL, frees p
packet_t crypt_delineize(crypt_t c, packet_t p);

// create a new open packet, NULL if error
packet_t crypt_openize(crypt_t self, crypt_t c, packet_t inner);

// processes an open packet into a inner packet or NULL
packet_t crypt_deopenize(crypt_t self, packet_t p);

// tries to create a new line, !0 if error/ignored, always frees inner
int crypt_line(crypt_t c, packet_t inner);

#ifdef CS_1a
int crypt_init_1a();
int crypt_new_1a(crypt_t c, unsigned char *key, int len);
void crypt_free_1a(crypt_t c);
int crypt_keygen_1a(packet_t p);
int crypt_public_1a(crypt_t c, unsigned char *key, int len);
int crypt_private_1a(crypt_t c, unsigned char *key, int len);
packet_t crypt_lineize_1a(crypt_t c, packet_t p);
packet_t crypt_delineize_1a(crypt_t c, packet_t p);
packet_t crypt_openize_1a(crypt_t self, crypt_t c, packet_t inner);
packet_t crypt_deopenize_1a(crypt_t self, packet_t p);
int crypt_line_1a(crypt_t c, packet_t inner);
#endif

#ifdef CS_2a
int crypt_init_2a();
int crypt_new_2a(crypt_t c, unsigned char *key, int len);
void crypt_free_2a(crypt_t c);
int crypt_keygen_2a(packet_t p);
int crypt_public_2a(crypt_t c, unsigned char *key, int len);
int crypt_private_2a(crypt_t c, unsigned char *key, int len);
packet_t crypt_lineize_2a(crypt_t c, packet_t p);
packet_t crypt_delineize_2a(crypt_t c, packet_t p);
packet_t crypt_openize_2a(crypt_t self, crypt_t c, packet_t inner);
packet_t crypt_deopenize_2a(crypt_t self, packet_t p);
int crypt_line_2a(crypt_t c, packet_t inner);
#endif

#ifdef CS_3a
int crypt_init_3a();
int crypt_new_3a(crypt_t c, unsigned char *key, int len);
void crypt_free_3a(crypt_t c);
int crypt_keygen_3a(packet_t p);
int crypt_public_3a(crypt_t c, unsigned char *key, int len);
int crypt_private_3a(crypt_t c, unsigned char *key, int len);
packet_t crypt_lineize_3a(crypt_t c, packet_t p);
packet_t crypt_delineize_3a(crypt_t c, packet_t p);
packet_t crypt_openize_3a(crypt_t self, crypt_t c, packet_t inner);
packet_t crypt_deopenize_3a(crypt_t self, packet_t p);
int crypt_line_3a(crypt_t c, packet_t inner);
#endif


-}

-- ---------------------------------------------------------------------

crypt_lineize :: Crypto -> TxTelex -> TeleHash (Crypto,Maybe LinePacket)
crypt_lineize c p = do
  if cLined c
    then crypt_lineize_1a c p
    else return (c,Nothing)

{-
packet_t crypt_lineize(crypt_t c, packet_t p)
{
  if(!c || !p || !c->lined) return NULL;
#ifdef CS_1a
  if(c->csid == 0x1a) return crypt_lineize_1a(c,p);
#endif
#ifdef CS_2a
  if(c->csid == 0x2a) return crypt_lineize_2a(c,p);
#endif
#ifdef CS_3a
  if(c->csid == 0x3a) return crypt_lineize_3a(c,p);
#endif
  return NULL;
}
-}
