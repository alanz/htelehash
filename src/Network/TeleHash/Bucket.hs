module Network.TeleHash.Bucket
  (
    bucket_load
  , bucket_get
  ) where

-- import Control.Applicative
-- import Control.Concurrent
import Control.Exception
-- import Control.Monad
-- import Control.Monad.Error
-- import Control.Monad.State
-- import Crypto.Random
-- import Data.Aeson (object,(.=), (.:), (.:?) )
-- import Data.Aeson.Encode
import Data.Aeson.Types
-- import Data.Bits
-- import Data.Char
-- import Data.IP
-- import Data.List
import Data.Maybe
-- import Data.String.Utils
-- import Data.Text.Lazy.Builder
-- import Data.Typeable
-- import Data.Word
-- import Network.BSD
-- import Network.Socket
-- import Prelude hiding (id, (.), head, either, catch)
-- import System.IO
-- import System.Log.Handler.Simple
-- import System.Log.Logger
-- import System.Time
-- import Network.TeleHash.Crypt
import Network.TeleHash.Hn
-- import Network.TeleHash.Paths
import Network.TeleHash.Types
import Network.TeleHash.Utils

-- import qualified Crypto.Hash.SHA256 as SHA256
-- import qualified Crypto.PubKey.DH as DH
-- import qualified Crypto.Types.PubKey.ECDSA as ECDSA
import qualified Data.Aeson as Aeson
-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Base16 as B16
-- import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
-- import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.HashMap.Strict as HM
-- import qualified Data.Map as Map
import qualified Data.Set as Set
-- import qualified Data.Text as Text
-- import qualified Data.Text.Lazy as TL
-- import qualified Network.Socket as NS
-- import qualified Network.Socket.ByteString as SB

-- ---------------------------------------------------------------------

{-

typedef struct bucket_struct
{
  int count;
  hn_t *hns;
} *bucket_t;

bucket_t bucket_new();
void bucket_free(bucket_t b);

void bucket_add(bucket_t b, hn_t h);
hn_t bucket_in(bucket_t b, hn_t h);
-}

-- ---------------------------------------------------------------------
{-
// simple array index function
hn_t bucket_get(bucket_t b, int index);
-}

-- |simple array index function
bucket_get :: Bucket -> Int -> Maybe HashName
bucket_get b index =
  if index > Set.size b || Set.size b == 0
    then Nothing
    else Just $ ghead "bucket_get" $ drop index $ Set.toList b
{-
hn_t bucket_get(bucket_t b, int index)
{
  if(index >= b->count) return NULL;
  return b->hns[index];
}
-}

-- ---------------------------------------------------------------------

bucket_load :: FilePath -> TeleHash Bucket
bucket_load fname = do
  fc <- io $ BL.readFile fname
  -- let ms = Aeson.decode fc :: Maybe [SeedInfo]
  -- logT $ "seedinfo=" ++ show ms

  let mv = Aeson.decode fc :: Maybe Value
  logT $ "seedinfo=" ++ show mv
  case mv of
    Nothing -> return Set.empty
    Just (Object vv) -> do
      let go (Object o) = hn_fromjson (packet_new_rx { rtJs = o})
          go _          = return Nothing
      hns <- mapM go $ HM.elems vv
      logT $ "bucket_load: hns=" ++ show hns
      return $ Set.fromList $ catMaybes hns
    Just _ -> assert False undefined

{-
bucket_t bucket_load(xht_t index, char *file)
{
  packet_t p, p2;
  hn_t hn;
  bucket_t b = NULL;
  int i;

  p = util_file2packet(file);
  if(!p) return b;
  if(*p->json != '{')
  {
    packet_free(p);
    return b;
  }

  // check each value, since js0n is key,len,value,len,key,len,value,len for objects
  for(i=0;p->js[i];i+=4)
    {
    p2 = packet_new();
    packet_json(p2, p->json+p->js[i+2], p->js[i+3]);
    hn = hn_fromjson(index, p2);
    packet_free(p2);
    if(!hn) continue;
    if(!b) b = bucket_new();
    bucket_add(b, hn);
    }

  packet_free(p);
  return b;
}
-}
