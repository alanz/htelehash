module Network.TeleHash.Path
  (
    path_alive
  , path_match
  ) where

-- import Control.Applicative
-- import Control.Concurrent
-- import Control.Exception
import Control.Monad
-- import Control.Monad.Error
-- import Control.Monad.State
-- import Crypto.Random
-- import Data.Aeson (object,(.=), (.:), (.:?) )
-- import Data.Aeson.Encode
-- import Data.Aeson.Types
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
import Prelude hiding (id, (.), head, either)
-- import System.IO
-- import System.Log.Handler.Simple
-- import System.Log.Logger
import System.Time

-- import Network.TeleHash.Crypt
import Network.TeleHash.Types
import Network.TeleHash.Utils

-- import qualified Crypto.Hash.SHA256 as SHA256
-- import qualified Crypto.PubKey.DH as DH
-- import qualified Crypto.Types.PubKey.ECDSA as ECDSA
-- import qualified Data.Aeson as Aeson
-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Base16 as B16
-- import qualified Data.ByteString.Char8 as BC
-- import qualified Data.ByteString.Lazy as BL
-- import qualified Data.Digest.Pure.SHA as SHA
-- import qualified Data.HashMap.Strict as HM
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import qualified Data.Text as Text
-- import qualified Data.Text.Lazy as TL
-- import qualified Network.Socket as NS
-- import qualified Network.Socket.ByteString as SB

-- ---------------------------------------------------------------------

path_alive :: Path -> TeleHash Bool
path_alive p = do
  timeNow <- io $ getClockTime
  -- logT $ "pathAlive: comparing " ++ show (timeNow,pAtIn p)
  case pAtIn p of
    Nothing -> return False
    Just t -> do
      let dt = diffClockTimes timeNow t
          diff60Sec = TimeDiff 0 0 0 0 0 60 0
      return (dt < diff60Sec)
{-
int path_alive(path_t p)
{
  unsigned long now;
  if(!p) return 0;
  now = platform_seconds();
  if((now - p->atIn) < 60) return 1;
  return 0;
}

-}

-- ---------------------------------------------------------------------

path_match :: Path -> Path -> Bool
path_match p1 p2 = (pJson p1) == (pJson p2)

{-
int path_match(path_t p1, path_t p2)
{
  if(!p1 || !p2) return 0;
  if(p1 == p2) return 1;
  if(util_cmp(p1->type,p2->type) != 0) return 0;
  if(strstr(p1->type,"ipv"))
  {
    if(util_cmp(p1->ip, p2->ip) == 0 && p1->port == p2->port) return 1;
  }else{
    if(util_cmp(p1->id, p2->id) == 0) return 1;
  }
  return 0;
}

-}
{-
pathMatch :: Path -> [Path] -> Maybe Path
pathMatch path1 paths = r
  where
    mtypes = filter (\p -> pathType path1 == pathType p) paths
    m :: Path -> Path -> Maybe Path
    m p1 p2
      | pathType p1 == PtRelay
         && pRelay p1 == pRelay p2 = Just p2

      | (pathType p1 == PtIPv4 ||
         pathType p1 == PtIPv6)
         && (pathIp p1 == pathIp p2)
         && (pathPort p1 == pathPort p2)
         = Just p2

      | pathType p1 == PtHttp
         && pathHttp p1 == pathHttp p2 = Just p2

      | pathType p1 == PtLocal
         && pId p1 == pId p2 = Just p2

      -- webrtc always matches
      | pathType p1 == PtWebRtc = Just p2

      | otherwise = Nothing

    r = case catMaybes $ map (m path1) mtypes of
          [] -> Nothing
          xs -> Just $ head xs
-}
