module Network.TeleHash.Path
  (
    path_alive
  , path_match
  ) where

import Control.Monad
import Data.Maybe
import Prelude hiding (id, (.), head, either)
import System.Time

import Network.TeleHash.Types
import Network.TeleHash.Utils

-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import qualified Data.Text as Text

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
