module Network.TeleHash.Periodic
  (
    switch_loop
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.State
import Data.List
import Data.Maybe
import Prelude hiding (id, (.), head, either)
import System.Time

import Network.TeleHash.Convert
import Network.TeleHash.Crypt
import Network.TeleHash.Dht
import Network.TeleHash.Hn
import Network.TeleHash.Packet
import Network.TeleHash.Path
import Network.TeleHash.Paths
import Network.TeleHash.Types
import Network.TeleHash.SwitchApi
import Network.TeleHash.Utils

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
-- import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text


-- ---------------------------------------------------------------------

-- |give all channels a chance
switch_loop :: TeleHash ()
switch_loop = do
  (TOD now _) <- io getClockTime
  sw <- get
  if swTick sw == now
    then return ()
    else do
      put $ sw { swTick = now }

      -- give all channels a tick
      forM_ (Map.keys (swIndexChans sw)) $ \cid -> do
        -- logT $ "switch_loop: processing : " ++ show cid
        chan_tick cid

      -- perform link maintenance every 29 secs
      -- logT $ "switch_loop:checking for dht maint :" ++ show (now,now `mod` param_link_ping_secs)
      if now `mod` param_link_ping_secs == 0
        then dhtMaint
        else return ()
      return ()

{-
// fire tick events no more than once a second
void switch_loop(switch_t s)
{
  hn_t hn;
  int i = 0;
  uint32_t now = platform_seconds();
  if(s->tick == now) return;
  s->tick = now;

  // give all channels a tick
  while((hn = bucket_get(s->active,i)))
  {
    i++;
    chan_tick(s,hn);
  }
}
-}
