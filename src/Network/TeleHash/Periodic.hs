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
import Network.TeleHash.Ext.Path
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
        then do
          dhtMaint
        else return ()

      if now `mod` param_path_sync_secs == 0
        then do
          sw2 <- get
          forM_ (Map.keys (swIndex sw2)) $ \hn ->do
            path_sync hn
        else return ()

      if now `mod` param_reseed_secs == 0
        then do
          sw2 <- get
          forM_ (Set.toList (swSeeds sw2)) $ \seed -> do
            void $ link_hn seed Nothing
        else return ()

      return ()
