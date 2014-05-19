module Network.TeleHash.Ext.Peer
  (
  ext_peer
  ) where

import Data.List
import Data.Maybe
import Prelude hiding (id, (.), head, either)

import Network.TeleHash.Types
import Network.TeleHash.Utils
import Network.TeleHash.SwitchUtils

-- ---------------------------------------------------------------------

ext_peer :: TChan -> TeleHash ()
ext_peer c = do
  logT $ "ext_peer entered for:" ++ show (chId c, chUid c)
  util_chan_popall c (Just (\p -> logT $ "TODO peer packet:" ++ showJson (rtJs p)))

