module Network.TeleHash.Ext.Peer
  (
  ext_peer
  ) where

import Control.Exception
import Data.List
import Data.Maybe
import Prelude hiding (id, (.), head, either)

import Network.TeleHash.Types
import Network.TeleHash.Utils
import Network.TeleHash.SwitchApi
import Network.TeleHash.SwitchUtils

-- ---------------------------------------------------------------------

ext_peer :: TChan -> TeleHash ()
ext_peer c = do
  logT $ "ext_peer entered for:" ++ show (chId c, chUid c)

  let
    respFunc p = do
      logT $ "ext_peer:respFunc:processing " ++ showJson (rtJs p)
      -- always respond/ack, except if there is an error or end
      let merr = packet_get_str p "err"
          mend = packet_get_str p "end"
      if any isJust [merr,mend]
        then do
          chan_fail (chUid c) Nothing
          return ()
        else do
          assert False undefined

  util_chan_popall c (Just respFunc)

