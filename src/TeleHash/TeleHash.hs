module TeleHash.TeleHash
       (
         initialize
       , seed
       , listen
       , connect
       , send
       , tap
       , dial
       , announce
       , ping
       , shutdown
       ) where

import Control.Monad.State

initialize = undefined

seed callback = undefined

listen = undefined

connect = undefined

send = undefined

tap = undefined

dial = undefined

announce = undefined

ping = undefined

shutdown = undefined

--
-- | The 'TeleHash' monad, a wrapper over IO, carrying the switch's immutable state.
--
type TeleHash = StateT Switch IO

--
-- | The state variable for a given TeleHash Switch
data Switch = Switch { swH :: Maybe SocketHandle
                     , swSeeds :: [String] -- IPP?
                     , swSeedsIndex :: Set.Set IPP
                     , swConnected :: Bool
                     , swMaster :: Map.Map Hash Line
                     , swSelfIpp :: Maybe IPP
                     , swSelfHash :: Maybe Hash
                     , swTaps :: [Tap]
                     , swCountOnline :: Int
                     , swCountTx :: Int
                     , swCountRx :: Int
                     , swSender :: (String -> SockAddr -> TeleHash ())
                       } deriving (Eq,Show)


pingSeeds :: TeleHash ()
pingSeeds = do
  seeds <- gets swSeeds
  connected <- gets swConnected

  -- logT $ "pingSeeds:" ++ (show connected) ++ " " ++ (show seeds)

  -- TODO: rotate the seeds, so the we use a fresh one each time through
  case (not connected) && (seeds /= []) of
    True -> do
      nextSeed <- rotateToNextSeed
      pingSeed nextSeed
    False -> return ()


-- EOF

