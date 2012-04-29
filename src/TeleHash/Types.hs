{-# LANGUAGE FlexibleInstances #-} -- For show of swSender

module TeleHash.Types
       (
         TeleHash(..)
       , Master(..)
       , mkMaster
       , SwitchState(..)
       , Line(..)
       , Switch(..)
       , mkSwitch

       -- convenience
       , io
       , logT
       ) where

import Control.Monad.State
import Data.String.Utils
import Network.Socket
import System.Log.Handler.Simple
import System.Log.Logger
import System.Time
import TeleHash.Telex
import qualified Data.Map as Map
import qualified Data.Set as Set


--
-- | The 'TeleHash' monad, a wrapper over IO, carrying the switch's immutable state.
--
type TeleHash = StateT Master IO


data Mode = ModeAnnouncer
          | ModeListener
          | ModeFull
          deriving (Eq,Show)
{-
    FULL:3,
    LISTENER: 2,
    ANNOUNCER:1
-}

data SwitchState = StateOffline
                 | StateSeeding
                 | StateOnline
                 deriving (Eq,Show)
{-
    offline: 0,
    seeding: 1,
    online: 2
-}

data Master = Master
            { selfMode      :: Mode
            , selfState     :: SwitchState
            , selfSeeds     :: [IPP]
            , selfNat       :: Maybe Bool
            , selfSNat      :: Maybe Bool
            , selfServer    :: Maybe Socket
            , selfCallbacks :: Callbacks
            , selfMe        :: Maybe Switch
            , selfNetwork   :: Map.Map IPP Switch

            , selfListeners :: [Tap]
                               -- ^maintain an array of .tap rules we are interested in
            , selfConnectors :: Set.Set IPP
                                -- ^maintains a hashtable of ends we are interested in contacting indexed by a end name.
            , selfResponseHandlers :: Map.Map Hash String
                                      -- ^maintains a hashtable of response handlers indexed by connection 'guid'


            -- Stats stuff
            , selfCountOnline :: Int
            , selfCountTx     :: Int
            , selfCountRx     :: Int

            , selfSender :: (String -> SockAddr -> TeleHash ())

            }
            deriving (Show)

mkMaster = Master
                  {
                    selfMode  = ModeListener
                  , selfState = StateOffline
                  , selfSeeds = [IPP "208.68.164.253:42424", IPP "208.68.163.247:42424"]
                  , selfNat       = Nothing
                  , selfSNat      = Nothing
                  , selfServer    = Nothing
                  , selfCallbacks = mkCallbacks ModeListener
                  , selfMe        = Nothing
                  , selfNetwork   = Map.empty

                  , selfListeners  = []
                  , selfConnectors = Set.empty
                  , selfResponseHandlers = Map.empty

                  , selfCountOnline = 0
                  , selfCountTx = 0
                  , selfCountRx = 0

                  , selfSender = doNullSendDgram
                  }

data Callbacks = Callbacks
                 { cbSock :: String
                 , cbNat  :: String
                 , cbSnat :: String
                 , cbNews :: String
                 , cbData :: String
                 , cbSignals :: String
                 , cbMode :: String
                 }
               deriving (Show)

mkCallbacks _ =
  Callbacks
           { cbSock    = "Socket"
           , cbNat     = "Nat"
           , cbSnat    = "Snat"
           , cbNews    = "News"
           , cbData    = "Data"
           , cbSignals = "Signals"
           , cbMode    = "Mode"
           }

data Switch = Switch {
  swiIpp        :: IPP,    -- IP and port of the destination
  swiHash       :: Hash,   -- Hash of the ipp (endpoint)
  swiNetwork    :: Set.Set Hash, -- lineNeighbors,
  swiEnd        :: String, -- this.hash.toString()
  swiVia        :: Maybe IPP,

  swiRingout    :: Int,    --  rand(32768),
  swiRingin     :: Maybe Int,
  swiLine       :: Maybe Int, -- When line is live, product of our ringout and their ringin

  swiATinit     :: ClockTime,
  swiATrecv     :: Maybe ClockTime,
  swiATsent     :: Maybe ClockTime,
  swiATexpected :: Maybe ClockTime,
  swiATline     :: Maybe ClockTime,
  swiATdropped  :: Maybe ClockTime,

  swiMisses     :: Maybe Int,
  swiSeed       :: Bool,
  swiIsSelf     :: Bool, -- If this switch represents us

  swiBr         :: Int,
  swiBrout      :: Int,
  swiBrin       :: Int,
  swiBsent      :: Int,

  swiIp         :: String, -- Split out host IP
  swiPort       :: String  -- Split out port
  } deriving (Eq,Show)

mkSwitch :: IPP -> ClockTime -> Int -> Maybe IPP -> Switch
mkSwitch endPoint@(IPP endPointStr) timeNow ringOutVal via =
  let
    [hostIp,port] = split ":" endPointStr
    endPointHash = mkHash endPoint
  in
   Switch {
       swiIpp        = endPoint,
       swiHash       = endPointHash,
       swiNetwork    = Set.fromList [endPointHash],
       swiEnd        = unHash endPointHash,
       swiVia        = via,

       swiRingout   = ringOutVal,
       swiRingin    = Nothing,
       swiLine      = Nothing,

       swiATinit     = timeNow,
       swiATrecv     = Nothing,
       swiATsent     = Nothing,
       swiATexpected = Nothing,
       swiATline     = Nothing,
       swiATdropped  = Nothing,
       swiMisses     = Just 0,
       swiSeed       = False,
       swiIsSelf     = False,

       swiBr         = 0,
       swiBrout      = 0,

       swiBrin       = 0,
       swiBsent      = 0,
       swiIp         = hostIp,
       swiPort       = port
       }

-- OLD
-- ---------------------------------------------------------------------

--
-- | The state variable for a given TeleHash Switch
data OSwitch = OSwitch { swH :: Maybe SocketHandle
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

instance (Show (String -> SockAddr -> TeleHash ())) where
  --show doNullSendDgram = "doNullSendDgram"
  -- show doSendDgram     = "doSendDgram"
  show _               = "send func"

instance (Eq (String -> SockAddr -> TeleHash ())) where
  (==) _ _ = True
  (/=) _ _ = False



data SocketHandle =
    SocketHandle {slSocket :: Socket
                 --, slAddress :: SockAddr
                 } deriving (Eq,Show)

data Line = Line {
  lineIpp       :: IPP,    -- IP and port of the destination
  lineEnd       :: Hash,   -- Hash of the ipp (endpoint)
  lineHost      :: String, -- Split out host IP
  linePort      :: String, -- Split out port
  lineRingout   :: Int,    --  rand(32768),
  lineRingin    :: Maybe Int,
  lineLine      :: Maybe Int, -- When line is live, product of our ringout and their ringin
  lineInit      :: ClockTime,
  lineSeenat    :: Maybe ClockTime,
  lineSentat    :: Maybe ClockTime,
  lineLineat    :: Maybe ClockTime,
  lineTapLast   :: Maybe ClockTime,
  lineBr        :: Int,
  lineBrout     :: Int,
  lineBrin      :: Int,
  lineBsent     :: Int,
  lineNeighbors :: Set.Set Hash, -- lineNeighbors,
  lineVisible   :: Bool,
  lineVisibled  :: Bool,
  lineRules     :: [Tap]
  } deriving (Eq,Show)


-- ---------------------------------------------------------------------

doNullSendDgram :: String -> SockAddr -> TeleHash ()
doNullSendDgram msgJson addr = do
  --logT ("doNullSendDgram[" ++ msgJson ++ "] to " ++ (show addr))
  logT ("doNullSendDgram" )

-- ---------------------------------------------------------------------
-- Convenience.
--
io :: IO a -> TeleHash a
io = liftIO

-- ---------------------------------------------------------------------
-- Logging

logT :: String -> TeleHash ()
logT str = io (warningM "Switch" str)


-- EOF