module TeleHash.Switch
       (
         setCallbacks
       , getSwitches
       , getSwitch
       , knownSwitch
       , getNear
       , ruleMatch
       , Callbacks(..)
       , Switch(..)

       -- , io
       -- , logT
       ) where

import Control.Monad.State
import Data.String.Utils
import System.IO
import System.Log.Handler.Simple
import System.Log.Logger
import System.Time
import TeleHash.Telex
import qualified Data.Map as Map
import qualified Data.Set as Set

-- ---------------------------------------------------------------------
--
-- The 'Network' monad, a wrapper over IO, carrying the network's state.
--
type Network = StateT Switch IO


data Switch = Switch {
  maNetwork :: Map.Map IPP RSwitch
  } deriving (Eq,Show)


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


-- ---------------------------------------------------------------------

setCallbacks callbacks = undefined

getSwitches = undefined

-- ---------------------------------------------------------------------

getSwitch :: IPP -> Network RSwitch
getSwitch ipp = do
  timeNow <- io getClockTime
  getOrCreateSwitch ipp timeNow
{-
function getSwitch(ipp) {
    if (network[ipp]) return network[ipp];
    return (new Switch(ipp));
    // create new one!
} -}

-- ---------------------------------------------------------------------

getOrCreateSwitch :: IPP -> ClockTime -> Network RSwitch
getOrCreateSwitch seedIPP timeNow = do
  switchMaybe <- getSwitchMaybeM seedIPP
  case switchMaybe of
    Just switch -> return switch
    Nothing -> do
      -- console.log(["\tNEWLINE[", endpoint, "]"].join(""));
      logT $ "\tNEWLINE[" ++ (show seedIPP) ++ "]"
      -- ringOutVal <- io (R.randomRIO (1,32768) )
      let switch = mkRSwitch seedIPP timeNow Nothing
      updateMasterSwitch switch
      return switch

getSwitchMaybeM :: IPP -> Network (Maybe RSwitch)
getSwitchMaybeM ipp = do
  master <- get
  let network = (maNetwork master)
  return $ getSwitchMaybe network ipp

getSwitchMaybe :: Map.Map IPP RSwitch  -> IPP -> Maybe RSwitch
getSwitchMaybe network ipp = lineMaybe
  where
    ismember = Map.member ipp network
    member = network Map.! ipp

    lineMaybe = case (ismember) of
      True -> Just member
      False -> Nothing

updateMasterSwitch :: RSwitch -> Network ()
updateMasterSwitch switch = do
  master <- get

  let
    network = (maNetwork master)

    ipp = swiIpp switch

    network' = Map.insert ipp switch network

    master' = master {maNetwork = network'}

  put master'


-- ---------------------------------------------------------------------

knownSwitch = undefined

getNear = undefined

ruleMatch = undefined

-- ---------------------------------------------------------------------
{-
// every seen IPP becomes a switch object that maintains itself
function Switch(ipp, arg) {
    // initialize the absolute minimum here to keep this lightweight as it's used all the time
    this.ipp = ipp;
    this.hash = new hlib.Hash(ipp);
    network[this.ipp] = this;
    this.end = this.hash.toString();
    if(arg) this.via = arg.via; // optionally, which switch introduced us
    this.ATinit = Date.now();
    this.misses = 0;
    this.seed = false;
    this.ip = this.ipp.substr(0, this.ipp.indexOf(':'));
    this.port = parseInt(this.ipp.substr(this.ipp.indexOf(':') + 1));
    console.error("New Switch created: " + this.ipp);
    if( arg && (arg.via || arg.init) ){
        //this switch has been .seen or we are created directly using 'new Switch(ipp, {init:true})'
        master.news(this);//pop it, ping it and open a line!
    }else{
        //the switch is being created indirectly by getSwitch(ipp) when we get a new telex from an unknown switch
        //or when we are trying to send a telex to a yet unknown switch.
    }
    return this;
}
-}
data RSwitch = RSwitch {
  swiIpp       :: IPP,    -- IP and port of the destination
  swiHash      :: Hash,   -- Hash of the ipp (endpoint)
  swiNetwork   :: Set.Set Hash, -- lineNeighbors,
  swiEnd       :: String, -- this.hash.toString()
  swiVia       :: Maybe IPP,
  swiATinit    :: ClockTime,
  swiMisses    :: Int,
  swiSeed      :: Bool,
  swiIp        :: String, -- Split out host IP
  swiPort      :: String  -- Split out port
  } deriving (Eq,Show)

mkRSwitch :: IPP -> ClockTime -> Maybe IPP -> RSwitch
mkRSwitch endPoint@(IPP endPointStr) timeNow via =
  let
    [hostIp,port] = split ":" endPointStr
    endPointHash = mkHash endPoint
  in
   RSwitch {
       swiIpp       = endPoint,
       swiHash      = endPointHash,
       swiNetwork   = Set.fromList [endPointHash],
       swiEnd       = unHash endPointHash,
       swiVia       = via,
       swiATinit    = timeNow,
       swiMisses    = 0,
       swiSeed      = False,
       swiIp        = hostIp,
       swiPort      = port
       }

-- ---------------------------------------------------------------------
-- Convenience.
--
io :: IO a -> Network a
io = liftIO

-- ---------------------------------------------------------------------
-- Logging

logT :: String -> Network ()
logT str = io (warningM "Switch" str)


-- EOF
