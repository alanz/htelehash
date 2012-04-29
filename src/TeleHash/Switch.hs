module TeleHash.Switch
       (
         setCallbacks
       , getSwitches
       , getSwitch
       , knownSwitch
       , getNear
       , ruleMatch
       , Switch(..)
       , getOrCreateSwitch
       , resolveToSeedIPP
       , sendTelex
       , doSendDgram

       -- , io
       -- , logT
       ) where

import Control.Monad.State
import Data.List
import Data.Maybe
import Data.String.Utils
import Network.BSD
import Network.Socket
import System.IO
import System.Time
import TeleHash.Telex
import TeleHash.Types
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System.Random as R

-- ---------------------------------------------------------------------

setCallbacks callbacks = undefined

getSwitches = undefined

-- ---------------------------------------------------------------------

getSwitch :: IPP -> TeleHash Switch
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

getOrCreateSwitch :: IPP -> ClockTime -> TeleHash Switch
getOrCreateSwitch seedIPP timeNow = do
  switchMaybe <- getSwitchMaybeM seedIPP
  case switchMaybe of
    Just switch -> return switch
    Nothing -> do
      -- console.log(["\tNEWLINE[", endpoint, "]"].join(""));
      logT $ "\tNEWLINE[" ++ (show seedIPP) ++ "]"
      ringOutVal <- io (R.randomRIO (1,32768) )
      let switch = mkSwitch seedIPP timeNow ringOutVal Nothing
      updateMasterSwitch switch
      return switch

getSwitchMaybeM :: IPP -> TeleHash (Maybe Switch)
getSwitchMaybeM ipp = do
  master <- get
  let network = (selfNetwork master)
  return $ getSwitchMaybe network ipp

getSwitchMaybe :: Map.Map IPP Switch  -> IPP -> Maybe Switch
getSwitchMaybe network ipp = lineMaybe
  where
    ismember = Map.member ipp network
    member = network Map.! ipp

    lineMaybe = case (ismember) of
      True -> Just member
      False -> Nothing

updateMasterSwitch :: Switch -> TeleHash ()
updateMasterSwitch switch = do
  master <- get

  let
    network = (selfNetwork master)

    ipp = swiIpp switch

    network' = Map.insert ipp switch network

    master' = master {selfNetwork = network'}

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

-- ---------------------------------------------------------------------
{-
getOrCreateSwitch :: IPP -> ClockTime -> TeleHash Switch
getOrCreateSwitch seedIPP timeNow = do
  switchMaybe <- getSwitchMaybeM (mkHash seedIPP)
  case switchMaybe of
    Just switch -> return switch
    Nothing -> do
      -- console.log(["\tNEWLINE[", endpoint, "]"].join(""));
      logT $ "\tNEWLINE[" ++ (show seedIPP) ++ "]"
      ringOutVal <- io (R.randomRIO (1,32768) )
      let switch = mkSwitch seedIPP timeNow ringOutVal
      updateTelehashSwitch switch
      return switch
-}

-- ---------------------------------------------------------------------

{-
getSwitchMaybeM :: Hash -> TeleHash (Maybe Switch)
getSwitchMaybeM hashIpp = do
  switch <- get
  let master = (swMaster switch)
  return $ getSwitchMaybe master hashIpp
-}
{-
getSwitchMaybe :: Map.Map Hash Line  -> Hash -> Maybe Line
getSwitchMaybe master hashIpp  = lineMaybe
  where
    ismember = Map.member hashIpp master
    member = master Map.! hashIpp

    lineMaybe = case (ismember) of
      True -> Just member
      False -> Nothing
-}
{-
removeLineM :: Hash -> TeleHash ()
removeLineM hash = do
  switch <- get
  master <- gets swMaster

  -- put $ switch {swMaster = Map.delete hash master}
  -- TODO: what about neighbours?
  let
    master' = Map.fromList $
              map (\(h,line) -> (h,line {lineNeighbors = Set.delete hash (lineNeighbors line)})) $
              Map.toList $ Map.delete hash master

  put $ switch {swMaster = master'}
-}
updateTelehashSwitch :: Switch -> TeleHash ()
updateTelehashSwitch switch = do
  master <- get

  let
    network = selfNetwork master

    endpointHash = swiHash switch
    endpointIpp  = swiIpp switch

    network' = Map.insert endpointIpp switch network

    master' = master {selfNetwork = network'}

  put master'


-- ---------------------------------------------------------------------

sendTelex :: Telex -> TeleHash ()
sendTelex msg = do
  timeNow <- io getClockTime
  res <- prepareTelex msg timeNow
  case (res) of
    Nothing -> return ()
    Just (line,msgJson) -> do
      -- console.log(["SEND[", telex._to, "]\t", msg].join(""));
      logT ( "SEND[:" ++ (show $ teleTo msg) ++ "]\t" ++ (msgJson))

      master <- get
      put master {selfCountTx = (selfCountTx master) + 1 }

      addr <- io (addrFromHostPort (swiIp line) (swiPort line))
      --Just socketh <- gets swH
      --io (sendDgram socketh msgJson addr)
      sender <- gets selfSender
      sender msgJson addr

      updateTelehashSwitch(line)

-- ---------------------------------------------------------------------

resolveToSeedIPP :: IPP -> IO (AddrInfo,String,String)
resolveToSeedIPP (IPP addr) = do
  let [hostname,port] = split ":" addr
  resolve hostname port

-- ---------------------------------------------------------------------

resolve :: String -> String -> IO (AddrInfo,String,String)
resolve hostname port = do
  -- Look up the hostname and port.  Either raises an exception
  -- or returns a nonempty list.  First element in that list
  -- is supposed to be the best option.
  addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
  let serveraddr = head addrinfos

  (Just resolvedhost,Just servicename) <- (getNameInfo [NI_NUMERICHOST] True True (addrAddress serveraddr))
  --putStrLn $ "resolve:" ++ (show hostname) ++ " " ++ (show servicename)

  --return (serveraddr,port)
  return (serveraddr,resolvedhost,servicename)

-- ---------------------------------------------------------------------

addrFromHostPort :: String -> String -> IO SockAddr
addrFromHostPort hostname port = do
  (serveraddr,_,_) <- resolve hostname port
  return (addrAddress serveraddr)

-- ---------------------------------------------------------------------

prepareTelex :: Telex -> ClockTime -> TeleHash (Maybe (Switch,String))
prepareTelex msg timeNow = do
  switch <- getOrCreateSwitch (teleTo msg) timeNow

  let ok  = not ((swiIsSelf switch) || (swiATdropped switch /= Nothing))

  case ok of
    False -> return Nothing
    True -> do
      -- if (this.ATexpected < Date.now()) this.misses = this.misses + 1 || 1;
      -- delete this.ATexpected;
      let misses = if ((swiATexpected switch /= Nothing) &&
                       (fromMaybe (TOD 0 0) (swiATexpected switch)) < timeNow)
                   then (safeAdd (swiMisses switch) 1)
                   else (swiMisses switch)

          expected = if ((teleSigEnd msg /= Nothing)
                         &&
                         ((teleHop msg == Nothing) || (teleHop msg == Just 0)))
                     then (Just (addSeconds timeNow 10))
                     else Nothing

          switch' = switch {swiMisses = misses, swiATexpected = expected}

          brBad = (swiBsent switch) - (swiBrin switch) > 10000
      case brBad of
        True -> do
          logT ( "MAX SEND DROP ")
          return Nothing
        False -> do
          -- if a line is open use that, else send a ring
          let
            msg' = if (swiLine switch == Nothing)
                   then (msg {teleRing = Just (swiRingout switch)})
                   else (msg {teleLine = Just (fromJust (swiLine switch))})

            -- update our bytes tracking and send current state
            -- telex._br = line.brout = line.br;
            msg'' = msg' { teleBr = swiBr switch }

            msgJson = encodeTelex msg''

            switch' = switch {
              swiBrout  = swiBr switch
              , swiBsent  = (swiBsent switch) + (length msgJson)
              , swiATsent = Just timeNow
              }

          return (Just (switch',msgJson))

-- ---------------------------------------------------------------------

safeAdd :: Maybe Int -> Int -> Maybe Int
safeAdd Nothing v = Just v
safeAdd (Just v1) v = Just (v1+v)

-- ---------------------------------------------------------------------

addSeconds :: ClockTime -> Integer -> ClockTime
addSeconds (TOD secs picosecs) secsToAdd = TOD (secs + secsToAdd) picosecs

-- ---------------------------------------------------------------------

sendDgram :: Socket -> String -> SockAddr -> IO ()
sendDgram socket msgJson addr =
  sendstr msgJson
    where
      -- Send until everything is done
      sendstr :: String -> IO ()
      sendstr [] = return ()
      sendstr omsg = do sent <- sendTo socket omsg addr
                        sendstr (genericDrop sent omsg)

-- ---------------------------------------------------------------------

doSendDgram :: String -> SockAddr -> TeleHash ()
doSendDgram msgJson addr = do
  Just socket <- gets selfServer
  io (sendDgram socket msgJson addr)


-- EOF
