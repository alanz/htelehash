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
       , process

       , healthy
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

knownSwitch :: IPP -> TeleHash Bool
knownSwitch ipp = do
  master <- get
  return (Map.member ipp (selfNetwork master))

getNear = undefined

ruleMatch = undefined

-- ---------------------------------------------------------------------

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
      logT ( "-->[:" ++ (show $ teleTo msg) ++ "]\t" ++ (msgJson))

      master <- get
      put master {selfCountTx = (selfCountTx master) + 1 }

      addr <- io (addrFromHostPort (swiIp line) (swiPort line))
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
      let
        -- if last time we sent there was an expected response and
        -- never got it, count it as a miss for health check
        misses = if ((swiATexpected switch /= Nothing) &&
                     (fromMaybe (TOD 0 0) (swiATexpected switch)) < timeNow)
                 then (safeAdd (swiMisses switch) 1)
                 else (swiMisses switch)

        -- if we expect a reponse, in 10sec we should count it as a miss if nothing
        -- if we are forwarding an +end signal (_hop > 0) dont expect a .see response.
        expected = if ((teleSigEnd msg /= Nothing)
                       &&
                       ((teleHop msg == Nothing) || (teleHop msg == Just 0)))
                   then (Just (addSeconds timeNow 10))
                   else Nothing

        switch' = switch {swiMisses = misses, swiATexpected = expected}

        -- check bytes sent vs received and drop if too much so we don't flood
        brBad = (swiBsent switch) - (swiBrin switch) > 10000
      case brBad of
        True -> do
          -- console.error("FLOODING " + this.ipp + ", dropping " + JSON.stringify(telex));
          logT ( "FLOODING " ++ (show $ swiIpp switch) ++ ", dropping " ++ (show msg))
          -- logT ( "MAX SEND DROP ")
          return Nothing
        False -> do

          -- if(master.mode() != MODE.ANNOUNCER ){
          --     if (!this.ring) this.ring = Math.floor((Math.random() * 32768) + 1);
          -- }

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

-- ---------------------------------------------------------------------
{-
// necessary utility to see if the switch is in a known healthy state
Switch.prototype.healthy = function () {
    if (this.self) return true; // we're always healthy haha
    //if(!this.popped) return true; //give a chance for switch to atleast get popped
    if (this.ATdropped) return false;
    if (this.ATinit > (Date.now() - 10000)) return true; // new switches are healthy for 10 seconds!
    if (!this.ATrecv) return false; // no packet, no love
    if (Date.now() > (this.ATrecv + 60000)) return false; //haven't recieved anything in last minute
    if (this.misses > 2) return false; // three strikes
    if (this.Bsent - this.BRin > 10000) return false; // more than 10k hasn't been acked
    return true; // <3 everyone else
}

-}
healthy :: Switch -> ClockTime -> Bool
healthy switch timeNow =
  let
    selfOk = swiIsSelf switch

    atDroppedOk = case (swiATdropped switch) of
      Nothing -> True
      Just _time -> False

    atInitOk = (swiATinit switch) > (addSeconds timeNow (-10))

    atRecvOk = case (swiATrecv switch) of
      Nothing -> False
      Just rxTime -> not (timeNow > (addSeconds rxTime 60))

    missesOk = case (swiMisses switch) of
      Nothing -> True
      Just misses -> misses <= 2

    brOk = not ((swiBsent switch) - (swiBrin switch) > 10000)

  in
   -- Relying on short circuit evaluation
   selfOk && atDroppedOk && atInitOk && atRecvOk && missesOk && brOk

-- ---------------------------------------------------------------------

-- | Process incoming telex from this switch

process :: Switch -> Telex -> TeleHash ()
process switch telex = do
  return ()
  {-
    // do all the integrity and line validation stuff
    if (!validate(this, telex)) return;

    if (this.ATdropped) return; //dont process telexes from switches marked to be purged!
    // basic header tracking
    if (!this.BR) this.BR = 0;
    this.BR += rawlen;
    // they can't send us that much more than what we've told them to, bad!
    if (this.BRout && this.BR - this.BRout > 12000) return;
    this.BRin = (telex._br) ? parseInt(telex._br) : undefined;
    if (this.BRin < 0) delete this.line; // negativity is intentionally signalled line drop (experimental)

    // TODO, if no ATrecv yet but we sent only a single +end last (dialing) and a +pop request for this ip, this
    // could be a NAT pingback and we should re-send our dial immediately


    // timer tracking
    this.ATrecv = Date.now();

    // responses mean healthy
    delete this.ATexpected;
    delete this.misses;

    // process serially per switch
    telex._ = this; // async eats 'this'
    if (!this.queue) this.queue = async.queue(worker, 1);
    this.queue.push(telex);
  -}

-- ---------------------------------------------------------------------

-- | Make sure this telex is valid coming from this switch, and twiddle our bits
validate :: Switch -> Telex -> TeleHash Bool
validate switch telex = do
  timeNow <- io getClockTime
  let
    -- first, if it's been more than 10 seconds after a line opened,
    -- be super strict, no more ringing allowed, _line absolutely required
    lineTimeOk = case (swiATline switch) of
      Nothing -> True
      Just atLine -> (addSeconds atLine 10) < timeNow

    lineOk = (swiLine switch) == (teleLine telex)
    -- if (s.ATline && s.ATline + 10000 < Date.now() && t._line != s.line) return false;

    ringOutVal = fromMaybe 32769 (swiRingout switch)

  case (lineTimeOk && lineOk) of
    False -> return False
    True -> do
      -- second, process incoming _line

      case (teleLine telex) of
        Nothing -> do ()
        Just msgLineNum -> do
          let
            -- can't get a _line w/o having sent a _ring
            ringSent = swiRingout switch /= Nothing

            msgLineOk = case (swiLine switch) of
              Just lineNum -> lineNum == msgLineNum && (lineNum `mod` ringOutVal == 0)
              Nothing -> True -- only test if we have a value


          -- we can set up the line now if needed
          switch' = case (ringSent && msgLineOk) of
            False -> switch
            True -> do
              case (swiLine switch) of
                Just _ -> switch
                Nothing -> switch { swiRingin = Just (msgLineNum `mod` ringOutVal),
                                    swiLine = msgLineNum,
                                    swiATline = Just timeNow
                                  }

    {-

    if (t._line) {
        // can't get a _line w/o having sent a _ring
        if (s.ring == undefined) return false;

        // be nice in what we accept, strict in what we send
        t._line = parseInt(t._line);

        // must match if exist
        if (s.line && t._line != s.line) return false;

        // must be a product of our sent ring!!
        if (t._line % s.ring != 0) return false;

        // we can set up the line now if needed
        if (!s.line) {
            s.ringin = t._line / s.ring; // will be valid if the % = 0 above
            s.line = t._line;
            s.ATline = Date.now();
        }
    }

    // last, process any incoming _ring's (remember, could be out of order after a _line and still be valid)
    if (t._ring) {

        // be nice in what we accept, strict in what we send
        t._ring = parseInt(t._ring);

        // already had a ring and this one doesn't match, should be rare
        if (s.ringin && t._ring != s.ringin) return false;

        // make sure within valid range
        if (t._ring <= 0 || t._ring > 32768) return false;

        // we can set up the line now if needed
        //if(s.ATline == 0){ //will never be true!

        if (master.mode() != MODE.ANNOUNCER && !s.ATline) { //changed this to calculate the _line on first packet received from a switch with _ring
            s.ringin = t._ring;
            if (!s.ring) s.ring = Math.floor((Math.random() * 32768) + 1);
            s.line = s.ringin * s.ring;
            s.ATline = Date.now();
        }
    }

    // we're valid at this point, line or otherwise
    return true;
}
-}
  return False



-- EOF
