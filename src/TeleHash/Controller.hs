{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

{- # LANGUAGE TypeOperators #-}
{- # LANGUAGE TemplateHaskell #-}
{- # LANGUAGE NoMonoPatBinds #-}

-- Above three settings are for the Json stuff using Data.Iso

module TeleHash.Controller
       (
       runMaster
       , startMasterThread
       , Signal(..)
       , Reply(..)
       , ReplyUser(..)
       , queryMaster
       , sendUserMsg
       -- For testing
       , recvTelex
       , parseTelex
       , mkTelex
       , Telex(..)
       , Hash(..)
       , unHash
       , Tap(..)
       , Switch(..)
       , IPP(..)
       , unIPP
       , Master(..)
       , SocketHandle(..)
       , TeleHash()
       , encodeTelex
       , isLineOk
       , isRingOk
       , mkSwitch
       , checkSwitch
       , getCommands
       , getSignals
       , mkHash
       , distanceTo
       , near_to
       , seeVisible
       , getSwitchMaybe
       , resolveToSeedIPP
       , addrFromHostPort
       , getOrCreateSwitch
       , online
       , prepareTelex
       , removeSwitchM
       , processSee
       , processCommand
       , doNullSendDgram
       , scanlines
       , rotateToNextSeed
       , tapSignals
       , processSignals
       ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Data.Bits
import Data.Char
import Data.List
import Data.Maybe
import Data.String.Utils
import Debug.Trace
import Network.BSD
import Network.Socket
import Prelude hiding (id, (.), head, either, catch)
import System.IO
import System.Log.Handler.Simple
import System.Log.Logger
import System.Time
--import TeleHash.Telex
import TeleHash.TelexOld
import Text.JSON
import Text.JSON.Generic
import Text.JSON.Types
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Network.Socket.ByteString as SB
import qualified System.Random as R

--
-- The 'TeleHash' monad, a wrapper over IO, carrying the master's immutable state.
--
type TeleHash = StateT Master IO

data Master = Master { selfH :: Maybe SocketHandle
                     , selfSeeds :: [String] -- IPP?
                     , selfSeedsIndex :: Set.Set IPP
                     , selfConnected :: Bool
                     , selfNetwork :: Map.Map Hash Switch
                     , selfSelfIpp :: Maybe IPP
                     , selfSelfHash :: Maybe Hash
                     , selfTaps :: [Tap]
                     , selfCountOnline :: Int
                     , selfCountTx :: Int
                     , selfCountRx :: Int
                     , selfSender :: (String -> SockAddr -> TeleHash ())
                       } deriving (Eq,Show)

-- instance (Show Master) where
--   show sw = "{" ++ (intersperse "," [show(selfH),show(selfSeeds),show(selfSeedsIndex),show(selfConnected),show(selfNetwork),show(selfSelfIpp),show(selfSelfHash),show(selfTaps),show(selfCountOnline),show(selfCountTx),show(selfCountRx)]) ++ "}"

instance (Show (String -> SockAddr -> TeleHash ())) where
  --show doNullSendDgram = "doNullSendDgram"
  --show doSendDgram     = "doSendDgram"
  show _               = "send func"

instance (Eq (String -> SockAddr -> TeleHash ())) where
  (==) _ _ = True
  (/=) _ _ = False

nBuckets :: Int
nBuckets = 160

data SocketHandle =
    SocketHandle {slSocket :: Socket
                 --, slAddress :: SockAddr
                 } deriving (Eq,Show)

data Switch = Switch {
  swiIpp       :: IPP,    -- IP and port of the destination
  swiEnd       :: Hash,   -- Hash of the ipp (endpoint)
  swiHost      :: String, -- Split out host IP
  swiPort      :: String, -- Split out port
  swiRingout   :: Int,    --  rand(32768),
  swiRingin    :: Maybe Int,
  swiLine      :: Maybe Int, -- When line is live, product of our ringout and their ringin
  swiATinit      :: ClockTime,
  swiATseen    :: Maybe ClockTime,
  swiATsent    :: Maybe ClockTime,
  swiATline    :: Maybe ClockTime,
  swiATlastTap   :: Maybe ClockTime,
  swiBr        :: Int,
  swiBrout     :: Int,
  swiBrin      :: Int,
  swiBsent     :: Int,
  swiNeighbors :: Set.Set Hash, -- swiNeighbors,
  swiVisible   :: Bool,
  swiVisibled  :: Bool,
  swiRules     :: [Tap]
  } deriving (Eq,Show)

mkSwitch :: IPP -> ClockTime -> Int -> Switch
mkSwitch endPoint@(IPP endPointStr) timeNow ringOutVal =
  let
    [hostIP,port] = split ":" endPointStr
    endPointHash = mkHash endPoint
    --(TOD _secs picosecs) = timeNow
    -- ringOut = fromIntegral (1 + (picosecs `mod` 32768))  -- TODO: rand 1..32768
    --ringOut <- liftIO (R.randomRIO (1,32768))
  in
   Switch {
       swiIpp       = endPoint,
       swiEnd       = endPointHash,
       swiHost      = hostIP,
       swiPort      = port,
       swiRingout   = ringOutVal,
       swiRingin    = Nothing,
       swiLine      = Nothing,
       swiATinit      = timeNow,
       swiATseen    = Nothing,
       swiATsent    = Nothing,
       swiATline    = Nothing,
       swiATlastTap   = Nothing,
       swiBr        = 0,
       swiBrout     = 0,
       swiBrin      = 0,
       swiBsent     = 0,
       swiNeighbors = Set.fromList [endPointHash],
       swiVisible   = False,
       swiVisibled  = False,
       swiRules     = []
       }

-- ---------------------------------------------------------------------
{-
getRandom :: Int -> Int
getRandom seed =
  let
    (res,_) = R.randomR (0,32767) (R.mkStdGen seed)
  in
   res
-}
-- ---------------------------------------------------------------------

main :: IO ((), Master)
main = do
  s <- streamHandler stdout DEBUG
  -- updateGlobalLogger rootLoggerName (addHandler s) -- setHandlers [s]
  updateGlobalLogger rootLoggerName (setHandlers [s])
  runMaster

-- ---------------------------------------------------------------------

data Signal = SignalPingSeeds | SignalScanSwitchs | SignalTapTap | SignalMsgRx String SockAddr
            | SignalGetMaster
            | SignalSendUserMsg [(String,String)] -- ^Send a user message, having the JSON key:value pairs given
              deriving (Typeable, Show, Eq)

data Reply = ReplyGetMaster Master
           deriving (Typeable, Show)

data ReplyUser = ReplyUserString String
           deriving (Typeable, Show)

onesec :: Int
onesec = 1000000

timer :: Int -> a -> Chan a -> IO ()
timer timeoutVal signalValue channel  = forever $
  threadDelay timeoutVal >> writeChan channel signalValue

-- ---------------------------------------------------------------------
-- Routines to interact with the running master, via the comms channel
queryMaster :: Chan Signal -> Chan b -> IO b
queryMaster ch1 ch2 = do
  writeChan ch1 SignalGetMaster
  res <- readChan ch2
  return res

sendUserMsg :: Chan Signal -> [(String,String)] -> IO ()
sendUserMsg ch1 vals = do
  writeChan ch1 (SignalSendUserMsg vals)

-- ---------------------------------------------------------------------
-- Logging

logT :: String -> TeleHash ()
logT str = io (warningM "Controller" str)

-- ---------------------------------------------------------------------
--
-- Set up actions to run on start and end, and run the main loop
--
runMaster :: IO ((),Master)
runMaster = bracket initialize disconnect loop
  where
    disconnect (_,_,_,ss) = sClose (slSocket (fromJust $ selfH ss))

    loop (ch1,ch2,ch3,st) = catch (runStateT (run ch1 ch2 ch3) st) (exc)

    exc :: SomeException -> IO ((),Master)
    exc _e = return ((),undefined)

-- ---------------------------------------------------------------------
--
-- Set up actions to run on start and end, and run the main loop in
-- its own thread
--

startMasterThread :: IO (Chan Signal,Chan Reply,Chan ReplyUser,ThreadId)
startMasterThread = do
  (ch1,ch2,ch3,st) <- initialize
  -- thread <- forkIO (io (runStateT run st))
  thread <- forkIO (doit ch1 ch2 ch3 st)
  return (ch1,ch2,ch3,thread)

  where
    doit :: Chan Signal -> Chan Reply -> Chan ReplyUser -> Master -> IO ()
    doit ch1 ch2 ch3 st = do
      _ <- runStateT (run ch1 ch2 ch3) st
      return ()


-- ---------------------------------------------------------------------
-- Hardcoded params for now
--initialSeeds = ["telehash.org:42424","6.singly.com:42424","208.68.160.25:42424"]
-- initialSeeds = ["208.68.160.25:42424","telehash.org:42424","6.singly.com:42424"]
initialSeeds = ["208.68.164.253:42424", "208.68.163.247:42424"]

-- s.setSeeds(["telehash.org:42424","6.singly.com:42424","208.68.160.25:42424"]);


initialize :: IO (Chan a,Chan b,Chan c,Master)
initialize = do
  -- Look up the hostname and port.  Either raises an exception
  -- or returns a nonempty list.  First element in that list
  -- is supposed to be the best option.

  -- (serveraddr,ip,port) <- resolveToSeedIPP initialSeed
  -- let seedIPP = IPP (ip ++ ":" ++ port)

  -- Establish a socket for communication
  --sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  sock <- socket AF_INET Datagram defaultProtocol

  -- We want to listen on all interfaces (0.0.0.0)
  bindAddr <- inet_addr "0.0.0.0"
  bindSocket sock (SockAddrInet 0 bindAddr)

  socketName <- getSocketName sock
  warningM "Controller" ("server listening " ++ (show socketName))

  ch1 <- newChan
  ch2 <- newChan
  ch3 <- newChan

  -- Save off the socket, and server address in a handle
  return $ (ch1, ch2, ch3,
            (Master (Just (SocketHandle sock)) initialSeeds
             -- (Set.fromList [seedIPP])
             Set.empty
             False Map.empty Nothing Nothing [] 0 0 0 doSendDgram))

-- ---------------------------------------------------------------------

resolveToSeedIPP :: String -> IO (AddrInfo,String,String)
resolveToSeedIPP addr = do
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

run :: Chan Signal -> Chan Reply -> Chan ReplyUser -> TeleHash ()
run ch1 ch2 ch3 = do
  -- ch1 <- io (newChan)
  _ <- io (forkIO (timer (10 * onesec) SignalPingSeeds ch1))
  _ <- io (forkIO (timer (10 * onesec) SignalScanSwitchs ch1))
  _ <- io (forkIO (timer (30 * onesec) SignalTapTap ch1))

  h <- gets selfH
  _ <- io (forkIO (dolisten h ch1))

  -- Get the ball rolling immediately
  pingSeeds

  -- Process the async messages from the various sources above
  forever $ do
    s <- io (readChan ch1)
    timeNow <- io getClockTime
    -- io (putStrLn $ "got signal: " ++ (show s) ++ " at " ++ (show timeNow))
    -- io (putStrLn $ "got signal:at " ++ (show timeNow))
    case s of
      SignalPingSeeds      -> pingSeeds
      SignalScanSwitchs      -> scanlines timeNow
      SignalTapTap         -> taptap timeNow
      SignalMsgRx msg addr -> recvTelex msg addr
      -- External commands
      SignalGetMaster      -> do
        sw <- getMaster
        io (writeChan ch2 (ReplyGetMaster sw))
      SignalSendUserMsg vals -> do
        io (writeChan ch3 (ReplyUserString "foo"))

    -- io (putStrLn $ "done signal:at " ++ (show timeNow))

-- ---------------------------------------------------------------------

getMaster :: TeleHash Master
getMaster = do
  master <- get
  return master

-- ---------------------------------------------------------------------

rotateToNextSeed :: TeleHash String
rotateToNextSeed = do
  seeds <- gets selfSeeds
  s <- get
  case seeds of
    [] -> return ""
    _  -> do
      put (s { selfSeeds = ((tail seeds) ++ [head seeds]) })
      return (head seeds)

-- ---------------------------------------------------------------------

pingSeeds :: TeleHash ()
pingSeeds = do
  seeds <- gets selfSeeds
  connected <- gets selfConnected

  -- logT $ "pingSeeds:" ++ (show connected) ++ " " ++ (show seeds)

  -- TODO: rotate the seeds, so the we use a fresh one each time through
  case (not connected) && (seeds /= []) of
    True -> do
      nextSeed <- rotateToNextSeed
      pingSeed nextSeed
    False -> return ()

-- ---------------------------------------------------------------------

pingSeed :: String -> TeleHash ()
pingSeed seed =
  do
    -- logT ( "pingSeed " ++ (show seed))

    (_serveraddr,ip,port) <- io (resolveToSeedIPP seed)

    --logT ( "pingSeed serveraddr=" ++ (show serveraddr))

    let seedIPP = IPP (ip ++ ":" ++ port)
    -- console.log(["SEEDING[", seedIPP, "]"].join(""));
    logT ( "SEEDING[" ++ (show seedIPP))

    master <- get
    put master {selfSeedsIndex = Set.insert seedIPP (selfSeedsIndex master) }

    timeNow <- io getClockTime

    line <- getOrCreateSwitch seedIPP timeNow
    let bootTelex = mkTelex seedIPP
    -- // any end will do, might as well ask for their neighborhood
    let bootTelex' = bootTelex { teleSigEnd = Just $ (swiEnd line) }

    sendTelex bootTelex'

    return ()

-- ---------------------------------------------------------------------

getOrCreateSwitch :: IPP -> ClockTime -> TeleHash Switch
getOrCreateSwitch seedIPP timeNow = do
  lineMaybe <- getSwitchMaybeM (mkHash seedIPP)
  case lineMaybe of
    Just line -> return line
    Nothing -> do
      -- console.log(["\tNEWLINE[", endpoint, "]"].join(""));
      logT $ "\tNEWLINE[" ++ (show seedIPP) ++ "]"
      ringOutVal <- io (R.randomRIO (1,32768) )
      let line = mkSwitch seedIPP timeNow ringOutVal
      updateTelehashSwitch line
      return line


-- ---------------------------------------------------------------------

getSwitchMaybeM :: Hash -> TeleHash (Maybe Switch)
getSwitchMaybeM hashIpp = do
  switch <- get
  let network = (selfNetwork switch)
  return $ getSwitchMaybe network hashIpp

getSwitchMaybe :: Map.Map Hash Switch  -> Hash -> Maybe Switch
getSwitchMaybe network hashIpp  = lineMaybe
  where
    ismember = Map.member hashIpp network
    member = network Map.! hashIpp

    lineMaybe = case (ismember) of
      True -> Just member
      False -> Nothing

removeSwitchM :: Hash -> TeleHash ()
removeSwitchM hash = do
  switch <- get
  network <- gets selfNetwork

  -- put $ switch {selfNetwork = Map.delete hash network}
  -- TODO: what about neighbours?
  let
    network' = Map.fromList $
              map (\(h,line) -> (h,line {swiNeighbors = Set.delete hash (swiNeighbors line)})) $
              Map.toList $ Map.delete hash network

  put $ switch {selfNetwork = network'}


-- ---------------------------------------------------------------------

addNeighbour :: Hash -> Hash -> IPP -> TeleHash ()
addNeighbour hash end ipp = do
  switch <- get
  let network = selfNetwork switch
  case (getSwitchMaybe network hash) of
    Just line -> do
      updateTelehashSwitch (line { swiNeighbors = Set.insert end (swiNeighbors line) })
      -- console.log(["\t\tSEED ", ipp, " into ", self.network[hash].ipp].join(""));
      logT ( ("\t\tSEED " ++ (show ipp) ++ " into " ++ (show $ swiIpp line) ))
      return ()
    Nothing -> return ()

-- ---------------------------------------------------------------------


myNop :: TeleHash ()
myNop = do return ()

updateTelehashSwitch :: Switch -> TeleHash ()
updateTelehashSwitch line = do
  switch <- get

  let
    network = (selfNetwork switch)

    endpointHash = swiEnd line

    network' = Map.insert endpointHash line network

    switch' = switch {selfNetwork = network'}

  put switch'

-- ---------------------------------------------------------------------

safeGetHop :: Telex -> Int
safeGetHop rxTelex = hop
  where
    hop =
      case (teleHop rxTelex) of
        Nothing -> 0
        Just h -> h

-- ---------------------------------------------------------------------

hashToIpp :: Map.Map Hash Switch -> Hash -> IPP
hashToIpp network h =
  let
    Just line = getSwitchMaybe network h
  in
    swiIpp line

-- ---------------------------------------------------------------------

cTIMEOUT :: Integer
--cTIMEOUT = 90
cTIMEOUT = 70

-- TODO: What return type makes sense? The Bool will always be true.
isLineOk :: Switch -> Integer -> Telex -> Either String Bool
isLineOk line secsNow msg = result
  where
    -- // first, if it's been more than 10 seconds after a line opened,
    -- // be super strict, no more ringing allowed, _line absolutely required
    ringTimedOut =
      case (swiATline line) of
        Just (TOD secs _picos) -> case (secsNow - secs > 10) of
          True -> (teleLine msg) /= (swiLine line)
          False -> False
        Nothing -> False

    -- // second, process incoming _line
    msgLineOk = case (teleLine msg) of
      Just msgLineNum ->
        case (swiLine line) of
          Just lineNum -> lineNum == msgLineNum && (lineNum `mod` (swiRingout line) == 0)
          Nothing -> True -- only test if we have a value
      Nothing -> True

    lineOk = msgLineOk && not ringTimedOut

    result = case (lineOk) of
      True -> Right True
      False -> Left $ "msgLineOk=" ++ (show msgLineOk) ++ ",ringTimedOut=" ++ (show ringTimedOut)

-- ---------------------------------------------------------------------

isRingOk :: Switch -> Telex -> Bool
isRingOk line msg = ringOk
  where
    ringOk = case (teleRing msg) of
      Just msgRing -> msgRingOk && lineOk
        where
            -- Check that line.ringin matches if it exists
            -- Check the incoming ring > 0 and <= 32768
            msgRingOk = (msgRing > 0) && (msgRing <= 32768)
            lineOk = case (swiRingin line) of
                      Just ri -> (ri == msgRing)
                      Nothing -> True

      Nothing -> True

-- ---------------------------------------------------------------------

checkSwitch :: Switch -> Telex -> ClockTime -> Either String Switch
checkSwitch line msg timeNow@(TOD secsNow _picosecsNow) =
  let
    lineOk = isLineOk line secsNow msg

    line' = case lineOk of
              Right _ -> case (swiATline line) of
                Just _ -> line
                Nothing -> case (teleLine msg) of
                  Just msgLine ->
                    line {swiRingin = Just (msgLine `div` ((swiRingout line))),
                          swiLine   = teleLine msg,
                          swiATline = Just timeNow
                         }
                  Nothing -> line
              Left _ -> line

    ringOk = isRingOk line' msg

    line'' = case (teleRing msg) of
               Just msgRing ->
                 if (not ringOk)
                   then line'
                   else (if ((swiATline line) /= Nothing)
                      then line'
                      else line' {
                                 swiRingin = Just msgRing,
                                 swiLine   = Just (msgRing * (swiRingout line)),
                                 swiATline = Just timeNow
                                 })
               Nothing -> line'

    valid = case lineOk of
              Left err -> Left err
              Right _  -> if ringOk then (Right True) else (Left "ringOk=False")

    msgLength = fromJust (teleMsgLength msg)

    line''' = case valid of
                 Right _ -> line'' { swiBr   = (swiBr line'') + msgLength,
                                     swiBrin = teleBr msg }
                 Left _  ->  line''

    brOk = (swiBr line''') - (swiBrout line''') <= 12000
  in
   case valid of
     Left err -> Left err
     Right _ -> case brOk of
       True -> Right $ line''' { swiATseen = Just timeNow }
       False -> Left $ "lineOk=" ++ (show lineOk) ++ ",ringOk=" ++ (show ringOk) ++ ",brOk=" ++ (show brOk)

-- ---------------------------------------------------------------------
--
-- Listen for incoming messages and drop them in the FIFO
--
dolisten :: Maybe SocketHandle -> Chan Signal -> IO ()
dolisten Nothing _ = return ()
dolisten (Just h) channel = forever $ do
    (msg,rinfo) <- (SB.recvFrom (slSocket h) 1000)

    -- (putStrLn ("dolisten:rx msg=" ++ (BC.unpack msg)))
    (writeChan channel (SignalMsgRx (BC.unpack msg) rinfo))

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

      switch <- get
      put switch {selfCountTx = (selfCountTx switch) + 1 }

      addr <- io (addrFromHostPort (swiHost line) (swiPort line))
      --Just socketh <- gets selfH
      --io (sendDgram socketh msgJson addr)
      sender <- gets selfSender
      sender msgJson addr

      updateTelehashSwitch(line)

-- ---------------------------------------------------------------------

doNullSendDgram :: String -> SockAddr -> TeleHash ()
doNullSendDgram msgJson addr = do
  --logT ("doNullSendDgram[" ++ msgJson ++ "] to " ++ (show addr))
  logT ("doNullSendDgram" )

doSendDgram :: String -> SockAddr -> TeleHash ()
doSendDgram msgJson addr = do
  Just socketh <- gets selfH
  io (sendDgram socketh msgJson addr)


-- ---------------------------------------------------------------------

prepareTelex :: Telex -> ClockTime -> TeleHash (Maybe (Switch,String))
prepareTelex msg timeNow = do
  line <- getOrCreateSwitch (teleTo msg) timeNow

  -- check br and drop if too much
  --  if (line.bsent - line.brin > 10000) {
  --      console.log("\tMAX SEND DROP\n");
  --      return;
  --  }

  let brBad = (swiBsent line) - (swiBrin line) > 10000
  case brBad of
    True -> do
      logT ( "MAX SEND DROP ")
      return Nothing
    False -> do
      -- if a line is open use that, else send a ring
      let
        msg' = if (swiLine line == Nothing)
               then (msg {teleRing = Just (swiRingout line)})
               else (msg {teleLine = Just (fromJust (swiLine line))})

        -- update our bytes tracking and send current state
        -- telex._br = line.brout = line.br;
        msg'' = msg' { teleBr = swiBr line }

        msgJson = encodeTelex msg''

        line' = line {
            swiBrout = swiBr line
          , swiBsent = (swiBsent line) + (length msgJson)
          , swiATsent = Just timeNow
          }

      return (Just (line',msgJson))

-- ---------------------------------------------------------------------

sendDgram :: SocketHandle -> String -> SockAddr -> IO ()
sendDgram socketh msgJson addr =
  sendstr msgJson
    where
      -- Send until everything is done
      sendstr :: String -> IO ()
      sendstr [] = return ()
      sendstr omsg = do sent <- sendTo (slSocket socketh) omsg addr
                        sendstr (genericDrop sent omsg)

-- ---------------------------------------------------------------------

-- (telex._line ? "OPEN":"RINGING")
getSwitchStatus :: Telex -> [Char]
getSwitchStatus msg =
  case (teleLine msg) of
    Just _ -> "OPEN"
    Nothing -> "RINGING"

-- ---------------------------------------------------------------------

-- Dispatch incoming raw messages

recvTelex :: String -> SockAddr -> TeleHash ()
recvTelex msg rinfo = do
    -- logT ( ("recvTelex:" ++  (show (parseTelex msg))))

    switch' <- get
    put switch' { selfCountRx = (selfCountRx switch') + 1 }
    -- switch <- get
    -- seedsIndex <- gets selfSeedsIndex

    (Just hostIP,Just port) <- io (getNameInfo [NI_NUMERICHOST] True True rinfo)
    let
      remoteipp = IPP (hostIP ++ ":" ++ port)

    timeNow <- io getClockTime
    --console.log(["RECV from ", remoteipp, ": ", JSON.stringify(telex)].join(""));
    logT ("RECV from " ++ (show remoteipp) ++ ":"++ msg
                  ++ " at " ++ (show timeNow))
    let
      maybeRxTelex = parseTelex msg

    case maybeRxTelex of
      Just rxTelex -> handleRxTelex rxTelex remoteipp timeNow
      Nothing -> return ()

handleRxTelex :: Telex -> IPP -> ClockTime -> TeleHash ()
handleRxTelex rxTelex remoteipp timeNow = do
    isOnline <- checkOnline rxTelex remoteipp timeNow
    case isOnline of
      False -> return ()
      True -> do
        -- // if this is a switch we know, check a few things
        line <- getOrCreateSwitch remoteipp timeNow
        let lstat = checkSwitch line rxTelex timeNow
        case lstat of
          Left reason -> do
            logT ( "\tLINE FAIL[" ++ reason ++ ", " ++ (show (swiIpp line, swiEnd line)))
            myNop
          Right line' -> do
            -- // we're valid at this point, line or otherwise, track bytes
            let diff = (swiBsent line') - (teleBr rxTelex)
            -- console.log(["\tBR ", line.ipp, " [", line.br, " += ",br, "] DIFF ", (line.bsent - t._br)].join(""));
            logT ("\tBR " ++ (show $ swiIpp line') ++ " [" ++ (show $ swiBr line') ++ " += " ++ (show $ teleMsgLength rxTelex) ++ "] DIFF " ++
                  (show (diff, (swiBsent line') , (teleBr rxTelex))))
                   -- (show $ ((swiBsent line') , (teleBr rxTelex)) ))

            logT ( "\tLINE STATUS " ++ (getSwitchStatus rxTelex))
            updateTelehashSwitch line'
            processCommands rxTelex remoteipp line'
            processSignals  rxTelex remoteipp line'

        tapSignals (hasSignals rxTelex) rxTelex

        return ()

-- ---------------------------------------------------------------------

checkOnline :: Telex -> IPP -> ClockTime -> TeleHash Bool
checkOnline rxTelex remoteipp timeNow = do
  switch <- get
  seedsIndex <- gets selfSeedsIndex

  case (selfConnected switch) of
      False -> do
        -- TODO : test that _to field is set. Requires different setup for the JSON leg.
        --        Why would it not be set? Also test that the _to value is us.
        -- // must have come from a trusted seed and have our to IPP info
        case (Set.member remoteipp seedsIndex ) of
          True -> do
            -- TODO: check that the ring/line stuff matches
            online rxTelex timeNow
            return True
          False -> do
            logT ( "recvTelex:we're offline and don't like that")
            return False

      True -> do
        -- logT ( "recvTelex:already online")
        return True

-- ---------------------------------------------------------------------

tapSignals :: Bool -> Telex -> TeleHash ()
tapSignals False _       = do return ()
tapSignals True  rxTelex = do
  -- // if not last-hop, check for any active taps (todo: optimize the matching, this is just brute force)
  case (safeGetHop rxTelex < 4) of
    False -> return ()
    True -> do
      network <- gets selfNetwork
      let
        signals = getSignals rxTelex

      -- logT ( "\tTAP CHECK:signals=" ++ (show $ signals)) -- ++debug

      mapM_ (\line -> tapSwitch rxTelex signals line) $ Map.elems network
      return ()

-- ---------------------------------------------------------------------

tapSwitch :: Telex -> [(String, String)] -> Switch -> StateT Master IO ()
tapSwitch telex signals line = do
  mapM_ (\rule -> processRule rule) $ swiRules line
  where
    processRule rule = do
      logT ( "\tTAP CHECK IS " ++ (show $ swiIpp line) ++ "\t" ++ (show rule))
      isMatch <- foldM (\acc (k,v) -> matchIs acc (k,v)) True [(tapIs rule)]
      hasMatch <- foldM (\acc k -> matchHas acc k) True (tapHas rule)
      logT ( "\tTAP CHECK:( " ++ (show (isMatch,hasMatch))) -- ++debug
      forward (isMatch && hasMatch)
      return ()

    matchIs :: Bool -> (String,String) -> TeleHash Bool
    matchIs acc (isKey,isVal) = do
      let
        telexIsVal = find (\(k,v) -> k == isKey) signals
      logT ( "\t\tIS match: " ++ (show telexIsVal) ++ "=" ++ (show isVal)++ "?")
      return (acc && (telexIsVal == Just (isKey,quote isVal)))

    quote str = "\"" ++ str ++ "\""

    matchHas acc hasKey = do
      let
        telexHasVal = find (\(k,v) -> k == hasKey) signals
        hasKeyInTelex = telexHasVal /= Nothing

      -- console.log("HAS match: " + hasKey + " -> " + (hasKey in telex));
      logT ( "\t\tHAS match: " ++ hasKey ++ " -> " ++ (show hasKeyInTelex))
      return (acc && hasKeyInTelex)

    -- // forward this switch a copy
    forward False = do
      logT ( "\tCHECK MISS")
      return ()
    forward True  = do
      let
        swipp = swiIpp line
      Just selfipp <- gets selfSelfIpp
      -- logT ( "DEBUG:(swipp,selfipp)=" ++ (show (swipp,selfipp)))
      -- // it's us, it has to be our tap_js
      case (swipp == selfipp) of
        True -> do
          -- console.log(["STDOUT[", JSON.stringify(telex), "]"].join(""));
          logT ( "STDOUT[" ++ (show telex) ++ "]")
          return ()
        False -> do
          let
            -- make a new message with all the signals from the old one
            tx = (mkTelex swipp) {teleHop = Just ((safeGetHop telex) + 1) }
            signals = getSignals telex
            tx' = foldl' addSignal tx signals
          sendTelex tx'

    addSignal tel (sigName, _sigVal) =
      case sigName of
        "+end" -> tel {teleSigEnd = (teleSigEnd telex)}
        "+pop" -> tel {teleSigPop = (teleSigPop telex)}
        x -> undefined -- Should never happen, but catch extensions when they come
                       -- TODO: make this recover gracefully and log something, instead

-- ---------------------------------------------------------------------

processCommands :: Telex -> IPP -> Switch -> TeleHash ()
processCommands rxTelex remoteipp line = do
  mapM_ (\k -> processCommand k remoteipp rxTelex line) (getCommands rxTelex)
  return ()

getCommands :: Telex -> [String]
getCommands telex = filter isCommand $ Map.keys (teleRest telex)
  where
    isCommand k = (head k) == '.'

-- ---------------------------------------------------------------------

processCommand
  :: String -> IPP -> Telex -> Switch -> TeleHash ()
processCommand ".see" remoteipp telex line = do
  -- logT ( "processCommand .see")
  --switch <- get
  Just selfipp <- gets selfSelfIpp
  case (teleSee telex) of
    -- // loop through and establish lines to them (just being dumb for now and trying everyone)
    Just seeipps -> mapM_ (\ipp -> processSee line remoteipp ipp)
                    $ filter (\i -> i /= selfipp) -- // skip ourselves :)
                    $ seeipps
    Nothing      -> return ()

-- ---------------------------------------------------------------------
-- Handle the .tap TeleHash command.
processCommand ".tap" _remoteipp telex line = do
  -- logT ( "processCommand .tap:" ++ (show (teleTap telex)) )
  {-
  -- // handle a tap command, add/replace rules
  if (telex[".tap"] && isArray(telex[".tap"])) {
      line.rules = telex[".tap"];
  }
  -}
  case (teleTap telex) of
    Nothing -> return ()
    Just tap -> updateTelehashSwitch (line { swiRules = tap })
  return ()

-- ---------------------------------------------------------------------

processCommand cmd _remoteipp _telex _line = do
  logT ( "processCommand : ***ignoring*** " ++ (show cmd))
  return ()

-- ---------------------------------------------------------------------

-- We are processing a single .see entry here, which is not ourselves
processSee :: Switch -> IPP -> IPP -> TeleHash ()
processSee line remoteipp seeipp = do
  -- logT ( "processSee " ++ (show line) ++ "," ++ (show remoteipp) ++ "," ++ (show seeipp))
  -- logT ( "processSee " ++ (show remoteipp) ++ "," ++ (show seeipp))

  switch <- get
  Just selfipp  <- gets selfSelfIpp
  Just selfhash <- gets selfSelfHash

  seeVisible (seeipp == remoteipp && not (swiVisible line)) line selfipp remoteipp

  -- let
  --   lineSee = getSwitchMaybe (selfNetwork switch) (mkHash seeipp)

  case (getSwitchMaybe (selfNetwork switch) (mkHash seeipp)) of
    Just _lineSee -> return () -- // skip if we know them already
    Nothing -> do
        -- // XXX todo: if we're dialing we'd want to reach out to any of these closer to that $tap_end
        -- // also check to see if we want them in a bucket
        bw <- bucket_want selfhash seeipp
        case (bw) of
          True -> do
            -- // send direct (should open our outgoing to them)
            sendTelex ((mkTelex seeipp) { teleSigEnd = Just selfhash })

            -- // send pop signal back to the switch who .see'd us in case the new one is behind a nat
            sendTelex ((mkTelex remoteipp) { teleSigEnd = Just (mkHash seeipp)
                                           , teleSigPop = Just ("th:" ++ (unIPP selfipp))
                                           , teleHop    = Just 1})
            return ()

          False -> return ()
  return ()


-- ---------------------------------------------------------------------

seeVisible :: Bool -> Switch -> IPP -> IPP -> TeleHash ()
seeVisible False _line _selfipp _remoteipp = do return ()
seeVisible True  line  selfipp  remoteipp = do
  -- // they're making themselves visible now, awesome
  logT ( "\t\tVISIBLE " ++ (show remoteipp))
  let line' = line {swiVisible = True}

  Right newNeighbourList <- near_to (swiEnd line') selfipp
  updateTelehashSwitch (line' { swiNeighbors = Set.union (Set.fromList newNeighbourList) (swiNeighbors line') })

  _ <- near_to (swiEnd line) remoteipp -- // injects this switch as hints into it's neighbors, fully seeded now
  return ()

-- ---------------------------------------------------------------------
{-
 * generate a .see for an +end, using a switch as a hint
 * -}

near_to :: Hash -> IPP -> TeleHash (Either String [Hash])
near_to endHash ipp = do
  switch <- get

  let
    -- endHash = mkHash end
    network = (selfNetwork switch)

  case (getSwitchMaybe network (mkHash ipp)) of
    Nothing -> return $ Left "no line for ipp"
    Just line -> do
      let
        -- of the existing and visible cached neighbors, sort by distance to this end
        see = sortBy hashDistanceOrder $ Set.toList $ Set.filter isSwitchVisible (swiNeighbors line)

        isSwitchVisible x = case (getSwitchMaybe network x) of
          Just l -> swiVisible l
          Nothing -> False

        hashDistanceOrder a b
          | dist < 0 = LT
          | dist > 0 = GT
          | otherwise = EQ
          where
            dist = (distanceTo endHash a) - (distanceTo endHash b)

      near_to_see line endHash ipp see

near_to_see :: Switch -> Hash -> IPP -> [Hash] -> TeleHash (Either String [Hash])
near_to_see _line _endHash _ipp []  = do return $ Left "empty see list"
near_to_see line  endHash  ipp  see = do
      let
        firstSee     = head see
        firstSeeHash = firstSee
        lineNeighborKeys = (swiNeighbors line)
        swiEndHash  = swiEnd line
      logT ( ("\tNEARTO " ++ (show endHash) ++ "\t" ++ (show ipp) ++ "\t" ++ (show $ Set.size lineNeighborKeys)
          ++ ">" ++ (show $ length see)
          ++ "\t" ++ (show $ distanceTo firstSeeHash endHash) ++ "=" ++ (show $ distanceTo swiEndHash endHash)))

      -- it's either us or we're the same distance away so return these results
      case (firstSee == (swiEnd line)
            || ((distanceTo firstSeeHash endHash) == (distanceTo swiEndHash endHash))) of
        True ->
          -- this +end == this line then replace the neighbors cache with this result
          -- and each in the result walk and insert self into their neighbors
          case (swiEndHash == endHash) of
            True -> do
              logT ( ("\tNEIGH for " ++ (show endHash) ++ " was " ++ (show lineNeighborKeys) ++ (show $ length see)))
              let neigh = Set.fromList $ take 5 see
              updateTelehashSwitch (line {swiNeighbors = neigh})
              logT ( ("\tNEIGH for " ++ (show endHash) ++ " is " ++ (show neigh) ++ (show $ length see)))

              mapM_ (\hash -> addNeighbour hash endHash ipp) $ Set.toList neigh
              logT ( ("\t\tSEE distance=" ++ (show $ distanceTo endHash firstSeeHash) ++ " count=" ++ (show $ length see) ))
              return $ Right see
            False -> do
              logT ( ("\t\tSEE distance=" ++ (show $ distanceTo endHash firstSeeHash) ++ " count=" ++ (show $ length see) ))
              return $ Right see -- TODO:plug in bit below
        False -> do
          -- whomever is closer, if any, tail recurse endseeswitch them
          Just lineFirstSee <- getSwitchMaybeM firstSee
          near_to endHash (swiIpp lineFirstSee)


-- ---------------------------------------------------------------------

-- // see if we want to try this switch or not, and maybe prune the bucket
bucket_want :: Hash -> IPP -> TeleHash Bool
bucket_want selfhash ipp = do
  {-
    var self = this;
    var pos = new Hash(ipp).distanceTo(self.selfhash);
    console.log(["\tBUCKET WANT[", pos, " ", ipp, "]"].join(""));
    if (pos < 0 || pos > self.NBUCKETS) {
        return undefined; // do not want
    }
    return 1; // for now we're always taking everyone, in future need to be more selective when the bucket is "full"!
  -}
  let
    -- // for now we're always taking everyone, in future need to be more selective when the bucket is "full"!
    pos = distanceTo (mkHash ipp) selfhash
  -- console.log(["\tBUCKET WANT[", pos, " ", ipp, "]"].join(""));
  logT ( "\tBUCKET WANT[" ++ (show pos) ++ " " ++ (show ipp) ++ "]")

  return ((pos >= 0) && (pos <= nBuckets))


-- ---------------------------------------------------------------------
{-
/**
 * XOR distance between two sha1 hex hashes, 159 is furthest bit, 0 is closest bit, -1 is same hash
 */
Hash.prototype.distanceTo = function(h) {
    if (isString(h)) { h = new Hash(h); }

    var nibbles = this.nibbles();
    var hNibbles = h.nibbles()

    var sbtab = [-1,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3];
    var ret = 156;
    for (var i = 0; i < nibbles.length; i++) {
        var diff = nibbles[i] ^ hNibbles[i];
        if (diff) {
            return ret + sbtab[diff];
        }
        ret -= 4;
    }
    return -1; // samehash ?!
}
-}

-- TODO: consider memoising this result, will be used a LOT
--distanceTo :: Num a => Hash -> Hash -> a
distanceTo :: Hash -> Hash -> Int
distanceTo (Hash this) (Hash h) = go 156 (reverse diffs)
  where
    go acc [] = acc
    go _acc (-1:[]) = -1
    go acc (-1:xs) = go (acc - 4) xs
    go acc (x:_xs) = acc + x

    diffs = map (\(a,b) -> sbtab !! (xor (digitToInt a) (digitToInt b))) $ zip this h
    sbtab = [-1,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3]

-- ---------------------------------------------------------------------

processSignals :: Telex -> IPP -> Switch -> TeleHash ()
processSignals rxTelex remoteipp line = do
  mapM_ (\(k,_v) -> processSignal k remoteipp rxTelex line) (getSignals rxTelex)
  return ()

getSignals :: Telex -> [(String, String)]
getSignals telex = filter isSignal $ Map.assocs (teleRest telex)
  where
    isSignal (k,_v) = (head k) == '+'

hasSignals :: Telex -> Bool
hasSignals telex = (getSignals telex) /= []

-- ---------------------------------------------------------------------

processSignal :: String -> IPP -> Telex -> Switch -> TeleHash ()
processSignal "+end" remoteipp telex line = do
  logT ( "processSignal :  +end")

  -- switch <- get
  network        <- gets selfNetwork
  Just selfipp  <- gets selfSelfIpp
  Just selfhash <- gets selfSelfHash

  let
    hop = safeGetHop telex
    Just end = teleSigEnd telex
  case (hop) of
    0 -> do
      let
        -- // start from a visible switch (should use cached result someday)
        vis = if (swiVisible line) then (remoteipp) else (selfipp)

      h <- near_to end vis -- // get closest hashes (of other switches)
      logT ( "+end hashes: " ++ (show h))
      let hashes = case h of
            Right hashes -> hashes
            Left msg -> []

      let ipps = take 5 $ map (\h -> hashToIpp network h) hashes
      logT ( "+end ipps: " ++ (show ipps))

        {-
        -- // TODO: this is where dampening should happen to not advertise switches that might be too busy
        if (!line.visibled) {
            ipps[self.selfipp] = line.visibled = 1; // mark ourselves visible at least once
        }
        -}
      let
        ipps' = if (swiVisibled line) then (ipps) else (selfipp:ipps)
      updateTelehashSwitch (line { swiVisibled = True })

        {-
        var ippKeys = keys(ipps);
        if (ippKeys.length) {
            var telexOut = new Telex(remoteipp);
            var seeipps = ippKeys.filter(function(ipp){ return ipp.length > 1 });
            telexOut[".see"] = seeipps;
            self.send(telexOut);
        }
       -}
      case (ipps') of
        [] -> do return ()
        seeipps -> do
          sendTelex ((mkTelex remoteipp) { teleSee = Just (nub seeipps) })
          return ()
      return ()
    _ -> do return ()


  -- // this is our .tap, requests to +pop for NATs
  case (end == selfhash && (teleSigPop telex) /= Nothing) of
    True -> do
      let
        Just str = teleSigPop telex
        [_,ip,port] = split ":" str -- TODO: more robust parsing here

      logT ( "POP? " ++ (show $ teleSigPop telex))
      logT ( "POP to " ++ ip ++ ":" ++ port)
      -- // should we verify that this came from a switch we actually have a tap on?
      sendTelex (mkTelex (IPP (ip ++ ":" ++ port)))
      {-
      if (end == self.selfhash && telex["+pop"]) {
          console.log("POP? " + telex["+pop"]);
          var tapMatch = telex["+pop"].match(/th\:([\d\.]+)\:(\d+)/);
          if (tapMatch) {
              // should we verify that this came from a switch we actually have a tap on?
              var ip = tapMatch[1];
              var port = tapMatch[2];
              console.log(["POP to ", ip, ":", port].join(""));
              self.send(new Telex([ip, port].join(":")));
          }
      }
      -}
      return ()
    False -> do return ()
  return ()

-- ---------------------------------------------------------------------

processSignal cmd _remoteipp _telex _line = do
  logT ( "processSignal : ***ignoring*** " ++ (show cmd))
  return ()

-- ---------------------------------------------------------------------

online :: Telex -> ClockTime -> TeleHash ()
online rxTelex timeNow = do

  logT "\tONLINE"

  let
    selfIpp  = (teleTo rxTelex)
    selfhash = mkHash $ teleTo rxTelex
  switch <- get
  put $ switch {selfConnected = True
              , selfSelfIpp   = Just selfIpp
              , selfSelfHash  = Just selfhash
              , selfCountOnline = (selfCountOnline switch) + 1
              }

  logT ( ("\tSELF[" ++ (show selfIpp) ++ " = " ++ (show selfhash) ++ "]"))

  line <- getOrCreateSwitch selfIpp timeNow

  -- ++AZ++
  switch' <- get
  -- let taps' = (Tap ("+end",unHash selfhash) ["+news"]):(selfTaps switch)
  let taps' = (Tap ("+end",unHash selfhash) ["+news"]):[]
  put $ switch' { selfTaps = taps'}
  -- ++AZ++

  taps <- gets selfTaps

  updateTelehashSwitch (line {swiVisible = True, swiRules = taps })

  -- // trigger immediate tapping to move things along
  taptap timeNow

  return ()


-- ---------------------------------------------------------------------

offline :: ClockTime -> TeleHash ()
offline now = do
  logT ( "OFFLINE at " ++ (show now))
  switch <- get
  put $ switch { selfSelfIpp = Nothing,
                selfSelfHash = Nothing,
                selfConnected = False,
                selfNetwork = Map.empty
              }

-- ---------------------------------------------------------------------

taptap :: ClockTime -> TeleHash ()
taptap timeNow@(TOD secs _picos) = do
  -- logT ( "taptap: " ++ (show timeNow))
  {-
    var self = this;
    if(!self.connected) return;
  -}
  connected <- gets selfConnected
  case (connected) of
    False -> return ()
    True -> do
      taps <- gets selfTaps
      mapM_ doTap taps
      return ()

  where
    doTap tap = do
      case (tapIs tap) of
        ("+end",tapEnd) -> do
          -- We have tested for online status, these will be set
          Just selfipp  <- gets selfSelfIpp
          Just selfhash <- gets selfSelfHash
          Right candidateHashes <- near_to (Hash tapEnd) selfipp
          let hashes = take 3 $ filter (\hash -> hash /= selfhash) candidateHashes
          mapM_ (\hash -> doTapSwitch tap tapEnd hash) hashes
          return ()
        _          -> return ()

    doTapSwitch tap tapEnd hash = do
      Just line <- getSwitchMaybeM hash
      let (TOD tapLastSecs _) = fromMaybe (TOD 0 0) (swiATlastTap line)
      case (tapLastSecs + 50 > secs) of -- Assuming wall clock secs > 50
        True -> return ()
        False -> do
          updateTelehashSwitch (line { swiATlastTap = Just timeNow })
          let telexOut = (mkTelex (swiIpp line)) {teleTap = Just [tap] }
          -- console.log(["\tTAPTAP to ", line.ipp, " end ", tapEnd, " tap ", JSON.stringify(tap)].join(""));
          logT (  "\tTAPTAP to " ++ (show $ swiIpp line) ++ " end " ++ (show tapEnd) ++ " tap " ++ (show tap))
          sendTelex telexOut
          return ()
      return ()

-- ---------------------------------------------------------------------
{-**
 * Update status of all lines, removing stale ones.
 *-}
scanlines :: ClockTime -> TeleHash ()
scanlines now@(TOD _secs _picos) = do
  connected <- gets selfConnected
  case (connected) of
    False -> return ()
    True -> do
      network <- gets selfNetwork
      let switches = Map.keys network
      logT ( "SCAN\t" ++ (show $ length switches))
      valid <- foldM (\acc hash -> fscanline acc hash) False switches

      seedsIndex <- gets selfSeedsIndex
      Just selfipp  <- gets selfSelfIpp
      case (valid == False && Set.notMember selfipp seedsIndex) of
        True -> do
          offline now
          return ()
        False -> return ()
      return ()

  where
    fscanline :: Bool -> Hash -> TeleHash Bool
    fscanline acc hash = do
      res  <- scanline hash
      return (acc || res) -- Any one must be valid.

    -- Return whether line is still Ok or not.
    scanline :: Hash -> TeleHash Bool
    scanline hash = do
      Just line     <- getSwitchMaybeM hash
      Just selfHash <- gets selfSelfHash
      Just selfipp  <- gets selfSelfIpp
      case (selfHash == hash) of
        True -> return False -- // skip our own endpoint and what is this (continue)
        False -> do
          case (swiEnd line /= hash) of
            True -> return False -- Pretty sure this can never happen // empty/dead line (continue)
            False -> do
              timedOut <- isTimedOut now line
              case (timedOut) of
                True -> do
                  -- // remove line if they never responded or haven't in a while
                  -- console.log(["\tPURGE[", hash, " ", line.ipp, "] last seen ", now - line.seenat, "s ago"].join(""));
                  logT ( "\tPURGE[" ++ (show hash) ++ " " ++ (show $ swiIpp line) ++ "] last seen " ++ (show $ swiATseen line))
                  removeSwitchM hash
                  return False
                False -> do
                  -- We have a valid line, return true from here on out
                  connected <- gets selfConnected
                  case connected of
                    True -> do
                      -- // +end ourselves to see if they know anyone closer as a ping
                      let
                        telexOut = (mkTelex $ swiIpp line) { teleSigEnd = Just selfHash }
                      -- // also .see ourselves if we haven't yet, default for now is to participate in the DHT
                      let
                        telexOut' = if (swiVisibled line)
                                      then (telexOut)
                                      else (telexOut { teleSee = Just [selfipp] })
                      -- // also .tap our hash for +pop requests for NATs
                      let
                        telexOut'' = telexOut' {teleTap = Just [Tap {tapIs  = ("+end", unHash selfHash),
                                                                   tapHas = ["+pop"] }]
                                             }
                      sendTelex telexOut''

                      return True
                    False ->
                      return True

    isTimedOut (TOD secs _picos) line = do
      case (swiATseen line) of
        Nothing -> do
          let (TOD initSecs _) = (swiATinit line)
          return (secs - initSecs > cTIMEOUT)
        Just (TOD seenatSecs _) ->
          return (secs - seenatSecs > cTIMEOUT)

-- ---------------------------------------------------------------------
-- Convenience.
--
io :: IO a -> TeleHash a
io = liftIO

-- ---------------------------------------------------------------------
-- Testing, see http://hackage.haskell.org/package/test-framework-th

--main = $(defaultMainGenerator)


-- EOF