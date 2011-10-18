{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- # LANGUAGE TypeOperators #-}
{- # LANGUAGE TemplateHaskell #-}
{- # LANGUAGE NoMonoPatBinds #-}

-- Above three settings are for the Json stuff using Data.Iso


module TeleHash.Controller 
       (
       runSwitch
       , startSwitchThread
       -- For testing
       , recvTelex  
       , parseTelex
       , mkTelex  
       , Telex(..)
       , Hash(..)  
       , Tap(..)  
       , Line(..)
       , IPP(..)  
       , Switch(..)         
       , SocketHandle(..)
       , TeleHash()
       , encodeTelex  
       , isLineOk
       , isRingOk  
       , mkLine  
       , checkLine  
       , getCommands  
       , getSignals  
       , mkHash  
       , distanceTo
       , near_to
       , seeVisible
       , getLineMaybe
       , resolveToSeedIPP
       , addrFromHostPort
       , getOrCreateLine  
       , online  
       , prepareTelex  
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
--import Debug.Trace
import Network.BSD
import Network.Socket
import Text.JSON
import Text.JSON.Generic
import Text.JSON.Types
import Prelude hiding (id, (.), head, either, catch)
import System.IO
import System.Log.Handler.Simple  
import System.Log.Logger
import System.Time
  --import TeleHash.Json 


--import Text.Printf
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Network.Socket.ByteString as SB
--import qualified System.Random as R

{-

The approach taken here is a blend of the example in
Control.Concurrent.Actor, and the IRC bot example from
http://www.haskell.org/haskellwiki/Roll_your_own_IRC_bot

-}

--
-- The 'TeleHash' monad, a wrapper over IO, carrying the switch's immutable state.
--
type TeleHash = StateT Switch IO
--type TeleHash = ErrorT String (StateT Switch IO)

newtype Hash = Hash String
             deriving (Data,Eq,Show,Typeable,Ord)
unHash :: Hash -> String
unHash (Hash str) = str

newtype IPP = IPP String
             deriving (Data,Eq,Show,Typeable,Ord)
unIPP :: IPP -> String
unIPP (IPP str) = str

data Switch = Switch { swH :: Maybe SocketHandle 
                     , swSeeds :: [String] -- IPP?
                     , swSeedsIndex :: Set.Set IPP
                     , swConnected :: Bool
                     , swMaster :: Map.Map Hash Line  
                     , swSelfIpp :: Maybe IPP
                     , swSelfHash :: Maybe Hash
                     , swTaps :: [Tap]  
                     } deriving (Eq,Show)

nBuckets :: Int
nBuckets = 160

data SocketHandle = 
    SocketHandle {slSocket :: Socket
                 , slAddress :: SockAddr
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

-- Note: endPointStr should always be a IP:PORT
mkLine :: IPP -> ClockTime -> Line
mkLine endPoint@(IPP endPointStr) timeNow =
  let
    [hostIP,port] = split ":" endPointStr
    endPointHash = mkHash endPoint
    (TOD _secs picosecs) = timeNow
    ringOut = fromIntegral (1 + (picosecs `mod` 32768))  -- TODO: rand 1..32768
  in  
   Line {
       lineIpp       = endPoint,
       lineEnd       = endPointHash,
       lineHost      = hostIP,
       linePort      = port,
       lineRingout   = ringOut,
       lineRingin    = Nothing,
       lineLine      = Nothing,
       lineInit      = timeNow,
       lineSeenat    = Nothing,
       lineSentat    = Nothing,
       lineLineat    = Nothing,
       lineTapLast   = Nothing,
       lineBr        = 0,
       lineBrout     = 0,
       lineBrin      = 0,
       lineBsent     = 0,
       lineNeighbors = Set.fromList [endPointHash],
       lineVisible   = False,
       lineVisibled  = False,
       lineRules     = []                       
       }

-- ---------------------------------------------------------------------

-- JSON stuff for a Telex

data Tap = Tap { tapIs :: (String,String), 
                 tapHas :: [String] } 
         deriving (Data, Typeable, -- For Text.JSON
                   Eq, Show)

data Telex = Telex 
             { teleRing   :: Maybe Int
             , teleSee    :: Maybe [IPP]
             , teleBr     :: Int
             , teleTo     :: IPP
             , teleLine   :: Maybe Int
             , teleHop    :: Maybe Int
             , teleSigEnd :: Maybe Hash
             , teleSigPop :: Maybe String
             , teleTap    :: Maybe [Tap]  
             , teleRest   :: Map.Map String String
             , teleMsgLength :: Maybe Int -- length of received Telex  
             } deriving (Data, Typeable, -- For Text.JSON
                                 Eq, Show)

parseTelex :: String -> Telex
parseTelex s = 
    let 
      decoded = (decode s :: Result JSValue)
      Ok (JSObject cc) = decoded
      
      Just to   = getStringMaybe      cc "_to"
      maybeRing = getIntMaybe         cc "_ring"
      maybeSee  = getStringArrayMaybe cc ".see"
      Just br   = getIntMaybe         cc "_br"
      maybeLine = getIntMaybe         cc "_line"
      maybeHop  = getIntMaybe         cc "_hop"
      maybeEnd  = getHashMaybe        cc "+end"
      maybeTap  = getTapMaybe         cc ".tap"
      maybePop  = getStringMaybe      cc "+pop"
      msgLength = length s
      
      maybeSee' = case maybeSee of
        Nothing -> Nothing
        Just see -> Just $ map (\ss -> (IPP ss)) see
    in 
     -- mkTelex to
     
     (mkTelex (IPP to)) {teleRing = maybeRing, 
                   teleSee = maybeSee', 
                   teleBr = br, 
                   teleTo = (IPP to), teleLine = maybeLine, teleHop = maybeHop,
                   teleSigEnd = maybeEnd, teleTap = maybeTap, 
                   teleSigPop = maybePop,
                   teleRest = Map.fromList $ map (\(name,val) -> (name, encode val)) (fromJSObject cc), -- horrible, but will do for now
                   teleMsgLength = Just msgLength }
     
-- ---------------------------------------------------------------------
-- Pretty sure these two exist in JSON somewhere, do not know how to find them

getStringMaybe :: JSObject JSValue -> String -> Maybe String
getStringMaybe cc field =
  case (get_field cc field) of
      Just (JSString jsStrVal)  -> Just (fromJSString jsStrVal)
      _                         -> Nothing  
    
getHashMaybe :: JSObject JSValue -> String -> Maybe Hash
getHashMaybe cc field = 
  case (get_field cc field) of
      Just (JSString jsStrVal)  -> Just (Hash $ fromJSString jsStrVal)
      _                         -> Nothing  

getStringArrayMaybe :: JSObject JSValue -> String -> Maybe [String]
getStringArrayMaybe cc field =
  case (get_field cc field) of
      Just (JSArray jsStrArrVal)-> Just (map getStr jsStrArrVal)
      _                         -> Nothing  
  where
    getStr (JSString jsStrVal) = fromJSString jsStrVal
    getStr x                   = "getStringArrayMaybe:oops:getstr :" ++ (show x)
      
getIntMaybe :: JSObject JSValue -> String -> Maybe Int
getIntMaybe cc field =
  case (get_field cc field) of
      Just (JSRational _ jsVal)  -> Just $ round (jsVal)
      Just (JSString jsStrVal)  -> Just $ read (fromJSString jsStrVal)
      _                         -> Nothing  

{-
(".tap",JSArray 
  [JSObject (JSONObject 
     {fromJSObject = [
        ("is",JSObject (JSONObject 
           {fromJSObject = [("+end",JSString (JSONString {fromJSString = "9fa23aa9f9ac828ced5a151fedd24af0d9fa8495"}))]})),
        ("has",JSArray [JSString (JSONString {fromJSString = "+pop"})])
        ]
     })])
-}
-- ".tap":[{"is":{"+end":"9fa23aa9f9ac828ced5a151fedd24af0d9fa8495"},"has":["+pop"]}]      
getTapMaybe :: JSObject JSValue -> String -> Maybe [Tap]
getTapMaybe cc field =
  case (get_field cc field) of
      Just (JSArray jsArrVal)-> Just (map getTap jsArrVal)
      _                         -> Nothing  
  where
    getTap (JSObject jsobj) = foldl' dTap (Tap ("","") []) $ fromJSObject jsobj

    dTap tap ("is",  JSObject o) = tap {tapIs = (k, fromJSString str)}
      where
        (k,JSString str) = head $ fromJSObject o
        
    --dTap tap ("has", o) = tap {tapHas= [show o]}
    dTap tap ("has", JSArray arr) = tap {tapHas= map (\(JSString s) -> fromJSString s) arr}
    
-- ---------------------------------------------------------------------
    
test_parseTelex :: Telex
test_parseTelex = parseTelex _inp

_inp :: [Char]
_inp = ("{\"_ring\":17904," ++
       "\".see\":[ \"208.68.163.247:42424\", \"208.68.160.25:55137\"]," ++ 
       "\"_br\":52," ++ 
       "\"_to\":\"173.19.99.143:63681\" }")

_inp2 :: [Char]
_inp2 = "{\"_to\":\"208.68.163.247:42424\",\"+end\":\"9fa23aa9f9ac828ced5a151fedd24af0d9fa8495\",\".see\":[\"196.215.128.240:51602\"],\".tap\":[{\"is\":{\"+end\":\"9fa23aa9f9ac828ced5a151fedd24af0d9fa8495\"},\"has\":[\"+pop\"]}],\"_line\":35486388,\"_br\":174}"

_inp3 :: [Char]
_inp3 = "{\"+end\":\"9fa23aa9f9ac828ced5a151fedd24af0d9fa8495\",\"_to\":\"208.68.163.247:42424\",\".see\":[\"196.215.128.240:51602\"],\".tap\":[{\"is\":{\"+end\":\"9fa23aa9f9ac828ced5a151fedd24af0d9fa8495\"},\"has\":[\"+pop\"]}],\"_line\":35486388,\"_br\":174}"

_tx1 :: Telex
_tx1 = parseTelex _inp
_tx2 :: Telex
_tx2 = parseTelex _inp2
_tx3 :: Telex
_tx3 = parseTelex _inp3

-- ---------------------------------------------------------------------

encodeTelex :: Telex -> String
encodeTelex t = 
  let
    to   = [("_to",   JSString $ toJSString $ unIPP $ teleTo t)]
    
    ring = case (teleRing t) of
      Just r -> [("_ring", JSString $ toJSString $ show r)]
      Nothing -> []
      
    see = case (teleSee t) of
      Just r -> [(".see", showJSON $ JSArray (map (\s -> JSString (toJSString (unIPP s))) r))]
      Nothing -> []
    
    -- br = [("_br", JSString $ toJSString $ show (teleBr t))]
    br = [("_br", JSRational False (fromIntegral (teleBr t)))]

    line = case (teleLine t) of
      Just r -> [("_line", JSString $ toJSString $ show r)]
      Nothing -> []

    hop = case (teleHop t) of
      -- Just r -> [("_hop", JSString $ toJSString $ show r)]
      Just r -> [("_hop", JSRational False (fromIntegral r))]
      Nothing -> []
      
    end = case (teleSigEnd t) of
      Just (Hash r) -> [("+end", JSString $ toJSString r)]
      Nothing -> []

    pop = case (teleSigPop t) of
      Just r -> [("+pop", JSString $ toJSString r)]
      Nothing -> []

    tap = case (teleTap t) of
      Just r -> [(".tap", JSArray $ map toJSTap r)]
      Nothing -> []
      
    toJSTap atap = JSObject $ toJSObject [
      ("is", JSObject $ toJSObject [
          (k, JSString $ toJSString v),
          ("has", showJSON $ JSArray (map (\s -> JSString (toJSString s)) (tapHas atap)))
          ])]
      where
      (k,v) = tapIs atap

      
  in
   -- encode $ toJSObject (to++ring++see++br++line++hop++end++tap++pop)
   encode $    toJSObject (to++end++ring++see++tap++line++br++hop++pop)
   
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

main :: IO ((), Switch)
main = do
  s <- streamHandler stdout DEBUG
  -- updateGlobalLogger rootLoggerName (addHandler s) -- setHandlers [s]
  updateGlobalLogger rootLoggerName (setHandlers [s])
  runSwitch

-- ---------------------------------------------------------------------

data Signal = SignalPingSeeds | SignalScanLines | SignalTapTap | SignalMsgRx String SockAddr
              deriving (Typeable, Show, Eq)

onesec :: Int
onesec = 1000000       
                       
timer :: Int -> a -> Chan a -> IO ()
timer timeoutVal signalValue channel  = forever $ 
  threadDelay timeoutVal >> writeChan channel signalValue 
  
-- ---------------------------------------------------------------------
-- Logging

logT :: String -> TeleHash ()
logT str = io (warningM "Controller" str)

-- ---------------------------------------------------------------------
--
-- Set up actions to run on start and end, and run the main loop
--
runSwitch :: IO ((),Switch)
runSwitch = bracket initialize disconnect loop
  where
    disconnect (_,ss) = sClose (slSocket (fromJust $ swH ss))
    
    loop (ch,st) = catch (runStateT (run ch) st) (exc)

    exc :: SomeException -> IO ((),Switch)
    exc _e = return ((),undefined)

-- ---------------------------------------------------------------------
--
-- Set up actions to run on start and end, and run the main loop in
-- its own thread
--
    
startSwitchThread :: IO (Chan Signal,ThreadId)
startSwitchThread = do
  (ch,st) <- initialize 
  -- thread <- forkIO (io (runStateT run st))
  thread <- forkIO (doit ch st)
  return (ch,thread)
  
  where
    doit :: Chan Signal -> Switch -> IO ()
    doit ch st = do
      runStateT (run ch) st
      return ()

    
-- ---------------------------------------------------------------------
-- Hardcoded params for now    
initialSeed :: String
initialSeed = "telehash.org:42424"

initialize :: IO (Chan a,Switch)
initialize = do 
  -- Look up the hostname and port.  Either raises an exception
  -- or returns a nonempty list.  First element in that list
  -- is supposed to be the best option.
  (serveraddr,ip,port) <- resolveToSeedIPP initialSeed
  let seedIPP = IPP (ip ++ ":" ++ port)

  -- Establish a socket for communication
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

  -- We want to listen on all interfaces (0.0.0.0)
  bindAddr <- inet_addr "0.0.0.0"
  bindSocket sock (SockAddrInet 0 bindAddr)
    
  socketName <- getSocketName sock
  warningM "Controller" ("server listening " ++ (show socketName))
         
  ch1 <- newChan

  -- Save off the socket, and server address in a handle
  return $ (ch1, (Switch (Just (SocketHandle sock (addrAddress serveraddr))) [initialSeed] 
                  (Set.fromList [seedIPP])
                  False Map.empty Nothing Nothing []))

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

--
-- We're in the Switch monad now, so we've connected successfully
-- Start processing commands
--
run :: Chan Signal -> TeleHash ()
run ch1 = do
  -- ch1 <- io (newChan)
  _ <- io (forkIO (timer (10 * onesec) SignalPingSeeds ch1))
  _ <- io (forkIO (timer (10 * onesec) SignalScanLines ch1))
  _ <- io (forkIO (timer (30 * onesec) SignalTapTap ch1)) 

  h <- gets swH 
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
      SignalScanLines      -> scanlines timeNow
      SignalTapTap         -> taptap timeNow
      SignalMsgRx msg addr -> recvTelex msg addr
    -- io (putStrLn $ "done signal:at " ++ (show timeNow))
      

-- ---------------------------------------------------------------------
  
pingSeeds :: TeleHash ()
pingSeeds = do
  seeds <- gets swSeeds
  connected <- gets swConnected
  
  -- logT $ "pingSeeds:" ++ (show connected) ++ " " ++ (show seeds)
  
  -- TODO: rotate the seeds, so the we use a fresh one each time through
  case (not connected) && (seeds /= []) of
    True -> pingSeed $ head seeds
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

    switch <- get
    put switch {swSeedsIndex = Set.insert seedIPP (swSeedsIndex switch) }
    
    timeNow <- io getClockTime
    
    line <- getOrCreateLine seedIPP timeNow
    let bootTelex = mkTelex seedIPP
    -- // any end will do, might as well ask for their neighborhood
    let bootTelex' = bootTelex { teleSigEnd = Just $ (lineEnd line) }
  
    sendTelex bootTelex'
    
    return ()
   
-- ---------------------------------------------------------------------

getOrCreateLine :: IPP -> ClockTime -> TeleHash Line
getOrCreateLine seedIPP timeNow = do
  switch <- get
  
  let 
    master = (swMaster switch)
    
    endpointHash = mkHash seedIPP
    
    ismember = Map.member endpointHash master
    member = master Map.! endpointHash
    hashOk = (lineIpp member) == seedIPP
    (line, newLineCreated) = if (ismember && hashOk) then (member, False) else (mkLine seedIPP timeNow, True)
    
    line' = line {lineNeighbors = Set.insert endpointHash (lineNeighbors line)}

    master' = Map.insert endpointHash line' master
    
    switch' = switch {swMaster = master'}
    
  put switch'
  
  case (newLineCreated) of
    True ->
      -- console.log(["\tNEWLINE[", endpoint, "]"].join(""));
      logT $ "\tNEWLINE[" ++ (show seedIPP) ++ "]"
    False -> myNop
  
  return line'
  
-- ---------------------------------------------------------------------
  
getLineMaybeM :: Hash -> TeleHash (Maybe Line)
getLineMaybeM hashIpp = do
  switch <- get
  let master = (swMaster switch)
  return $ getLineMaybe master hashIpp
  
getLineMaybe :: Map.Map Hash Line  -> Hash -> Maybe Line
getLineMaybe master hashIpp  = lineMaybe
  where
    ismember = Map.member hashIpp master
    member = master Map.! hashIpp
  
    lineMaybe = case (ismember) of
      True -> Just member
      False -> Nothing
  
removeLineM :: Hash -> TeleHash ()    
removeLineM hash = do
  switch <- get
  master <- gets swMaster
  
  put $ switch {swMaster = Map.delete hash master}
  -- TODO: what about neighbours?
  
-- ---------------------------------------------------------------------

addNeighbour :: Hash -> Hash -> IPP -> TeleHash ()
addNeighbour hash end ipp = do
  switch <- get
  let master = swMaster switch
  case (getLineMaybe master hash) of
    Just line -> do
      updateTelehashLine (line { lineNeighbors = Set.insert end (lineNeighbors line) })
      -- console.log(["\t\tSEED ", ipp, " into ", self.master[hash].ipp].join(""));
      logT ( ("\t\tSEED " ++ (show ipp) ++ " into " ++ (show $ lineIpp line) ))
      return ()
    Nothing -> return ()
  
-- ---------------------------------------------------------------------


myNop :: TeleHash ()
myNop = do return ()

updateTelehashLine :: Line -> TeleHash ()
updateTelehashLine line = do
  switch <- get
  
  let 
    master = (swMaster switch)
    
    endpointHash = lineEnd line
    
    master' = Map.insert endpointHash line master
    
    switch' = switch {swMaster = master'}
    
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

hashToIpp :: Map.Map Hash Line -> Hash -> IPP
hashToIpp master h = 
  let
    Just line = getLineMaybe master h
  in
    lineIpp line

-- ---------------------------------------------------------------------

-- TODO: What return type makes sense? The Bool will always be true.
isLineOk :: Line -> Integer -> Telex -> Either String Bool
isLineOk line secsNow msg = result
  where
    timedOut =
      case (lineLineat line) of
        Just (TOD secs _picos) -> secsNow - secs > 40 -- Was 10 
        Nothing -> False  
  
    msgLineOk = case (teleLine msg) of
      Just msgLineNum -> 
        case (lineLine line) of
          Just lineNum -> lineNum == msgLineNum && (lineNum `mod` (lineRingout line) == 0)
          Nothing -> True -- only test if we have a value
      Nothing -> True  
      
    lineOk = msgLineOk && not timedOut
    
    result = case (lineOk) of
      True -> Right True
      False -> Left $ "msgLineOk=" ++ (show msgLineOk) ++ ",timedOut=" ++ (show timedOut) 

-- ---------------------------------------------------------------------

isRingOk :: Line -> Telex -> Bool
isRingOk line msg = ringOk
  where
    ringOk = case (teleRing msg) of
      Just msgRing -> msgRingOk && lineOk
        where
            -- Check that line.ringin matches if it exists
            -- Check the incoming ring > 0 and <= 32768
            msgRingOk = (msgRing > 0) && (msgRing <= 32768)
            lineOk = case (lineRingin line) of 
                      Just ri -> (ri == msgRing)
                      Nothing -> True

      Nothing -> True
    
-- ---------------------------------------------------------------------
    
checkLine :: Line -> Telex -> ClockTime -> Either String Line
checkLine line msg timeNow@(TOD secsNow _picosecsNow) = 
  let
    lineOk = isLineOk line secsNow msg
  
    line' = case lineOk of
              Right _ -> case (lineLineat line) of
                Just _ -> line
                Nothing -> case (teleLine msg) of
                  Just msgLine ->
                    line {lineRingin = Just (msgLine `div` ((lineRingout line))),
                          lineLine   = teleLine msg,
                          lineLineat = Just timeNow
                         }
                  Nothing -> line
              Left _ -> line

    ringOk = isRingOk line' msg
    
    line'' = case (teleRing msg) of
               Just msgRing ->
                 if (not ringOk) 
                   then line'
                   else (if ((lineLineat line) /= Nothing) 
                      then line'
                      else line' { 
                                 lineRingin = Just msgRing,
                                 lineLine   = Just (msgRing * (lineRingout line)),
                                 lineLineat = Just timeNow
                                 })
               Nothing -> line'
                   
    valid = case lineOk of
              Left err -> Left err
              Right _  -> if ringOk then (Right True) else (Left "ringOk=False")
    
    msgLength = fromJust (teleMsgLength msg)
    
    line''' = case valid of
                 Right _ -> line'' { lineBr = (lineBr line'') + msgLength,
                               lineBrin = msgLength }
                 Left _  ->  line''
                    
    brOk = (lineBr line''') - (lineBrout line''') <= 12000
  in
   case valid of
     Left err -> Left err
     Right _ -> case brOk of
       True -> Right $ line''' { lineSeenat = Just timeNow }
       False -> Left $ "lineOk=" ++ (show lineOk) ++ ",ringOk=" ++ (show ringOk) ++ ",brOk=" ++ (show brOk) 
       
-- ---------------------------------------------------------------------

mkTelex :: IPP -> Telex
mkTelex seedIPP = 
    -- set _to = seedIPP
  -- Telex Nothing Nothing 0 (T.pack seedIPP) Nothing Nothing Nothing Map.empty -- Nothing
  Telex Nothing Nothing 0 seedIPP Nothing Nothing Nothing Nothing Nothing Map.empty Nothing 
                  
-- ---------------------------------------------------------------------  
--
-- Listen for incoming messages and drop them in the FIFO
--
dolisten :: Maybe SocketHandle -> Chan Signal -> IO ()
dolisten (Just h) channel = forever $ do
    (msg,rinfo) <- (SB.recvFrom (slSocket h) 1000)

    -- (putStrLn ("dolisten:rx msg=" ++ (BC.unpack msg)))
    (writeChan channel (SignalMsgRx (BC.unpack msg) rinfo))

-- ---------------------------------------------------------------------  
{-
sendTelex :: Telex -> TeleHash ()
sendTelex msg = do 
  timeNow <- io getClockTime
  line <- getOrCreateLine (teleTo msg) timeNow
  
  -- check br and drop if too much
  --  if (line.bsent - line.brin > 10000) {
  --      console.log("\tMAX SEND DROP\n");
  --      return;
  --  }

  let brBad = (lineBsent line) - (lineBrin line) > 10000
  case brBad of
    True -> do
      logT ( "MAX SEND DROP ")
      return ()
    False -> do
          -- if a line is open use that, else send a ring
          -- if ("line" in line) {
          --    telex._line = parseInt(line["line"]);      
          -- } else {
          --    telex._ring = parseInt(line["ringout"]);
          -- }

      let
        msg' = if (lineLine line == Nothing) 
               then (msg {teleRing = Just (lineRingout line)})
               else (msg {teleLine = Just (fromJust (lineLine line))})
                    
        -- update our bytes tracking and send current state
        -- telex._br = line.brout = line.br;
        msg'' = msg' { teleBr = lineBr line }
        
        msgJson = encodeTelex msg''
    
        line' = line { 
            lineBrout = lineBr line 
          , lineBsent = (lineBsent line) + (length msgJson)
          , lineSentat = Just timeNow            
          }       
        
      -- console.log(["SEND[", telex._to, "]\t", msg].join(""));
      logT ( "SEND[:" ++ (show $ teleTo msg'') ++ "]\t" ++ (msgJson))
                
      Just socketh <- gets swH
      addr <- io (addrFromHostPort (lineHost line) (linePort line))
      io (sendDgram socketh msgJson addr)
      
      updateTelehashLine(line')
      
      --return ()
-}
sendTelex :: Telex -> TeleHash ()
sendTelex msg = do 
  timeNow <- io getClockTime
  res <- prepareTelex msg timeNow
  case (res) of
    Nothing -> return ()
    Just (line,msgJson) -> do
      -- console.log(["SEND[", telex._to, "]\t", msg].join(""));
      logT ( "SEND[:" ++ (show $ teleTo msg) ++ "]\t" ++ (msgJson))
                
      Just socketh <- gets swH
      addr <- io (addrFromHostPort (lineHost line) (linePort line))
      io (sendDgram socketh msgJson addr)
      
      updateTelehashLine(line)
      
-- ---------------------------------------------------------------------  

prepareTelex :: Telex -> ClockTime -> TeleHash (Maybe (Line,String))
prepareTelex msg timeNow = do 
  line <- getOrCreateLine (teleTo msg) timeNow
  
  -- check br and drop if too much
  --  if (line.bsent - line.brin > 10000) {
  --      console.log("\tMAX SEND DROP\n");
  --      return;
  --  }

  let brBad = (lineBsent line) - (lineBrin line) > 10000
  case brBad of
    True -> do
      logT ( "MAX SEND DROP ")
      return Nothing
    False -> do
      -- if a line is open use that, else send a ring
      let
        msg' = if (lineLine line == Nothing) 
               then (msg {teleRing = Just (lineRingout line)})
               else (msg {teleLine = Just (fromJust (lineLine line))})
                    
        -- update our bytes tracking and send current state
        -- telex._br = line.brout = line.br;
        msg'' = msg' { teleBr = lineBr line }
        
        msgJson = encodeTelex msg''
    
        line' = line { 
            lineBrout = lineBr line 
          , lineBsent = (lineBsent line) + (length msgJson)
          , lineSentat = Just timeNow            
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
{-
--sendMsg :: Json a => SocketHandle -> a -> IO ()
sendMsg socketh msg =
  sendstr sendmsg
    where 
      --sendmsg = BC.unpack $ head $ BL.toChunks $ encode $ toJson msg
      sendmsg = encodeMsg msg
  
      -- Send until everything is done
      sendstr :: String -> IO ()
      sendstr [] = return ()
      sendstr omsg = do sent <- sendTo (slSocket socketh) omsg (slAddress socketh)
                        sendstr (genericDrop sent omsg)
-}          
-- ---------------------------------------------------------------------

-- (telex._line ? "OPEN":"RINGING")
getLineStatus :: Telex -> [Char]
getLineStatus msg = 
  case (teleLine msg) of
    Just _ -> "OPEN"
    Nothing -> "RINGING"
  
-- ---------------------------------------------------------------------

-- Dispatch incoming raw messages

recvTelex :: String -> SockAddr -> TeleHash ()
recvTelex msg rinfo = do
    -- logT ( ("recvTelex:" ++  (show (parseTelex msg))))
    
    switch <- get
    seedsIndex <- gets swSeedsIndex

    (Just hostIP,Just port) <- io (getNameInfo [NI_NUMERICHOST] True True rinfo)
    let
      remoteipp = IPP (hostIP ++ ":" ++ port)
    
    timeNow <- io getClockTime
    --console.log(["RECV from ", remoteipp, ": ", JSON.stringify(telex)].join(""));
    logT ("RECV from " ++ (show remoteipp) ++ ":"++ msg
                  ++ " at " ++ (show timeNow))
    let
      rxTelex = parseTelex msg
    
    isOnline <- checkOnline rxTelex remoteipp timeNow
    {-
    case (swConnected switch) of
      False -> do
        -- TODO : test that _to field is set. Requires different setup for the JSON leg. 
        --        Why would it not be set? Also test that the _to value is us.
        -- // must have come from a trusted seed and have our to IPP info
        case (Set.member remoteipp seedsIndex ) of
          True -> online rxTelex timeNow
          False -> do
            logT ( "recvTelex:we're offline and don't like that")
            return () -- TODO: no further processing. ++FIX++This should be a control return

      True -> do
        -- logT ( "recvTelex:already online")
        myNop
    -}
      
    case isOnline of                
      False -> return ()
      True -> do
        -- // if this is a switch we know, check a few things
        line <- getOrCreateLine remoteipp timeNow
        let lstat = checkLine line rxTelex timeNow
        case lstat of
          Left reason -> do
            logT ( "\tLINE FAIL[" ++ reason ++ ", " ++ (show (lineIpp line, lineEnd line)))
            myNop
          Right line' -> do
            -- // we're valid at this point, line or otherwise, track bytes
            let diff = (lineBsent line') - (teleBr rxTelex)
            -- console.log(["\tBR ", line.ipp, " [", line.br, " += ",br, "] DIFF ", (line.bsent - t._br)].join(""));
            logT ("\tBR " ++ (show $ lineIpp line') ++ " [" ++ (show $ lineBr line') ++ " += " ++ (show $ length msg) ++ "] DIFF " ++ 
                  (show (diff, (lineBsent line') , (teleBr rxTelex))))
                   -- (show $ ((lineBsent line') , (teleBr rxTelex)) ))

            logT ( "\tLINE STATUS " ++ (getLineStatus rxTelex))
            updateTelehashLine line'
            processCommands rxTelex remoteipp line'
            processSignals  rxTelex remoteipp line'

        tapSignals (hasSignals rxTelex) rxTelex
    
        return ()

-- ---------------------------------------------------------------------
        
checkOnline :: Telex -> IPP -> ClockTime -> TeleHash Bool
checkOnline rxTelex remoteipp timeNow = do
  switch <- get
  seedsIndex <- gets swSeedsIndex

  case (swConnected switch) of
      False -> do
        -- TODO : test that _to field is set. Requires different setup for the JSON leg. 
        --        Why would it not be set? Also test that the _to value is us.
        -- // must have come from a trusted seed and have our to IPP info
        case (Set.member remoteipp seedsIndex ) of
          True -> do
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
      master <- gets swMaster
      mapM_ (\line -> tapLine rxTelex line) $ Map.elems master    
      return ()
  
-- ---------------------------------------------------------------------

tapLine :: Telex -> Line -> TeleHash ()
tapLine telex line = do 
  mapM_ (\rule -> processRule rule) $ lineRules line
  where
    processRule rule = do
      logT ( "\t TAP CHECK IS " ++ (show $ lineIpp line) ++ "\t" ++ (show rule))
      isMatch <- foldM (\acc (k,v) -> matchIs acc (k,v)) True [(tapIs rule)]
      hasMatch <- foldM (\acc k -> matchHas acc k) True (tapHas rule)
      forward (isMatch && hasMatch)
      return ()
      
    matchIs :: Bool -> (String,String) -> TeleHash Bool  
    matchIs acc (isKey,isVal) = do
      let
        telexIsVal = case (isKey) of
          ".end" -> teleSigEnd telex
          _      -> Nothing
      logT ( "IS match: " ++ (show telexIsVal) ++ "=" ++ (show isVal)++ "?")
      case (isKey) of
        ".end" -> return (acc && ((teleSigEnd telex) == Just (Hash isVal)))
        _      -> return False
    
    
    matchHas acc hasKey = do
      -- console.log("HAS match: " + hasKey + " -> " + (hasKey in telex));
      hasKeyInTelex <- case (hasKey) of
        "+pop" -> return $ (teleSigPop telex) /= Nothing
        x      -> do
          logT ( "HAS match: no code for " ++ (show x))
          return False
            
      logT ( "HAS match: " ++ hasKey ++ " -> " ++ (show hasKeyInTelex))
      return (acc && hasKeyInTelex)

    -- // forward this switch a copy
    forward False = do 
      logT ( "\tCHECK MISS")
      return ()
    forward True  = do                  
      let
        swipp = lineIpp line
      Just selfipp <- gets swSelfIpp
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
        ".end" -> tel {teleSigEnd = (teleSigEnd telex)}
        ".pop" -> tel {teleSigPop = (teleSigPop telex)}
        _ -> undefined -- Should never happen, but catch extensions when they come
                       -- TODO: make this recover gracefully and log something, instead
                        
-- ---------------------------------------------------------------------

processCommands :: Telex -> IPP -> Line -> TeleHash ()
processCommands rxTelex remoteipp line = do
  mapM_ (\k -> processCommand k remoteipp rxTelex line) (getCommands rxTelex)      
  return () 

getCommands :: Telex -> [String]
getCommands telex = filter isCommand $ Map.keys (teleRest telex)
  where
    isCommand k = (head k) == '.'

-- ---------------------------------------------------------------------
    
processCommand
  :: String -> IPP -> Telex -> Line -> TeleHash ()
processCommand ".see" remoteipp telex line = do 
  -- logT ( "processCommand .see")
  --switch <- get
  Just selfipp <- gets swSelfIpp
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
    Just tap -> updateTelehashLine (line { lineRules = tap })
  return ()

-- ---------------------------------------------------------------------
  
processCommand cmd _remoteipp _telex _line = do 
  logT ( "processCommand : ***ignoring*** " ++ (show cmd))
  return ()
  
-- ---------------------------------------------------------------------

-- We are processing a single .see entry here, which is not ourselves
processSee :: Line -> IPP -> IPP -> TeleHash ()
processSee line remoteipp seeipp = do
  -- logT ( "processSee " ++ (show line) ++ "," ++ (show remoteipp) ++ "," ++ (show seeipp))
  -- logT ( "processSee " ++ (show remoteipp) ++ "," ++ (show seeipp))

  switch <- get
  Just selfipp  <- gets swSelfIpp
  Just selfhash <- gets swSelfHash

  seeVisible (seeipp == remoteipp && not (lineVisible line)) line selfipp remoteipp
  
  -- let 
  --   lineSee = getLineMaybe (swMaster switch) (mkHash seeipp)
    
  case (getLineMaybe (swMaster switch) (mkHash seeipp)) of
    Just _lineSee -> return () -- // skip if we know them already
    Nothing ->
        -- // XXX todo: if we're dialing we'd want to reach out to any of these closer to that $tap_end
        -- // also check to see if we want them in a bucket
        case (bucket_want selfhash seeipp) of
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

seeVisible :: Bool -> Line -> IPP -> IPP -> TeleHash ()
seeVisible False _line _selfipp _remoteipp = do return ()
seeVisible True  line  selfipp  remoteipp = do
  -- // they're making themselves visible now, awesome
  logT ( "\t\tVISIBLE " ++ (show remoteipp))
  let line' = line {lineVisible = True}
  
  Right newNeighbourList <- near_to (lineEnd line') selfipp
  updateTelehashLine (line' { lineNeighbors = Set.union (Set.fromList newNeighbourList) (lineNeighbors line') }) 
  
  _ <- near_to (lineEnd line) remoteipp -- // injects this switch as hints into it's neighbors, fully seeded now
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
    master = (swMaster switch)

  case (getLineMaybe master (mkHash ipp)) of
    Nothing -> return $ Left "no line for ipp" 
    Just line -> do
      let 
        -- of the existing and visible cached neighbors, sort by distance to this end
        see = sortBy hashDistanceOrder $ Set.toList $ Set.filter isLineVisible (lineNeighbors line)
        
        isLineVisible x = case (getLineMaybe master x) of
          Just l -> lineVisible l
          Nothing -> False
          
        hashDistanceOrder a b
          | dist < 0 = LT
          | dist > 0 = GT
          | otherwise = EQ
          where 
            dist = (distanceTo endHash a) - (distanceTo endHash b)
            
      near_to_see line endHash ipp see
      
near_to_see :: Line -> Hash -> IPP -> [Hash] -> TeleHash (Either String [Hash])
near_to_see _line _endHash _ipp []  = do return $ Left "empty see list"      
near_to_see line  endHash  ipp  see = do                    
      let
        firstSee     = head see      
        firstSeeHash = firstSee
        lineNeighborKeys = (lineNeighbors line)
        lineEndHash  = lineEnd line
      logT ( ("\tNEARTO " ++ (show endHash) ++ "\t" ++ (show ipp) ++ "\t" ++ (show $ Set.size lineNeighborKeys) 
          ++ ">" ++ (show $ length see)
          ++ "\t" ++ (show $ distanceTo firstSeeHash endHash) ++ "=" ++ (show $ distanceTo lineEndHash endHash)))
        
      -- it's either us or we're the same distance away so return these results
      case (firstSee == (lineEnd line) 
            || ((distanceTo firstSeeHash endHash) == (distanceTo lineEndHash endHash))) of
        True -> 
          -- this +end == this line then replace the neighbors cache with this result 
          -- and each in the result walk and insert self into their neighbors
          case (lineEndHash == endHash) of
            True -> do
              logT ( ("\tNEIGH for " ++ (show endHash) ++ " was " ++ (show lineNeighborKeys) ++ (show $ length see)))
              let neigh = Set.fromList $ take 5 see
              updateTelehashLine (line {lineNeighbors = neigh})
              logT ( ("\tNEIGH for " ++ (show endHash) ++ " is " ++ (show neigh) ++ (show $ length see)))

              mapM_ (\hash -> addNeighbour hash endHash ipp) $ Set.toList neigh
              logT ( ("\t\tSEE distance=" ++ (show $ distanceTo endHash firstSeeHash) ++ " count=" ++ (show $ length see) ))
              return $ Right see
            False -> do 
              logT ( ("\t\tSEE distance=" ++ (show $ distanceTo endHash firstSeeHash) ++ " count=" ++ (show $ length see) ))
              return $ Right see -- TODO:plug in bit below
        False -> do
          -- whomever is closer, if any, tail recurse endseeswitch them
          Just lineFirstSee <- getLineMaybeM firstSee
          near_to endHash (lineIpp lineFirstSee)


-- ---------------------------------------------------------------------

bucket_want :: Hash -> IPP -> Bool
bucket_want selfhash ipp = 
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
  in
   (pos >= 0) && (pos <= nBuckets)
   

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

processSignals :: Telex -> IPP -> Line -> TeleHash ()
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

processSignal :: String -> IPP -> Telex -> Line -> TeleHash ()
processSignal "+end" remoteipp telex line = do 
  logT ( "processSignal :  +end")
  
  -- switch <- get
  master        <- gets swMaster
  Just selfipp  <- gets swSelfIpp
  Just selfhash <- gets swSelfHash
  
  let 
    hop = safeGetHop telex
    Just end = teleSigEnd telex
  case (hop) of
    0 -> do
      let
        -- // start from a visible switch (should use cached result someday)
        vis = if (lineVisible line) then (remoteipp) else (selfipp)
      Right hashes <- near_to end vis -- // get closest hashes (of other switches)
      logT ( "+end hashes: " ++ (show hashes))
      
        -- // convert back to IPPs
        {-
        var ipps = {};
        hashes.slice(0,5).forEach(function(hash){
            ipps[self.master[hash].ipp] = 1;
        });
        -}
      let ipps = take 5 $ map (\h -> hashToIpp master h) hashes
      logT ( "+end ipps: " ++ (show ipps))

        {-
        -- // TODO: this is where dampening should happen to not advertise switches that might be too busy
        if (!line.visibled) {
            ipps[self.selfipp] = line.visibled = 1; // mark ourselves visible at least once
        }
        -}
      let   
        ipps' = if (lineVisibled line) then (ipps) else (selfipp:ipps)
      updateTelehashLine (line { lineVisibled = True })
      
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
  put $ switch {swConnected = True
              , swSelfIpp   = Just selfIpp
              , swSelfHash  = Just selfhash
              }
  
  logT ( ("\tSELF[" ++ (show selfIpp) ++ " = " ++ (show selfhash) ++ "]"))
    
  line <- getOrCreateLine selfIpp timeNow

  -- ++AZ++
  switch <- get
  -- let taps' = (Tap ("+end",unHash selfhash) ["+news"]):(swTaps switch)
  let taps' = (Tap ("+end",unHash selfhash) ["+news"]):[]
  put $ switch { swTaps = taps'}
  -- ++AZ++

  taps <- gets swTaps
  
  updateTelehashLine (line {lineVisible = True, lineRules = taps })
    
  -- // trigger immediate tapping to move things along
  taptap timeNow
  
  return ()
                    
                    
-- ---------------------------------------------------------------------

offline :: TeleHash ()
offline = do
  logT ( "OFFLINE")
  switch <- get
  put $ switch { swSelfIpp = Nothing,
                swSelfHash = Nothing,
                swConnected = False,
                swMaster = Map.empty
              }
    
-- ---------------------------------------------------------------------
  
taptap :: ClockTime -> TeleHash ()
taptap timeNow@(TOD secs _picos) = do 
  -- logT ( "taptap: " ++ (show timeNow))
  {-
    var self = this;
    if(!self.connected) return;
  -}
  connected <- gets swConnected
  case (connected) of
    False -> return ()
    True -> do
      taps <- gets swTaps
      mapM_ doTap taps
      return ()
      
  where
    doTap tap = do
      case (tapIs tap) of
        ("+end",tapEnd) -> do
          -- We have tested for online status, these will be set
          Just selfipp  <- gets swSelfIpp
          Just selfhash <- gets swSelfHash
          Right candidateHashes <- near_to (Hash tapEnd) selfipp
          let hashes = take 3 $ filter (\hash -> hash /= selfhash) candidateHashes
          mapM_ (\hash -> doTapLine tap tapEnd hash) hashes
          return ()
        _          -> return ()  
        
    doTapLine tap tapEnd hash = do
      Just line <- getLineMaybeM hash
      let (TOD tapLastSecs _) = fromMaybe (TOD 0 0) (lineTapLast line)
      case (tapLastSecs + 50 > secs) of -- Assuming wall clock secs > 50 
        True -> return ()
        False -> do
          updateTelehashLine (line { lineTapLast = Just timeNow }) 
          let telexOut = (mkTelex (lineIpp line)) {teleTap = Just [tap] }
          -- console.log(["\tTAPTAP to ", line.ipp, " end ", tapEnd, " tap ", JSON.stringify(tap)].join(""));
          logT (  "\tTAPTAP to " ++ (show $ lineIpp line) ++ " end " ++ (show tapEnd) ++ " tap " ++ (show tap))
          sendTelex telexOut
          return ()
      return ()

-- ---------------------------------------------------------------------  
{-**
 * Update status of all lines, removing stale ones.
 *-}
scanlines :: ClockTime -> TeleHash ()
scanlines now@(TOD _secs _picos) = do
  connected <- gets swConnected
  case (connected) of
    False -> return ()
    True -> do
      master <- gets swMaster
      let switches = Map.keys master
      logT ( "SCAN\t" ++ (show $ length switches))
      valid <- foldM (\acc hash -> fscanline acc hash) True switches
      
      seedsIndex <- gets swSeedsIndex
      Just selfipp  <- gets swSelfIpp
      case (valid == False && Set.notMember selfipp seedsIndex) of
        True -> do
          logT ( "scanlines:" ++ (show now))
          offline
          return ()
        False -> return ()
      return ()
  
  where
  {-
    var self = this;
    if(!self.connected) return;
    var now = time();
    var switches = keys(self.master);
    var valid = 0;
    console.log(["SCAN\t" + switches.length].join(""));
  -}
    fscanline :: Bool -> Hash -> TeleHash Bool
    fscanline acc hash = do 
      res  <- scanline hash
      return (acc && res)
                            
    -- Return whether line is still Ok or not.
    scanline :: Hash -> TeleHash Bool
    scanline hash = do
      Just line     <- getLineMaybeM hash
      Just selfHash <- gets swSelfHash
      Just selfipp  <- gets swSelfIpp
      case (selfHash == hash) of
        True -> return True -- False -- // skip our own endpoint and what is this (continue)
        False -> do
          case (lineEnd line /= hash) of
            True -> return False -- // empty/dead line (continue)
            False -> do
              timedOut <- isTimedOut now line
              case (timedOut) of
                True -> do
                  -- // remove line if they never responded or haven't in a while
                  -- console.log(["\tPURGE[", hash, " ", line.ipp, "] last seen ", now - line.seenat, "s ago"].join(""));
                  logT ( "\tPURGE[" ++ (show hash) ++ " " ++ (show $ lineIpp line) ++ "] last seen " ++ (show $ lineSeenat line))
                  removeLineM hash 
                  return False
                False -> do
                  -- We have a valid line, return true from here on out
                  connected <- gets swConnected
                  case connected of
                    True -> do
                      -- // +end ourselves to see if they know anyone closer as a ping
                      let 
                        telexOut = (mkTelex $ lineIpp line) { teleSigEnd = Just selfHash }
                      -- // also .see ourselves if we haven't yet, default for now is to participate in the DHT
                      let 
                        telexOut' = if (lineVisibled line) 
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
  {-  
        
        valid++;
        
        if (self.connected) {
        
            // +end ourselves to see if they know anyone closer as a ping
            var telexOut = new Telex(line.ipp);
            telexOut["+end"] = self.selfhash;
        
            // also .see ourselves if we haven't yet, default for now is to participate in the DHT
            if (!line.visibled++) {
                telexOut[".see"] = [self.selfipp];
            }
            
            // also .tap our hash for +pop requests for NATs
            var tapOut = {is: {}};
            tapOut.is['+end'] = self.selfhash;
            tapOut.has = ['+pop'];
            telexOut[".tap"] = [tapOut];
            self.send(telexOut);
            
        }
    });
    
    if (!valid && !self.seedsIndex[self.selfipp]) {
        self.offline();
    }
  -}
    isTimedOut (TOD secs _picos) line = do
      case (lineSeenat line) of
        Nothing -> do
          let (TOD initSecs _) = (lineInit line)
          return (secs - initSecs > 70)
        Just (TOD seenatSecs _) -> 
          return (secs - seenatSecs > 70)
        {-
        if ((line.seenat == 0 && now - line.init > 70)
                || (line.seenat != 0 && now - line.seenat > 70)) {
            // remove line if they never responded or haven't in a while
            console.log(["\tPURGE[", hash, " ", line.ipp, "] last seen ", now - line.seenat, "s ago"].join(""));
            self.master[hash] = {};
            return;
        }
        -}
-- ---------------------------------------------------------------------
{-
/**
 * Hash objects represent a message digest of string content,
 * with methods useful to DHT calculations.
 */
-}

mkHash :: IPP -> Hash
mkHash (IPP str) =
  let
    digest = SHA.sha1 $ BL.fromChunks [BC.pack str]
  in  
   -- B64.encode $ BL.unpack $ SHA.bytestringDigest digest
   Hash (show digest)


-- ---------------------------------------------------------------------
-- Convenience.
--
io :: IO a -> TeleHash a
io = liftIO

-- ---------------------------------------------------------------------
-- Testing, see http://hackage.haskell.org/package/test-framework-th
    
--main = $(defaultMainGenerator)


-- EOF