{-# LANGUAGE FlexibleInstances #-} -- For show of swSender

module TeleHash.TeleHash
       (
         initialize
       , seed
       -- , listen
       -- , connect
       -- , send
       , tap
       , dial
       , announce
       , ping
       -- , shutdown

       , Signal (..)
       , Reply (..)
       , querySwitch
       ) where

import Control.Concurrent
import Control.Monad.State
import Network.Socket
import System.IO
import System.Log.Handler.Simple
import System.Log.Logger
import System.Time
import TeleHash.Switch
import TeleHash.Telex
import Text.JSON
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Network.Socket.ByteString as SB
import qualified Data.ByteString.Char8 as BC


-- ---------------------------------------------------------------------

--
-- | state variables
data StateVar = StateVar
                { svSelf :: Master
                , svListeners :: [Tap] -- ^maintain an array of .tap rules we are interested in
                , svConnectors :: Set.Set IPP -- ^maintains a hashtable of ends we are interested in contacting indexed by a end name.
                , svResponseHandlers :: Map.Map Hash String -- ^maintains a hashtable of response handlers indexed by connection 'guid'
                }

{-
var self;
var listeners = [];         //maintain an array of .tap rules we are interested in
var connectors = {};        //maintains a hashtable of ends we are interested in contacting indexed by a end name.
var responseHandlers = {};  //maintains a hashtable of response handlers indexed by connection 'guid'
-}


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
            { selfMode :: Mode
            , selfState :: SwitchState
            , selfSeeds :: [IPP]
            , selfNat :: Bool
            , selfServer :: Maybe Socket
            , selfCallbacks :: Callbacks
            }
            deriving (Show)

mkMaster = Master
                  {
                    selfMode = ModeListener
                  , selfState = StateOffline
                  , selfSeeds = [IPP "208.68.164.253:42424", IPP "208.68.163.247:42424"]
                  , selfNat = False
                  , selfServer = Nothing
                  , selfCallbacks = mkCallbacks ModeListener
                  }

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
-- OLD
-- ---------------------------------------------------------------------
--
-- | The 'TeleHash' monad, a wrapper over IO, carrying the switch's immutable state.
--
-- type TeleHash = StateT OSwitch IO
type TeleHash = StateT Master IO

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
  --show doSendDgram     = "doSendDgram"
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

{-
newtype Hash = Hash String
--             deriving (Data,Eq,Show,Typeable,Ord)
             deriving (Eq,Show,Ord)
unHash :: Hash -> String
unHash (Hash str) = str


newtype IPP = IPP String
             -- deriving (Data,Eq,Show,Typeable,Ord)
             deriving (Eq,Show,Ord)
unIPP :: IPP -> String
unIPP (IPP str) = str

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

parseTelex :: String -> Maybe Telex
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
     case decoded of
       Ok _ ->
         Just (mkTelex (IPP to)) {teleRing = maybeRing,
                   teleSee = maybeSee',
                   teleBr = br,
                   teleTo = (IPP to), teleLine = maybeLine, teleHop = maybeHop,
                   teleSigEnd = maybeEnd, teleTap = maybeTap,
                   teleSigPop = maybePop,
                   teleRest = Map.fromList $ map (\(name,val) -> (name, encode val)) (fromJSObject cc), -- horrible, but will do for now
                   teleMsgLength = Just msgLength }
       _ -> Nothing

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
    getTap _                = error "Should never happen"

    dTap tap ("is",  JSObject o) = tap {tapIs = (k, fromJSString str)}
      where
        (k,JSString str) = head $ fromJSObject o

    --dTap tap ("has", o) = tap {tapHas= [show o]}
    dTap tap ("has", JSArray arr) = tap {tapHas= map (\(JSString s) -> fromJSString s) arr}
    dTap _   _                    = error "Should never happen"
-}

-- ---------------------------------------------------------------------

-- called init in the node.js version
initialize = getSelf

-- ---------------------------------------------------------------------

-- seed :: IO (
seed arg = do
  getSelf arg

-- ---------------------------------------------------------------------

listen = undefined

connect = undefined

send = undefined

tap = undefined

dial = undefined

announce = undefined

ping = undefined

shutdown = undefined

-- ---------------------------------------------------------------------

--getSelf :: Maybe Master -> IO (Chan a,Chan b,Master)
getSelf :: Maybe Master -> IO (Chan Signal,Chan Reply,ThreadId)
getSelf arg = do
  sock <- socket AF_INET Datagram defaultProtocol

  -- We want to listen on all interfaces (0.0.0.0)
  bindAddr <- inet_addr "0.0.0.0"
  bindSocket sock (SockAddrInet 0 bindAddr)

  socketName <- getSocketName sock
  warningM "Controller" ("server listening " ++ (show socketName))

  ch1 <- newChan
  ch2 <- newChan
  let st  = mkMaster { selfServer = Just sock }

  thread <- forkIO (doit ch1 ch2 st)
  return (ch1,ch2,thread)

  where
    doit :: Chan Signal -> Chan Reply -> Master -> IO ()
    doit ch1 ch2 st = do
      _ <- runStateT (run ch1 ch2) st
      return ()

  -- return $ (ch1,ch2,mkMaster { selfServer = Just sock })

-- ---------------------------------------------------------------------

run :: Chan Signal -> Chan Reply -> TeleHash ()
run ch1 ch2 = do
  -- ch1 <- io (newChan)
  _ <- io (forkIO (timer (10 * onesec) SignalPingSeeds ch1))
  _ <- io (forkIO (timer (10 * onesec) SignalScanLines ch1))
  _ <- io (forkIO (timer (30 * onesec) SignalTapTap ch1))

  h <- gets selfServer
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
      -- External commands
      SignalGetSwitch      -> do
        master <- getMaster
        io (writeChan ch2 (ReplyGetMaster master))
    -- io (putStrLn $ "done signal:at " ++ (show timeNow))

-- ---------------------------------------------------------------------

getMaster :: TeleHash Master
getMaster = do
  master <- get
  return master

-- ---------------------------------------------------------------------
--
-- Listen for incoming messages and drop them in the FIFO
--
dolisten :: Maybe Socket -> Chan Signal -> IO ()
dolisten Nothing _ = return ()
dolisten (Just h) channel = forever $ do
    (msg,rinfo) <- (SB.recvFrom h 1000)

    -- (putStrLn ("dolisten:rx msg=" ++ (BC.unpack msg)))
    (writeChan channel (SignalMsgRx (BC.unpack msg) rinfo))

-- ---------------------------------------------------------------------

data Signal = SignalPingSeeds | SignalScanLines | SignalTapTap | SignalMsgRx String SockAddr |
              SignalGetSwitch
              deriving (Show, Eq)

data Reply = ReplyGetMaster Master
           | ReplyGetSwitch Switch
           deriving (Show)

onesec :: Int
onesec = 1000000

timer :: Int -> a -> Chan a -> IO ()
timer timeoutVal signalValue channel  = forever $
  threadDelay timeoutVal >> writeChan channel signalValue

-- ---------------------------------------------------------------------
-- Routines to interact with the running switch, via the comms channel
querySwitch :: Chan Signal -> Chan b -> IO b
querySwitch ch1 ch2 = do
  writeChan ch1 SignalGetSwitch
  res <- readChan ch2
  return res

-- ---------------------------------------------------------------------
{-**
 * Update status of all lines, removing stale ones.
 *-}
scanlines :: ClockTime -> TeleHash ()
scanlines now@(TOD _secs _picos) = do return ()

-- ---------------------------------------------------------------------

taptap :: ClockTime -> TeleHash ()
taptap timeNow@(TOD secs _picos) = do return ()

-- ---------------------------------------------------------------------

-- Dispatch incoming raw messages

recvTelex :: String -> SockAddr -> TeleHash ()
recvTelex msg rinfo = do return ()

  -- ---------------------------------------------------------------------

{-
setup :: Maybe Master -> IO (Master)
setup arg =
  case arg of
    Just m -> m
    Nothing -> mkMaster
-}

-- ---------------------------------------------------------------------
pingSeeds :: TeleHash ()
pingSeeds = do return ()

{-
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
-}
-- ---------------------------------------------------------------------

purgeSeeds :: TeleHash ()
purgeSeeds = undefined
  {-
    self.seeds.forEach(function (ipp) {
        slib.getSwitch(ipp).drop();
    });
  -}

-- ---------------------------------------------------------------------

rotateToNextSeed :: TeleHash IPP
rotateToNextSeed = do
  seeds <- gets selfSeeds
  s <- get
  case seeds of
    [] -> return (IPP "")
    _  -> do
      put (s { selfSeeds = ((tail seeds) ++ [head seeds]) })
      return (head seeds)

-- ---------------------------------------------------------------------

{-
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
-}

-- ---------------------------------------------------------------------
-- Convenience.
--
io :: IO a -> TeleHash a
io = liftIO

-- ---------------------------------------------------------------------
-- Logging

logT :: String -> TeleHash ()
logT str = io (warningM "Switch" str)

-- ---------------------------------------------------------------------


-- EOF

