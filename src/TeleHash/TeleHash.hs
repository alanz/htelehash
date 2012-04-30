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

-- import Text.JSON
import Control.Concurrent
import Control.Monad.State
import Data.Maybe
import Data.String.Utils
import Network.Info
import Network.Socket
import System.IO
import System.Log.Handler.Simple
import System.Log.Logger
import System.Time
import TeleHash.Switch
import TeleHash.Telex
import TeleHash.Types
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Network.Socket.ByteString as SB
import qualified Data.ByteString.Lazy as BL

-- ---------------------------------------------------------------------

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
  let st  = mkMaster { selfServer = Just sock, selfSender = doSendDgram }

  thread <- forkIO (doit ch1 ch2 st)
  return (ch1,ch2,thread)

  where
    doit :: Chan Signal -> Chan Reply -> Master -> IO ()
    doit ch1 ch2 st = do
      _ <- runStateT (run ch1 ch2) st
      return ()

  -- return $ (ch1,ch2,mkMaster { selfServer = Just sock })

-- ---------------------------------------------------------------------

getMeMaybe :: TeleHash (Maybe Switch)
getMeMaybe = do
  master <- get
  case (selfMe master) of
    Nothing -> return Nothing
    Just ipp -> do
      switch <- getOrCreateSwitch ipp (TOD 0 0)
      return (Just switch)

getMe :: TeleHash Switch
getMe = do
  maybeMe <- getMeMaybe
  let Just me = maybeMe
  return me

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
      SignalMsgRx msg addr -> incomingDgram msg addr
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
    -- (writeChan channel (SignalMsgRx msg rinfo))

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

-- was recvTelex
incomingDgram :: String -> SockAddr -> TeleHash ()
incomingDgram msg rinfo = do

  {-
    if (self.state == STATE.offline) {
        //drop all packets
        return;
    }
  -}
  master <- get
  case (selfState master) of
    StateOffline -> do
      logT ("incomingDgram:offline, so dropping packet")
      return ()
    _ -> do

    -- logT ( ("incomingDgram:" ++  (show (parseTelex msg))))

      (Just hostIP,Just port) <- io (getNameInfo [NI_NUMERICHOST] True True rinfo)
      let
        remoteipp = IPP (hostIP ++ ":" ++ port)

      timeNow <- io getClockTime
      --console.log(["RECV from ", remoteipp, ": ", JSON.stringify(telex)].join(""));
      logT ("<--\tfrom " ++ (show remoteipp) ++ ":"++ msg
                  ++ " at " ++ (show timeNow))

      let
        maybeRxTelex = parseTelex msg

      case maybeRxTelex of
        Just rxTelex -> handleRxTelex rxTelex remoteipp timeNow
        Nothing -> do
          -- TODO:
          -- //out of band data non JSON. (used by the channels module)
          -- if (self.handleOOB) self.handleOOB(msg, rinfo);
          --   return;
          return ()

-- ---------------------------------------------------------------------

handleRxTelex :: Telex -> IPP -> ClockTime -> TeleHash ()
handleRxTelex rxTelex remoteipp timeNow = do
  {-
  if (self.state == STATE.seeding) {
    //only accept packets from seeds - note: we need at least 2 live seeds for SNAT detection
    for (var i in self.seeds) {
        if (from == self.seeds[i]) {
            handleSeedTelex(telex, from, msg.length);
            break;
        }
    }
    return;
  }
  if (self.state == STATE.online) {
      //process all packets
      handleTelex(telex, from, msg.length);
  }
  -}
  master <- get
  case (selfState master) of
    StateOffline -> return () -- Should not happen
    StateSeeding -> do
      -- only accept packets from seeds -
      -- NOTE: we need at least 2 live seeds for SNAT detection
      case (remoteipp `elem` (selfSeeds master)) of
        True -> handleSeedTelex rxTelex remoteipp timeNow
        False -> return ()
    StateOnline -> do
      handleTelex rxTelex remoteipp timeNow

-- ---------------------------------------------------------------------

handleSeedTelex :: Telex -> IPP -> ClockTime -> TeleHash ()
handleSeedTelex telex remoteipp timeNow = do
  {-
    //do NAT detection once
    if(!self.nat){
        if (!self.me && telex._to && !util.isLocalIP(telex._to)) {
            //we are behind NAT
            self.nat = true;
            console.log("NAT Detected!");
        }
    }
  -}
  master <- get
  isLocal <- isLocalIP (teleTo telex)
  case (fromMaybe False (selfNat master)) of
    True -> return ();
    False -> do
      case ( (selfMe master == Nothing)
             && (not isLocal)
           ) of
        True -> do
          logT ("NAT Detected")
          put (master { selfNat = Just True })

  master' <- get
  {-
      //first telex from seed will establish our identity
    if (!self.me && telex._to) {
        self.me = slib.getSwitch(telex._to);
        self.me.self = true; // flag switch to not send to itself
        clearTimeout(self.seedTimeout);
        delete self.seedTimeout;
        console.log("our identity:",self.me.ipp," hash:",self.me.end);
        //delay...to allow time for SNAT detection (we need a response from another seed)
        setTimeout(function () {
            if (!self.snat && self.mode == MODE.FULL){
                 self.me.visible = true; //become visible (announce our-selves in .see commands)
                 console.error('Making ourself Visible..');
            }
            self.state = STATE.online;
            console.log("GOING ONLINE: nat:",self.nat," snat:",self.snat, " visible:", self.me.visible," mode:", self.mode);
            if(self.nat) doPopTap(); //only needed if we are behind NAT
            if (self.onSeeded) self.onSeeded();
        }, 2000);
    }
  -}
  case (selfMe master') of
    Just _ -> return ();
    Nothing -> do
      -- first telex from seed will establish our identity
      switch <- getOrCreateSwitch remoteipp timeNow
      let
        switchMe = switch {
          swiIsSelf = True
          }

      updateTelehashSwitch switchMe

      -- TODO: This part should happen in 2000 ms, to allow more seed replies to detect SNAT
      -- NOTE: This process only makes sense if the initial seeding process hits more than one
      --       seed at the same time.
      delayedSetMe

-- ---------------------------------------------------------------------

handleTelex :: Telex -> IPP -> ClockTime -> TeleHash ()
handleTelex telex remoteipp timeNow = do
  return ()

-- ---------------------------------------------------------------------

delayedSetMe :: TeleHash ()
delayedSetMe = do
  {-
        //delay...to allow time for SNAT detection (we need a response from another seed)
        setTimeout(function () {
            if (!self.snat && self.mode == MODE.FULL){
                 self.me.visible = true; //become visible (announce our-selves in .see commands)
                 console.error('Making ourself Visible..');
            }
            self.state = STATE.online;
            console.log("GOING ONLINE: nat:",self.nat," snat:",self.snat, " visible:", self.me.visible," mode:", self.mode);
            if(self.nat) doPopTap(); //only needed if we are behind NAT
            if (self.onSeeded) self.onSeeded();
        }, 2000);
  -}
  master <- get
  case ((selfSNat master == Nothing || selfSNat master == Just False)
        && (selfMode master == ModeFull)) of
    False -> return ()
    True -> do
      me <- getMe
      logT ("Making ourself Visible..")
      updateTelehashSwitch (me {swiVisible = True})

  put $ (master { selfState = StateOnline })
  -- console.log("GOING ONLINE: nat:",self.nat," snat:",self.snat, " visible:", self.me.visible," mode:", self.mode);
  switchMe <- getMe
  let
    visible = case (selfMe master) of
      Nothing -> False
      Just ipp -> (swiVisible switchMe)

  logT("GOING ONLINE: nat:" ++ (show $ selfNat master) ++ " snat:" ++ (show $ selfSNat master) ++ " visible:" ++ (show visible) ++ " mode:" ++ (show $  selfMode master))

  case (selfNat master == Just True) of
    True -> doPopTap
    False -> return ()

  -- if (self.onSeeded) self.onSeeded();

-- ---------------------------------------------------------------------

doPopTap :: TeleHash ()
doPopTap = return ()

-- ---------------------------------------------------------------------

isLocalIP :: IPP -> TeleHash Bool
isLocalIP (IPP ipp) = do
  interfaces <- io (getNetworkInterfaces)
  let
    ifstrings = map (\interface -> show $ ipv4 interface) interfaces
    [ip,port] = split ":" ipp

    matching = filter (\ifs -> ip == ifs) ifstrings
  return (matching /= [])

-- ---------------------------------------------------------------------

updateTelehashSwitch :: Switch -> TeleHash ()
updateTelehashSwitch switch = do
  master <- get

  let
    network = (selfNetwork master)

    network' = Map.insert (swiIpp switch) switch network

    master' = master {selfNetwork = network'}

  put master'

-- ---------------------------------------------------------------------

{-
setup :: Maybe Master -> IO (Master)
setup arg =
  case arg of
    Just m -> m
    Nothing -> mkMaster
-}

-- ---------------------------------------------------------------------

setState :: SwitchState -> TeleHash ()
setState state = do
  master <- get
  put $ master {selfState = state }

-- ---------------------------------------------------------------------

pingSeeds :: TeleHash ()
pingSeeds = do
  seeds <- gets selfSeeds
  switchState <- gets selfState
  let connected = (switchState == StateOnline)

  -- logT $ "pingSeeds:" ++ (show connected) ++ " " ++ (show seeds)

  -- TODO: rotate the seeds, so the we use a fresh one each time through
  case (not connected) && (seeds /= []) of
    True -> do
      setState StateSeeding
      nextSeed <- rotateToNextSeed
      pingSeed nextSeed
    False -> return ()

-- ---------------------------------------------------------------------


pingSeed :: IPP -> TeleHash ()
pingSeed seed =
  do
    -- logT ( "pingSeed " ++ (show seed))

    (_serveraddr,ip,port) <- io (resolveToSeedIPP seed)

    --logT ( "pingSeed serveraddr=" ++ (show serveraddr))

    let seedIPP = IPP (ip ++ ":" ++ port)
    -- console.log(["SEEDING[", seedIPP, "]"].join(""));
    logT ( "SEEDING[" ++ (show seedIPP))

    --switch <- get
    --put switch {swSeedsIndex = Set.insert seedIPP (swSeedsIndex switch) }

    timeNow <- io getClockTime

    switch <- getOrCreateSwitch seedIPP timeNow
    let bootTelex = mkTelex seedIPP
    -- // any end will do, might as well ask for their neighborhood
    let bootTelex' = bootTelex { teleSigEnd = Just $ (swiHash switch) }

    sendTelex bootTelex'

    return ()

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
function resetIdentity() {
    if (self.me) {
        self.me.drop();
    }
    delete self.me;
    listeners = [];
    connectors = {};
    delete self.nat;
    delete self.snat;
}
-}
resetIdentity :: ClockTime -> TeleHash ()
resetIdentity timeNow = do
  master <- get
  case (selfMe master) of
    Nothing -> return ()
    Just ipp -> do
      switch <- getOrCreateSwitch ipp timeNow
      dropSwitch switch timeNow

  let
    master' = master { selfMe = Nothing
                     , selfListeners = []
                     , selfConnectors = Set.empty
                     , selfNat = Nothing
                     , selfSNat = Nothing
                     }

  put master'

-- ---------------------------------------------------------------------
{-
Switch.prototype.drop = function () {
    //PURGE!:  delete main reference to self, should auto-GC if no others
    console.error('purging.. ' + this.ipp);
    if (this.healthy()) this.send({
        _br: -10000
    });
    delete network[this.ipp];
}
-}
dropSwitch :: Switch -> ClockTime -> TeleHash ()
dropSwitch switch timeNow = do
  logT ("purging..." ++ (show $ swiIpp switch))
  case (healthy switch timeNow) of
    False -> return ()
    True -> do
      sendTelex $ (mkTelex (swiIpp switch)) { teleBr = -10000 }
  master <- get
  put $ master {selfNetwork = Map.delete (swiIpp switch) (selfNetwork master) }

-- EOF

