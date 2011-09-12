{-# LANGUAGE DeriveDataTypeable #-}

{- # LANGUAGE TypeOperators #-}
{- # LANGUAGE TemplateHaskell #-}
{- # LANGUAGE NoMonoPatBinds #-}

-- Above three settings are for the Json stuff using Data.Iso

module TeleHash.Controller 
       (
       -- For testing
       recvTelex  
       , parseTelex
       , mkTelex  
       , Telex(..)
       , Hash(..)  
       , Tap(..)  
       , Line(..)
       , IPP(..)  
       , Switch(..)         
       , SocketHandle(..)
       , TeleHash(..)
       , encodeMsg  
       , isLineOk
       , isRingOk  
       , mkLine  
       , checkLine  
       , getCommands  
       , getSignals  
       , mkHash  
       , distanceTo
       , near_to
       ) where

import Control.Applicative
import Control.Category
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Exception 
import Data.Bits
import Data.Char
import Data.List
import Data.Maybe
import Data.String.Utils
import Debug.Trace
import Network.BSD
import Network.Socket
import Numeric
import Text.JSON
import Text.JSON.Generic
import Text.JSON.Types
import Prelude hiding (id, (.), head, either, catch)
import System.Exit
import System.IO
import System.Time
--import TeleHash.Json 

--import Test.QuickCheck

import Text.Printf
import qualified Codec.Binary.Base64 as B64
import qualified Control.Concurrent.Actor as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Network.Socket.ByteString as SB
import qualified System.Random as R
--import Control.Category

{-

The approach taken here is a blend of the example in
Control.Concurrent.Actor, and the IRC bot example from
http://www.haskell.org/haskellwiki/Roll_your_own_IRC_bot

-}

--
-- The 'TeleHash' monad, a wrapper over IO, carrying the switch's immutable state.
--
--type TeleHash = ReaderT Switch IO
type TeleHash = StateT Switch IO

newtype Hash = Hash String
             deriving (Data,Eq,Show,Typeable,Ord)

newtype IPP = IPP String
             deriving (Data,Eq,Show,Typeable,Ord)
unIPP (IPP str) = str

data Switch = Switch { swH :: SocketHandle 
                     , swSeeds :: [String] -- IPP?
                     , swSeedsIndex :: Set.Set IPP
                     , swConnected :: Bool
                     , swMaster :: Map.Map Hash Line  
                     , swSelfIpp :: Maybe IPP
                     , swSelfHash :: Maybe Hash
                     , swTaps :: [Tap]  
                     } deriving (Eq,Show)

nBuckets = 160

data SocketHandle = 
    SocketHandle {slSocket :: Socket,
                  slAddress :: SockAddr
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
  lineBr        :: Int,
  lineBrout     :: Int,
  lineBrin      :: Int,
  lineBsent     :: Int,
  lineNeighbors :: Set.Set Hash, -- lineNeighbors,
  lineVisible   :: Bool,
  lineRules     :: [Tap]
  } deriving (Eq,Show)

-- Note: endPointStr should always be a IP:PORT
mkLine :: IPP -> ClockTime -> Line
mkLine endPoint@(IPP endPointStr) timeNow =
  let
    [hostIP,port] = split ":" endPointStr
    endPointHash = mkHash endPoint
    (TOD secs picosecs) = timeNow
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
       lineBr        = 0,
       lineBrout     = 0,
       lineBrin      = 0,
       lineBsent     = 0,
       lineNeighbors = Set.fromList [endPointHash],
       lineVisible   = False,
       lineRules     = []                       
       }

-- ---------------------------------------------------------------------

-- JSON stuff for a Telex
-- TODO: merge in stuff from Json.hs and get rid of that file, or refactor
-- See https://github.com/MedeaMelana/JsonGrammar for examples/howto

data Tap = Tap { tapIs :: (String,String), tapHas :: [String] } 
         deriving (Data, Typeable, -- For Text.JSON
                   Eq, Show)

data Telex = Telex 
             { teleRing   :: Maybe Int
             , teleSee    :: Maybe [String]
             , teleBr     :: Int
             , teleTo     :: IPP
             , teleLine   :: Maybe Int
             , teleHop    :: Maybe String
             , teleSigEnd :: Maybe Hash
             , teleTap    :: Maybe [Tap]  
             -- , teleRest   :: Maybe (Map.Map T.Text Value)
             -- , teleRest   :: (Map.Map T.Text Value)
             , teleRest   :: [(String, String)]
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
      maybeHop  = getStringMaybe      cc "_hop"
      maybeEnd  = getHashMaybe        cc "+end"
      maybeTap  = getTapMaybe         cc ".tap"
      msgLength = length s
    in 
     -- mkTelex to
     
     (mkTelex (IPP to)) {teleRing = maybeRing, teleSee = maybeSee, teleBr = br, 
                   teleTo = (IPP to), teleLine = maybeLine, teleHop = maybeHop,
                   teleSigEnd = maybeEnd, teleTap = maybeTap, 
                   teleRest = map (\(name,val) -> (name, encode val)) (fromJSObject cc), -- horrible, but will do for now
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
      
getIntMaybe :: JSObject JSValue -> String -> Maybe Int
getIntMaybe cc field =
  case (get_field cc field) of
      Just (JSRational _ jsVal)  -> Just $ round (jsVal)
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
      Just r -> [(".see", showJSON $ JSArray (map (\s -> JSString (toJSString s)) r))]
      Nothing -> []
    
    br = [("_br", JSString $ toJSString $ show (teleBr t))]

    line = case (teleLine t) of
      Just r -> [("_line", JSString $ toJSString $ show r)]
      Nothing -> []

    hop = case (teleHop t) of
      Just r -> [("_hop", JSString $ toJSString $ show r)]
      Nothing -> []
      
    end = case (teleSigEnd t) of
      Just (Hash r) -> [("+end", JSString $ toJSString r)]
      Nothing -> []

    tap = case (teleTap t) of
      Just r -> [(".tap", JSArray $ map toJSTap r)]
      Nothing -> []
      
    toJSTap tap = JSObject $ toJSObject [
      ("is", JSObject $ toJSObject [
          (k, JSString $ toJSString v),
          ("has", showJSON $ JSArray (map (\s -> JSString (toJSString s)) (tapHas tap)))
          ])]
      where
      (k,v) = tapIs tap

      
  in
   encode $ toJSObject (to++ring++see++br++line++hop++end++tap)
   
-- ---------------------------------------------------------------------

getRandom :: Int -> Int
getRandom seed = 
  let
    (res,_) = R.randomR (0,32767) (R.mkStdGen seed) 
  in
   res

-- ---------------------------------------------------------------------
--
-- Set up actions to run on start and end, and run the main loop
--
main :: IO ((),Switch)
main = bracket initialize disconnect loop
  where
    disconnect ss = sClose (slSocket (swH ss))
    -- loop st    = catch (runReaderT run st) (const $ return ())
    -- loop st    = catch (runReaderT run st) (exc)
    loop st    = catch (runStateT run st) (exc)

    exc :: SomeException -> IO ((),Switch)
    exc e = return ((),undefined)


-- ---------------------------------------------------------------------
-- Hardcoded params for now    
initialSeed = "telehash.org:42424"

initialize :: IO Switch
initialize =
    notify $ do 
       -- Look up the hostname and port.  Either raises an exception
       -- or returns a nonempty list.  First element in that list
       -- is supposed to be the best option.
       {-
       let [hostname,port] = split ":" initialSeed
       addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
       let serveraddr = head addrinfos
       -}
       (serveraddr,ip,port) <- resolveToSeedIPP initialSeed
       let seedIPP = IPP (ip ++ ":" ++ port)

       -- Establish a socket for communication
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

       -- We want to listen on all interfaces (0.0.0.0)
       bindAddr <- inet_addr "0.0.0.0"
       bindSocket sock (SockAddrInet 0 bindAddr)
    
       -- Save off the socket, and server address in a handle
       return $ (Switch (SocketHandle sock (addrAddress serveraddr)) [initialSeed] 
                 (Set.fromList [seedIPP])
                 False Map.empty Nothing Nothing [])
    where
       notify a = bracket_
                  (printf "Connecting to %s ... " initialSeed >> hFlush stdout)
                  (putStrLn "done.")
                  a

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
      
  (Just hostname,Just servicename) <- (getNameInfo [NI_NUMERICHOST] True True (addrAddress serveraddr))
  --putStrLn $ "resolve:" ++ (show hostname) ++ " " ++ (show servicename)
                           
  --return (serveraddr,port)         
  return (serveraddr,hostname,servicename)         
  
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
run :: TeleHash ()
run = do
  -- write "NICK" nick
  -- write "USER" (nick++" 0 * :tutorial bot")
  -- write "JOIN" chan
  --asks swH >>= dolisten
  gets swH >>= dolisten


-- ---------------------------------------------------------------------
  
pingSeeds :: TeleHash ()
pingSeeds = do

  seeds <- gets swSeeds
  connected <- gets swConnected
  
  io (putStrLn $ "pingSeeds:" ++ (show connected) ++ " " ++ (show seeds))

  case (not connected) && (seeds /= []) of
    True -> pingSeed $ head seeds
    False -> return ()

-- ---------------------------------------------------------------------
      
pingSeed :: String -> TeleHash ()
pingSeed seed = 
  do
    io (putStrLn $ "pingSeed " ++ (show seed))
    
    (serveraddr,ip,port) <- io (resolveToSeedIPP seed)
    
    --io (putStrLn $ "pingSeed serveraddr=" ++ (show serveraddr))
    
    let seedIPP = IPP (ip ++ ":" ++ port)
    io (putStrLn $ "pingSeed seedIPP=" ++ (show seedIPP))

    -- TODO: set self.seedsIndex[seedIPP] = true;
    state <- get
    put state {swSeedsIndex = Set.insert seedIPP (swSeedsIndex state) }
    
    timeNow <- io getClockTime
    
    line <- getOrCreateLine seedIPP timeNow
    let bootTelex = mkTelex seedIPP
    -- bootTelex["+end"] = line.end; // any end will do, might as well ask for their neighborhood
    let bootTelex' = bootTelex { teleSigEnd = Just $ (lineEnd line) }
  
    -- io (putStrLn $ "pingSeed telex=" ++ (show bootTelex'))
    
    -- io (putStrLn $ "sendMsg1:" ++ (show $ bootTelex'))
    -- io (putStrLn $ "sendMsg3:" ++ (show $ encodeMsg bootTelex'))

    socketh <- gets swH
    sendTelex bootTelex'
    
    return ()
   
-- ---------------------------------------------------------------------

getOrCreateLine :: IPP -> ClockTime -> TeleHash Line
getOrCreateLine seedIPP timeNow = do
  state <- get
  
  let 
    master = (swMaster state)
    
    endpointHash = mkHash seedIPP
    
    ismember = Map.member endpointHash master
    member = master Map.! endpointHash
    hashOk = (lineIpp member) == seedIPP
    line = if (ismember && hashOk) then (member) else (mkLine seedIPP timeNow)
    
    line' = line {lineNeighbors = Set.insert endpointHash (lineNeighbors line)}

    master' = Map.insert endpointHash line' master
    
    state' = state {swMaster = master'}
    
  put state'
  
  return line'
  
-- ---------------------------------------------------------------------
  
getLineMaybeM :: Hash -> TeleHash (Maybe Line)
getLineMaybeM hashIpp = do
  state <- get
  let master = (swMaster state)
  return $ getLineMaybe master hashIpp
  
getLineMaybe :: Map.Map Hash Line  -> Hash -> Maybe Line
getLineMaybe master hashIpp  = lineMaybe
  where
    ismember = Map.member hashIpp master
    member = master Map.! hashIpp
  
    lineMaybe = case (ismember) of
      True -> Just member
      False -> Nothing
  
-- ---------------------------------------------------------------------

addNeighbour :: Hash -> Hash -> TeleHash ()
addNeighbour hash end = do
  state <- get
  let master = swMaster state
  case (getLineMaybe master hash) of
    Just line -> do
      updateTelehashLine (line { lineNeighbors = Set.insert end (lineNeighbors line) })
      -- console.log(["\t\tSEED ", ipp, " into ", self.master[hash].ipp].join(""));
      io (putStrLn ("SEED " ++ (show hash) ++ " with " ++ (show end) ))
      return ()
    Nothing -> return ()
  
-- ---------------------------------------------------------------------


myNop :: TeleHash ()
myNop = do return ()

updateTelehashLine :: Line -> TeleHash ()
updateTelehashLine line = do
  state <- get
  
  let 
    master = (swMaster state)
    
    seedIPP = lineIpp line
    endpointHash = lineEnd line
    
    master' = Map.insert endpointHash line master
    
    state' = state {swMaster = master'}
    
  put state'

-- ---------------------------------------------------------------------

isLineOk :: Line -> Integer -> Telex -> Bool
isLineOk line secsNow msg = lineOk
  where
    timedOut =
      case (lineLineat line) of
        Just (TOD secs picos) -> secsNow - secs > 10 
        --Just (TOD secs picos) -> secsNow - secs > 60 
        Nothing -> False  
  
    msgLineOk = case (teleLine msg) of
      Just msgLineNum -> 
        case (lineLine line) of
          Just lineNum -> lineNum == msgLineNum && (lineNum `mod` (lineRingout line) == 0)
          Nothing -> True -- only test if we have a value
      Nothing -> True  
      
    lineOk = msgLineOk && not timedOut

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
    
checkLine :: Line -> Telex -> ClockTime -> (Bool, Line)
checkLine line msg timeNow@(TOD secsNow picosecsNow) = 
  let
    lineOk = isLineOk line secsNow msg
  
    line' = if lineOk 
              then case (lineLineat line) of
                Just _ -> line
                Nothing -> case (teleLine msg) of
                  Just msgLine ->
                    line {lineRingin = Just (msgLine `div` ((lineRingout line))),
                          lineLine   = teleLine msg,
                          lineLineat = Just timeNow
                         }
                  Nothing -> line
              else line
                   
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
                   
    valid = lineOk && ringOk
    

  -- TODO: *1. Update state with the new line value                          
  --       2. Return a true/false value as per the original             
  --       3. Do the rest of the processing, as required in original
  {-
    // we're valid at this point, line or otherwise, track bytes
    console.log([
        "\tBR ", line.ipp, " [", line.br, " += ",
        br, "] DIFF ", (line.bsent - t._br)].join(""));
    line.br += br;
    line.brin = t._br;
    
    // they can't send us that much more than what we've told them to, bad!
    if (line.br - line.brout > 12000) {
        return false;
    }
  -}
    msgLength = fromJust (teleMsgLength msg)
    
    line''' = if valid
                 then line'' { lineBr = (lineBr line'') + msgLength,
                               lineBrin = msgLength }
                 else line''
                    
    brOk = (lineBr line''') - (lineBrout line''') <= 12000
    
    
  -- updateTelehashLine line'''
  in
   (valid && brOk, line''' { lineSeenat = Just timeNow })
  

-- ---------------------------------------------------------------------

mkTelex :: IPP -> Telex
mkTelex seedIPP = 
    -- set _to = seedIPP
  -- Telex Nothing Nothing 0 (T.pack seedIPP) Nothing Nothing Nothing Map.empty -- Nothing
  Telex Nothing Nothing 0 seedIPP Nothing Nothing Nothing Nothing [] Nothing -- Map.empty -- Nothing
                  
-- ---------------------------------------------------------------------  
--
-- Process each line from the server
--
dolisten :: SocketHandle -> TeleHash ()
dolisten h = forever $ do
    -- TODO: move this pingSeeds call out to an actor, driven by a timer.
    pingSeeds
    
    -- TODO: is this a blocking call?
    (msg,rinfo) <- io (SB.recvFrom (slSocket h) 1000)

    io (putStrLn ("dolisten:rx msg=" ++ (BC.unpack msg)))
    -- io (putStrLn ("dolisten:rx=" ++ (show (parseTelex (BC.unpack msg)))))
    -- eval $ parseTelex (head $ BL.toChunks msg)
    recvTelex (BC.unpack msg) rinfo

  where
    forever a = a >> forever a

-- ---------------------------------------------------------------------  
--
-- Dispatch a command
--
eval :: Maybe Telex -> TeleHash ()
--eval     "!uptime"             = uptime >>= privmsg
--eval     "!quit"               = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
--eval x | "!id " `isPrefixOf` x = privmsg (drop 4 x)
eval     _                     = return () -- ignore everything else
--eval     _                     = io (exitWith ExitSuccess)
 
-- ---------------------------------------------------------------------  

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
      io (putStrLn "MAX SEND DROP ")
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
               -- TODO: this is horrible, must be a better type choice     
               -- else (msg {teleLine = Just $ T.pack $ show (fromJust (lineLine line))})
               else (msg {teleLine = Just $ (fromJust (lineLine line))})
                    
        -- update our bytes tracking and send current state
        -- telex._br = line.brout = line.br;
        msg'' = msg' { teleBr = lineBr line }
        
        -- var msg = new Buffer(JSON.stringify(telex), "utf8");
        --msgJson = encodeMsg msg''
        msgJson = encodeTelex msg''
    
        -- line.bsent += msg.length;
        -- line.sentat = time();
        line' = line { 
            lineBrout = lineBr line 
          , lineBsent = (lineBsent line) + (length msgJson)
          , lineSentat = Just timeNow            
          }       
        
      -- console.log(["SEND[", telex._to, "]\t", msg].join(""));
      io (putStrLn $ "sendTelexff:" ++ (show $ teleTo msg'') ++ " " ++ (msgJson))
                
      -- self.server.send(msg, 0, msg.length, line.port, line.host);
      socketh <- gets swH
      addr <- io (addrFromHostPort (lineHost line) (linePort line))
      io (sendDgram socketh msgJson addr)
      
      updateTelehashLine(line')
      
      --return ()
  

-- ---------------------------------------------------------------------  
       
-- encodeMsg msg = BC.unpack $ head $ BL.toChunks $ encode $ toJSON msg
encodeMsg :: Data a => a -> String
encodeMsg msg = encodeJSON msg


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
    -- io (putStrLn ("recvTelex:" ++  (show (parseTelex msg))))
    let
      rxTelex = parseTelex msg
    
    state <- get
    seedsIndex <- gets swSeedsIndex

    (Just hostIP,Just port) <- io (getNameInfo [NI_NUMERICHOST] True True rinfo)
    let
      remoteipp = IPP (hostIP ++ ":" ++ port)
    
    timeNow <- io getClockTime
    io (putStrLn ("recvTelex:remoteipp=" ++ (show remoteipp) ++ ",seedsIndex="++(show seedsIndex) 
                  ++ ",time=" ++ (show timeNow)))
    case (swConnected state) of
      False -> do
        -- TODO : test that _to field is set. Requires different setup for the JSON leg. 
        --        Why would it not be set? Also test that the _to value is us.
        -- // must have come from a trusted seed and have our to IPP info
        case (Set.member remoteipp seedsIndex ) of
          True -> online(rxTelex)
          False -> do
            io (putStrLn $ "recvTelex:we're offline and don't like that")
            return () -- TODO: no further processing. This is not a control return

      True -> do
        io (putStrLn $ "recvTelex:already online")


    -- // if this is a switch we know, check a few things
    line <- getOrCreateLine remoteipp timeNow
    let (lstat, line') = checkLine line rxTelex timeNow
    case lstat of
      False -> do
        io (putStrLn $ "LINE FAIL[" ++ (show line))
        myNop
      True -> do
        io (putStrLn $ "LINE STATUS " ++ (getLineStatus rxTelex))
        updateTelehashLine line'
        processCommands rxTelex remoteipp line'
        processSignals  rxTelex remoteipp line'

    return ()

-- ---------------------------------------------------------------------

-- TODO: implement this
processCommands rxTelex remoteipp line = do
  let ff = map (\x -> []) (getCommands rxTelex)
  return () 

getCommands telex = filter isCommand (teleRest telex)
  where
    isCommand (k,_v) = (head k) == '.'

-- ---------------------------------------------------------------------
    
processCommand ".see" remoteipp telex line = do 
  case (teleSee telex) of 
    Just seeipps -> mapM_ (\ipp -> processSee line remoteipp ipp) seeipps
    Nothing      -> return ()
  
processCommand _ _remoteipp _telex _line = do return ()
  
-- ---------------------------------------------------------------------

processSee :: Line -> String -> String -> TeleHash ()
processSee line remoteipp seeipp = do
  {-
        if (self.selfipp == seeipp) {
            // skip ourselves :)
            return; //continue;
        }
  -}      
  state <- get
  selfipp <- gets swSelfIpp
  
  {-
  if (selfipp == Just seeipp)
    then line
    else
    case (seeipp == remoteipp && not (lineVisible line)) of
      True ->
        io (putStrLn $ "VISIBLE " ++ (show remoteipp))
        line {lineVisible = True}
      False -> line
  -}
  seeVisible (seeipp == remoteipp && not (lineVisible line)) line selfipp remoteipp
  {-
        // they're making themselves visible now, awesome
        if (seeipp == remoteipp && !line.visible) {
            console.log(["\t\tVISIBLE ", remoteipp].join(""));
            line.visible=1;
            self.near_to(line.end, self.selfipp).map(function(x) { return line.neighbors[x]=1; });
            self.near_to(line.end, remoteipp); // injects this switch as hints into it's neighbors, fully seeded now
        }
  -}        
  {-
        var seeippHash = new Hash(seeipp).toString(); 
        
        if (self.master[seeippHash]) {
            return; //continue; // skip if we know them already
        }
        
        // XXX todo: if we're dialing we'd want to reach out to any of these closer to that $tap_end
        // also check to see if we want them in a bucket
        if (self.bucket_want(seeipp)) {
            
            // send direct (should open our outgoing to them)
            var telexOut = new Telex(seeipp);
            telexOut["+end"] = self.selfhash;
            self.send(telexOut);
            
            // send pop signal back to the switch who .see'd us in case the new one is behind a nat
            telexOut = new Telex(remoteipp);
            telexOut["+end"] = seeippHash;
            telexOut["+pop"] = "th:" + self.selfipp;
            telexOut["_hop"] = 1;
            self.send(telexOut);
        }
    });
  -}
  return ()
  

-- ---------------------------------------------------------------------

seeVisible False _line _selfipp _remoteipp = do return ()
seeVisible True  line  selfipp  remoteipp = do
  io (putStrLn $ "VISIBLE " ++ (show remoteipp))
  updateTelehashLine (line {lineVisible = True})
  {-
  // they're making themselves visible now, awesome
  if (seeipp == remoteipp && !line.visible) {
      console.log(["\t\tVISIBLE ", remoteipp].join(""));
      line.visible=1;
      self.near_to(line.end, self.selfipp).map(function(x) { return line.neighbors[x]=1; });
      self.near_to(line.end, remoteipp); // injects this switch as hints into it's neighbors, fully seeded now
  }
  -}        
  -- TODO: implement this ^^^^  ??
  return ()

-- ---------------------------------------------------------------------
{-
 * generate a .see for an +end, using a switch as a hint
 * -}

near_to :: IPP -> IPP -> TeleHash [Hash]
near_to end ipp = do
  {-
    var endHash = new Hash(end);
    var line = self.master[new Hash(ipp).toString()];
    if (!line) {
        return undefined; // should always exist except in startup or offline, etc
    }
  -}  
  state <- get

  let
    endHash = mkHash end
    master = (swMaster state)

  case (getLineMaybe master endHash) of
    Nothing -> return []
    Just line -> do
      let 
        {-    
        // of the existing and visible cached neighbors, sort by distance to this end
        var see = keys(line.neighbors)
        .filter(function(x){ return self.master[x] && self.master[x].visible })
        .sort(function(a,b){ return endHash.distanceTo(a) - endHash.distanceTo(b) });
        -}
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
            
      {-
      if (!see.length) {
          return undefined;
      }
    
      var firstSee = see[0];
      var firstSeeHash = new Hash(firstSee);
      var lineNeighborKeys = keys(line.neighbors);
      var lineEndHash = new Hash(line.end);
    
      console.log(["\tNEARTO ", end, '\t', ipp, '\t', 
          lineNeighborKeys.length, ">", see.length, '\t',
          firstSeeHash.distanceTo(end), "=", lineEndHash.distanceTo(end)].join(""));
       -}
        firstSee = head see      
        firstSeeHash = firstSee
        lineNeighborKeys = (lineNeighbors line)
        lineEndHash = lineEnd line
      io (putStrLn $ ("NEARTO " ++ (show end) ++ "\t" ++ (show ipp) ++ "\t" ++ (show $ Set.size lineNeighborKeys) 
          ++ ">" ++ (show $ length see)
          ++ (show $ distanceTo firstSeeHash endHash) ++ "=" ++ (show $ distanceTo lineEndHash endHash)))
      {-        
      // it's either us or we're the same distance away so return these results
      if (firstSee == line.end
              || (firstSeeHash.distanceTo(end) == lineEndHash.distanceTo(end))) {
      -}
      case (firstSee == (lineEnd line) 
            || ((distanceTo firstSeeHash endHash) == (distanceTo lineEndHash endHash))) of
        True -> 
          {-  
          // this +end == this line then replace the neighbors cache with this result 
          // and each in the result walk and insert self into their neighbors
          if (line.end == end) {
          -}
          case ((lineIpp line) == end) of
            True -> do
              -- console.log(["\tNEIGH for ", end, " was ", lineNeighborKeys.join(","), " ", see.length].join(""));
              io (putStrLn ("NEIGH for " ++ (show end) ++ " was " ++ (show lineNeighborKeys) ++ (show $ length see)))
              {-
              var neigh = {};
              see.slice(0,5).forEach(function(seeHash){
                  neigh[seeHash] = 1;
              });
              line.neighbors = neigh;
              -}
              let neigh = Set.fromList $ take 5 see
              updateTelehashLine (line {lineNeighbors = neigh})
              -- console.log(["\tNEIGH for ", end, " is ", lineNeighborKeys.join(","), " ", see.length].join(""));
              io (putStrLn ("NEIGH for " ++ (show end) ++ " is " ++ (show neigh) ++ (show $ length see)))

              {-
              lineNeighborKeys.forEach(function(hash) {
                  if (hash in self.master) {
                      if (self.master[hash].neighbors == null) {
                          self.master[hash].neighbors = {};
                      }
                      self.master[hash].neighbors[end]=1;
                      console.log(["\t\tSEED ", ipp, " into ", self.master[hash].ipp].join(""));
                  }
              });
              -}
              mapM_ (\hash -> addNeighbour hash endHash) $ Set.toList neigh
              -- console.log(["\t\tSEE distance=", endHash.distanceTo(firstSeeHash), " count=", see.length].join(""));
              io (putStrLn ("SEE distance=" ++ (show $ distanceTo endHash firstSeeHash) ++ " count=" ++ (show $ length see) ))
              return see
            False -> do
              -- console.log(["\t\tSEE distance=", endHash.distanceTo(firstSeeHash), " count=", see.length].join(""));
              io (putStrLn ("SEE distance=" ++ (show $ distanceTo endHash firstSeeHash) ++ " count=" ++ (show $ length see) ))
              return see -- TODO:plug in bit below
              {-
              }
              console.log(["\t\tSEE distance=", endHash.distanceTo(firstSeeHash), " count=", see.length].join(""));
              return see;
             -}
        False -> do
          -- // whomever is closer, if any, tail recurse endseeswitch them
          -- return self.near_to(end, self.master[firstSee].ipp);
          
          Just lineFirstSee <- getLineMaybeM firstSee
          near_to end (lineIpp lineFirstSee)


-- ---------------------------------------------------------------------

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
    go acc (-1:[]) = -1
    go acc (-1:xs) = go (acc - 4) xs
    go acc (x:xs) = acc + x 
    
    diffs = map (\(a,b) -> sbtab !! (xor (digitToInt a) (digitToInt b))) $ zip this h
    sbtab = [-1,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3]
                               
-- ---------------------------------------------------------------------

-- TODO: implement this
processSignals rxTelex remoteipp line = do return ()

getSignals telex = filter isSignal (teleRest telex)
  where
    isSignal (k,_v) = (head k) == '+'

-- ---------------------------------------------------------------------

-- TODO: implement this
online rxTelex = do 
                    
  io (putStrLn $ "ONLINE")
  
  
  --  self.connected = true;
  state <- get
  put $ state {swConnected = True}
  
  let
  --  self.selfipp = telex._to;
    selfIpp = (teleTo rxTelex)
  --  self.selfhash = new Hash(self.selfipp).toString();
    selfhash = mkHash $ teleTo rxTelex
  --  console.log(["\tSELF[", telex._to, " = ", self.selfhash, "]"].join(""));
  io (putStrLn ("SELF[" ++ (show selfIpp) ++ " = " ++ (show selfhash) ++ "]"))
    
  --  var line = self.getline(self.selfipp);
  timeNow <- io getClockTime
  line <- getOrCreateLine selfIpp timeNow

  --  line.visible = 1; // flag ourselves as default visible
  --  line.rules = self.taps; // if we're tap'ing anything
  
  taps <- gets swTaps
  updateTelehashLine (line {lineVisible = True, lineRules = taps })
    
  --  // trigger immediate tapping to move things along
  --  self.taptap();
  taptap
  
  return ()
                    
                    
-- ---------------------------------------------------------------------
  
-- TODO: implement this  
taptap :: TeleHash ()
taptap = do return ()
  
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

-- From http://hackage.haskell.org/packages/archive/thespian/0.9/doc/html/Control-Concurrent-Actor.html

act1 :: A.Actor Int Int
act1 = forever $ do
    (num, addr) <- A.receive
    liftIO . putStrLn $ "act1: received " ++ (show num)
    A.send addr (num + 1)

act2 :: Int -> A.Address Int Int -> A.Actor Int Int
act2 n0 addr = do
    A.send addr n0
    forever $ do
        (num, addr1) <- A.receive
        liftIO . putStrLn $ "act2: received " ++ (show num)
        A.send addr1 (num + 1)

main_actor = do
    addr1 <- A.spawn act1
    addr2 <- A.spawn $ act2 0 addr1
    threadDelay 20000000
    
-- ---------------------------------------------------------------------


-- EOF