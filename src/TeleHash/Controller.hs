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
       , encodeMsg  
       , isLineOk
       , isRingOk  
       , mkLine  
       , checkLine  
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

data Switch = Switch { swH :: SocketHandle 
                     , swSeeds :: [String]
                     , swSeedsIndex :: Set.Set String
                     , swConnected :: Bool
                     , swMaster :: Map.Map Hash Line  
                     , swSelfIpp :: Maybe String  
                     , swSelfHash :: Maybe String
                     , swTaps :: [Tap]  
                     } deriving (Eq,Show)

data SocketHandle = 
    SocketHandle {slSocket :: Socket,
                  slAddress :: SockAddr
                 } deriving (Eq,Show)

data Line = Line {
  lineIpp       :: String, -- IP and port of the destination
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
mkLine :: String -> ClockTime -> Line
mkLine endPointStr timeNow =
  let
    [hostIP,port] = split ":" endPointStr
    endPointHash = mkHash endPointStr
    (TOD secs picosecs) = timeNow
    ringOut = fromIntegral (1 + (picosecs `mod` 32768))  -- TODO: rand 1..32768
  in  
   Line {
       lineIpp       = endPointStr,
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
             , teleTo     :: String
             , teleLine   :: Maybe Int
             , teleHop    :: Maybe String
             , teleSigEnd :: Maybe Hash
             , teleTap    :: Maybe [Tap]  
             -- , teleRest   :: Maybe (Map.Map T.Text Value)
             -- , teleRest   :: (Map.Map T.Text Value)
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
     
     (mkTelex to) {teleRing = maybeRing, teleSee = maybeSee, teleBr = br, 
                   teleTo = to, teleLine = maybeLine, teleHop = maybeHop,
                   teleSigEnd = maybeEnd, teleTap = maybeTap, 
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
    getTap (JSObject jsobj) = foldl dTap (Tap ("","") []) $ fromJSObject jsobj

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
    to   = [("_to",   JSString $ toJSString $ teleTo t)]
    
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

       -- Establish a socket for communication
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

       -- We want to listen on all interfaces (0.0.0.0)
       bindAddr <- inet_addr "0.0.0.0"
       bindSocket sock (SockAddrInet 0 bindAddr)
    
       -- Save off the socket, and server address in a handle
       return $ (Switch (SocketHandle sock (addrAddress serveraddr)) [initialSeed] 
                 (Set.fromList [initialSeed])
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
    
    let seedIPP = ip ++ ":" ++ port
    io (putStrLn $ "pingSeed seedIPP=" ++ (show seedIPP))

    -- TODO: set self.seedsIndex[seedIPP] = true;
    state <- get
    put state {swSeedsIndex = Set.insert seedIPP (swSeedsIndex state) }
    
    timeNow <- io getClockTime
    
    line <- getTelehashLine seedIPP timeNow
    let bootTelex = mkTelex seedIPP
    -- bootTelex["+end"] = line.end; // any end will do, might as well ask for their neighborhood
    let bootTelex' = bootTelex { teleSigEnd = Just $ (lineEnd line) }
  
    io (putStrLn $ "pingSeed telex=" ++ (show bootTelex'))
    
    -- io (putStrLn $ "sendMsg1:" ++ (show $ bootTelex'))
    -- io (putStrLn $ "sendMsg3:" ++ (show $ encodeMsg bootTelex'))

    socketh <- gets swH
    sendTelex bootTelex'
    
    return ()
   
-- ---------------------------------------------------------------------

getTelehashLine :: String -> ClockTime -> TeleHash Line
getTelehashLine seedIPP timeNow = do
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

mkTelex :: String -> Telex
mkTelex seedIPP = 
    -- set _to = seedIPP
  -- Telex Nothing Nothing 0 (T.pack seedIPP) Nothing Nothing Nothing Map.empty -- Nothing
  Telex Nothing Nothing 0 seedIPP Nothing Nothing Nothing Nothing Nothing -- Map.empty -- Nothing
                  
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

    io (putStrLn ("dolisten:rx msg=" ++ (show msg)))
    io (putStrLn ("dolisten:rx=" ++ (show (parseTelex (BC.unpack msg)))))
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
  line <- getTelehashLine (teleTo msg) timeNow
  
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

-- Dispatch incoming raw messages

recvTelex :: String -> SockAddr -> TeleHash ()
recvTelex msg rinfo = do
    io (putStrLn ("recvTelex:" ++  (show (parseTelex msg))))
    let
      rxTelex = parseTelex msg
    
    state <- get
    seedsIndex <- gets swSeedsIndex

    -- if (!self.connected)
    -- {
    --     // must have come from a trusted seed and have our to IPP info
    --     if(self.seedsIndex[remoteipp] && "_to" in telex)
    --     {
    --         self.online(telex);
    --     }else{
    --         console.log("we're offline and don't like that");
    --         return;
    --     }
    -- }
    (Just hostIP,Just port) <- io (getNameInfo [NI_NUMERICHOST] True True rinfo)
    let
      remoteipp = hostIP ++ ":" ++ port

    io (putStrLn ("recvTelex:remoteipp=" ++ remoteipp ++ ",seedsIndex="++(show seedsIndex)))
    case (swConnected state) of
      False -> do
        -- TODO : test that _to field is set. Requires different setup for the JSON leg. 
        --        Why would it not be set? Also test that the _to value is us.
        case (Set.member remoteipp seedsIndex ) of
          True -> online(rxTelex)
          False -> do
            io (putStrLn $ "recvTelex:we're offline and don't like that")
            return () -- TODO: no further processing. This is not a control return

      True -> do
        io (putStrLn $ "recvTelex:already online")


    -- // if this is a switch we know, check a few things
    -- var line = self.getline(remoteipp, telex._ring);
    timeNow <- io getClockTime
    line <- getTelehashLine remoteipp timeNow
    -- var lstat = self.checkline(line, telex, msgstr.length);
    let (lstat, line') = checkLine line rxTelex timeNow
        
    -- if (!lstat) {
    --     console.log(["\tLINE FAIL[", JSON.stringify(line), "]"].join(""));
    --     return;
    -- } else {
    --     console.log(["\tLINE STATUS ", (telex._line ? "OPEN":"RINGING")].join(""));
    -- }

    -- TODO: update the line state monad, if ok
    -- TODO: process the line commands, if ok

    return ()

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
  line <- getTelehashLine selfIpp timeNow

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

mkHash :: String -> Hash
mkHash str =
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