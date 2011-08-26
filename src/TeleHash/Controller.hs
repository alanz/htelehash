{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonoPatBinds #-}

-- Above three settings are for the Json stuff using Data.Iso

module TeleHash.Controller 
       (
       -- For testing
       recvTelex  
       , parseTeleHashEntry
       ) where

import Control.Category
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Exception 
import Data.Attoparsec
import Data.Aeson (Object,json,Value(..),encode)
import Data.Bits
import Data.Char
-- import Data.Iso
import Data.List
import Data.Maybe
import Data.String.Utils
--import Language.JsonGrammar
import Network.BSD
import Network.Socket
import Numeric
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

data Switch = Switch { swH :: SocketHandle 
                     , swSeeds :: [String]
                     , swSeedsIndex :: Set.Set String
                     , swConnected :: Bool
                     , swMaster :: Map.Map String Line  
                     , swSelfIpp :: Maybe String  
                     , swSelfHash :: Maybe String
                     , swTaps :: [String]  
                     }

data SocketHandle = 
    SocketHandle {slSocket :: Socket,
                  slAddress :: SockAddr}

data Line = Line {
  lineIpp       :: String, -- IP and port of the destination
  lineEnd       :: String, -- Hash of the ipp (endpoint)
  lineHost      :: String, -- Split out host IP
  linePort      :: String, -- Split out port
  lineRingout   :: Int,    --  rand(32768),
  lineRingin    :: Maybe Int, 
  lineLine      :: Maybe Int, -- When line is live, product of our ringout and their ringin
  lineInit      :: ClockTime,
  lineSeenat    :: ClockTime,
  lineSentat    :: ClockTime,
  lineLineat    :: Maybe ClockTime,
  lineBr        :: Int,
  lineBrout     :: Int,
  lineBrin      :: Int,
  lineBsent     :: Int,
  lineNeighbors :: Set.Set String, -- lineNeighbors,
  lineVisible   :: Bool,
  lineRules     :: [String]
  }

mkLine endPointStr timeNow =
  let
    [hostname,port] = split ":" endPointStr
    endPointHash = mkHash endPointStr
    (TOD secs picosecs) = timeNow
    ringOut = fromIntegral (1 + (picosecs `mod` 32768))  -- TODO: rand 1..32768
  in  
   Line {
       lineIpp       = endPointStr,
       lineEnd       = endPointHash,
       lineHost      = hostname,
       linePort      = port,
       lineRingout   = ringOut,
       lineRingin    = Nothing,
       lineLine      = Nothing,
       lineInit      = timeNow,
       lineSeenat    = undefined,
       lineSentat    = undefined,
       lineLineat    = undefined,
       lineBr        = 0,
       lineBrout     = 0,
       lineBrin      = 0,
       lineBsent     = 0,
       lineNeighbors = Set.fromList [endPointHash],
       lineVisible   = False,
       lineRules     = []                       
       }

-- ---------------------------------------------------------------------

-- JSON stuff for a TeleHashEntry
-- TODO: merge in stuff from Json.hs and get rid of that file, or refactor
-- See https://github.com/MedeaMelana/JsonGrammar for examples/howto


data TeleHashEntry = TeleHashEntry 
                     { teleRing   :: Maybe Int
                     , teleSee    :: Maybe [T.Text]
                     , teleBr     :: Int
                     , teleTo     :: T.Text
                     , teleLine   :: Maybe Int
                     , teleHop    :: Maybe T.Text
                     , teleSigEnd :: Maybe T.Text  
                     -- , teleRest   :: Maybe (Map.Map T.Text Value)
                     , teleRest   :: (Map.Map T.Text Value)
                     } deriving (Eq, Show)


{-
telexJson = $(deriveIsos ''TeleHashEntry)

instance Json TeleHashEntry where
  grammar = telexJson . object
    ( optionalProp "_ring"
    . optionalProp ".see"
    . prop "_br"
    . prop "_to"
    . optionalProp "_line"
    . optionalProp "_hop"
    . optionalProp "+end"
    . rest
    -- . optionalRest
    )

optionalProp :: Json a => String -> Iso (Object :- t) (Object :- Maybe a :- t)
optionalProp name = duck just . prop name <> duck nothing

optionalRest :: Iso (Object :- t) (Object :- Maybe (Map.Map T.Text Value) :- t)
optionalRest = duck just . rest <> duck nothing
-}
  
-- TODO : pick up error and deal with it, below
parseAll :: BC.ByteString -> Value
parseAll s = case (parse json s) of
  Done _  r -> r
  _         -> Null

parseTeleHashEntry :: BC.ByteString -> Maybe TeleHashEntry
parseTeleHashEntry s = fromJson $ parseAll s

test_parseTeleHashEntry = parseTeleHashEntry _inp

_inp = BC.pack ("{\"_ring\":17904," ++
       "\".see\":[ \"208.68.163.247:42424\", \"208.68.160.25:55137\"]," ++ 
       "\"_br\":52," ++ 
       "\"_to\":\"173.19.99.143:63681\" }")

_inp2 = BC.pack "{\"_to\":\"208.68.163.247:42424\",\"+end\":\"9fa23aa9f9ac828ced5a151fedd24af0d9fa8495\",\".see\":[\"196.215.128.240:51602\"],\".tap\":[{\"is\":{\"+end\":\"9fa23aa9f9ac828ced5a151fedd24af0d9fa8495\"},\"has\":[\"+pop\"]}],\"_line\":35486388,\"_br\":174}"

_inp3 = BC.pack "{\"+end\":\"9fa23aa9f9ac828ced5a151fedd24af0d9fa8495\",\"_to\":\"208.68.163.247:42424\",\".see\":[\"196.215.128.240:51602\"],\".tap\":[{\"is\":{\"+end\":\"9fa23aa9f9ac828ced5a151fedd24af0d9fa8495\"},\"has\":[\"+pop\"]}],\"_line\":35486388,\"_br\":174}"

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
    let bootTelex' = bootTelex { teleSigEnd = Just $ T.pack $ (lineEnd line) }
    -- line = undefined
  
    io (putStrLn $ "pingSeed telex=" ++ (show bootTelex'))
    
    -- io (putStrLn $ "sendMsg:" ++ (show $ BC.unpack $ head $ BL.toChunks $ encode $ toJson bootTelex))
    
    io (putStrLn $ "sendMsg:" ++ (show $ bootTelex))
    io (putStrLn $ "sendMsg:" ++ (show $ toJson bootTelex))
    io (putStrLn $ "sendMsg:" ++ (show $ encodeMsg bootTelex))

    -- TODO: call sendTelex instead, which manages/reuses lines on the way
    socketh <- gets swH
    --io (sendMsg socketh bootTelex)
    sendTelex bootTelex
    
    return ()
   
-- ---------------------------------------------------------------------

getTelehashLine :: String -> ClockTime -> TeleHash Line
getTelehashLine seedIPP timeNow = do
  state <- get
  
  let 
    master = (swMaster state)
    
    ismember = Map.member seedIPP master
    member = master Map.! seedIPP 
    hashOk = (lineIpp member) == seedIPP
    line = if (ismember && hashOk) then (member) else (mkLine seedIPP timeNow)
    
    line' = line {lineNeighbors = Set.insert seedIPP (lineNeighbors line)}

    master' = Map.insert seedIPP line' master
    
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
    
    master' = Map.insert seedIPP line master
    
    state' = state {swMaster = master'}
    
  put state'

-- ---------------------------------------------------------------------

checkLine line msg timeNow@(TOD secsNow picosecsNow) = do
  -- // first, if it's been more than 10 seconds after a line opened, 
  -- // be super strict, no more ringing allowed, _line absolutely required
  -- if (line.lineat > 0 && time() - line.lineat > 10) {
  --     if (t._line != line.line) {
  --         return false;
  --     }
  -- }
  
  let
    timedOut =
      case (lineLineat line) of
        Just (TOD secs picos) -> secsNow - secs > 10 
        Nothing -> False  
  
    lineEstablished = case (lineLine line) of
      Just lineNum -> case (teleLine msg) of
        Just msgLineNum -> lineNum == msgLineNum
        Nothing -> False  
      Nothing -> False
  
    lineBad = timedOut && not lineEstablished
    
  --  // second, process incoming _line
  --  if (t._line) {
    lineOk = case (teleLine msg) of  
      Just lineNum -> if not timedOut && lineEstablished  
                      then (lineNum `mod` (lineRingout line) == 0)
                      else False
  --      if (line.ringout <= 0) {
  --          return false;
  --      }
  --      
  --      // be nice in what we accept, strict in what we send
  --      t._line = parseInt(t._line);
  --      
  --      // must match if exist
  --      if (line.line && t._line != line.line) {
  --          return false;
  --      }
  --      
  --      // must be a product of our sent ring!!
  --      if (t._line % line.ringout != 0) {
  --          return false;
  --      }
  --      
  --      // we can set up the line now if needed
  --      if(line.lineat == 0) {
  --          line.ringin = t._line / line.ringout; // will be valid if the % = 0 above
  --          line.line = t._line;
  --          line.lineat = time();
  --      }
  --  }
      Nothing -> False    

    line' = if lineOk 
              then case (lineLineat line) of
                Just _ -> line
                Nothing -> line {lineRingin = Just ((fromJust $ teleLine msg) `div` ((lineRingout line))),
                                 lineLine   = (teleLine msg),             
                                 lineLineat = Just timeNow
                                }
              else line
                   
  -- TODO: propagate line' to State.  
                   
  --  // last, process any incoming _ring's (remember, could be out of order, after a _line)
  --  if (t._ring) {
    line'' = case (teleRing msg) of
               Just msgRing -> 
                 let
                   msgRingin = (fromMaybe 0 (teleRing msg))
                   ringinOk = case (lineRingin line) of 
                     Just ri -> (ri == msgRingin) && (msgRingin > 0) && (msgRingin <= 32768)
                     Nothing -> True
                   in
                     if (not ringinOk) then line'
                       else if ((lineLineat line) /= Nothing) then line'
                            else line' {lineRingin = Just msgRingin,
                                        lineLine = Just (msgRingin * (lineRingout line)),
                                        lineLineat = Just timeNow
                                       }
                   
  --      // already had a ring and this one doesn't match, should be rare
  --      if (line.ringin && t._ring != line.ringin) {
  --          return false;
  --      }
  --      
  --      // make sure within valid range
  --      if (t._ring <= 0 || t._ring > 32768) {
  --          return false;
  --      }
  --      
  --      // we can set up the line now if needed
  --      if (line.lineat == 0) {
  --          line.ringin = t._ring;
  --          line.line = line.ringin * line.ringout;
  --          line.lineat = time();
  --      }
  --  }
               Nothing -> line'
                   
  -- TODO: 1. Update state with the new line value                          
  --       2. Return a true/false value as per the original             
  return ()

-- ---------------------------------------------------------------------

mkTelex :: String -> TeleHashEntry
mkTelex seedIPP = 
    -- set _to = seedIPP
  TeleHashEntry Nothing Nothing 0 (T.pack seedIPP) Nothing Nothing Nothing Map.empty -- Nothing
                  
-- ---------------------------------------------------------------------  
--
-- Process each line from the server
--
dolisten :: SocketHandle -> TeleHash ()
dolisten h = {- forever $ -} do
    pingSeeds
    
    -- msg <- io (SB.recv (slSocket h) 1000)
    (msg,rinfo) <- io (SB.recvFrom (slSocket h) 1000)

    io (putStrLn $ show msg)
    io (putStrLn ("dolisten:rx msg=" ++ (show msg)))
    --io (putStrLn (show (parseTeleHashEntry (head $ BL.toChunks msg))))
    io (putStrLn ("dolisten:rx=" ++ (show (parseTeleHashEntry msg))))
    -- eval $ parseTeleHashEntry (head $ BL.toChunks msg)
    recvTelex msg rinfo

  where
    forever a = a >> forever a

-- ---------------------------------------------------------------------  
--
-- Dispatch a command
--
eval :: Maybe TeleHashEntry -> TeleHash ()
--eval     "!uptime"             = uptime >>= privmsg
--eval     "!quit"               = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
--eval x | "!id " `isPrefixOf` x = privmsg (drop 4 x)
eval     _                     = return () -- ignore everything else
--eval     _                     = io (exitWith ExitSuccess)
 
-- ---------------------------------------------------------------------  

sendTelex :: TeleHashEntry -> TeleHash ()
sendTelex msg = do 
  timeNow <- io getClockTime
  line <- getTelehashLine (T.unpack (teleTo msg)) timeNow
  
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
        msgJson = encodeMsg msg''
    
        -- line.bsent += msg.length;
        -- line.sentat = time();
        line' = line { 
            lineBrout = lineBr line 
          , lineBsent = (lineBsent line) + (length msgJson)
          , lineSentat = timeNow            
          }       
        
      -- console.log(["SEND[", telex._to, "]\t", msg].join(""));
      io (putStrLn $ "sendTelex:" ++ (show $ teleTo msg'') ++ " " ++ (msgJson))
                
      -- self.server.send(msg, 0, msg.length, line.port, line.host);
      socketh <- gets swH
      addr <- io (addrFromHostPort (lineHost line) (linePort line))
      io (sendDgram socketh msgJson addr)
      
      updateTelehashLine(line')
      
      --return ()
  

-- ---------------------------------------------------------------------  
       
encodeMsg msg = BC.unpack $ head $ BL.toChunks $ encode $ toJson msg

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

sendMsg :: Json a => SocketHandle -> a -> IO ()
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
recvTelex :: BC.ByteString -> SockAddr -> TeleHash ()
recvTelex msg rinfo = do
    -- io (putStrLn $ show msg)
    io (putStrLn (show (parseTeleHashEntry msg)))
    let
      Just rxTelex = parseTeleHashEntry msg
    
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
    (Just hostname,Just port) <- io (getNameInfo [NI_NUMERICHOST] True True rinfo)
    let
      remoteipp = hostname ++ ":" ++ port

    io (putStrLn ("recvTelex:remoteipp=" ++ remoteipp ++ ",seedsIndex="++(show seedsIndex)))
    case (swConnected state) of
      False -> do
        -- TODO : test that _to field is set. Requires different setup for the JSON leg. 
        --        Why would it not be set?
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
    lstat <- checkLine line rxTelex timeNow
    -- if (!lstat) {
    --     console.log(["\tLINE FAIL[", JSON.stringify(line), "]"].join(""));
    --     return;
    -- } else {
    --     console.log(["\tLINE STATUS ", (telex._line ? "OPEN":"RINGING")].join(""));
    -- }


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
    selfhash = mkHash  $ T.unpack (teleTo rxTelex)
  --  console.log(["\tSELF[", telex._to, " = ", self.selfhash, "]"].join(""));
  io (putStrLn ("SELF[" ++ (show selfIpp) ++ " = " ++ selfhash ++ "]"))
    
  --  var line = self.getline(self.selfipp);
  timeNow <- io getClockTime
  line <- getTelehashLine (T.unpack selfIpp) timeNow

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

mkHash str =
  let
    digest = SHA.sha1 $ BL.fromChunks [BC.pack str]
  in  
   -- B64.encode $ BL.unpack $ SHA.bytestringDigest digest
   show digest


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