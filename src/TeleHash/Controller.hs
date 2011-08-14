{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonoPatBinds #-}

-- Above three settings are for the Json stuff using Data.Iso

-- import Control.Exception -- for base-3, with base-4 use Control.OldException
import Codec.Binary.Base64 as B64
import Control.Category
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.OldException
import Data.Attoparsec
import Data.Aeson (Object,json,Value(..))
import Data.Bits
import Data.Char
import Data.Iso
import Data.List
import Data.String.Utils
import Language.JsonGrammar
import Network.BSD
import Network.Socket 
import Numeric
import Prelude hiding (id, (.), head, either, catch)
import System.Exit
import System.IO
import System.Time
--import TeleHash.Json 
import Text.Printf
import qualified Control.Concurrent.Actor as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Text as T
import qualified Network.Socket.ByteString.Lazy as SL
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
type TeleHash = ReaderT Switch IO
data Switch = Switch { swH :: SocketHandle 
                     , swSeeds :: [String]
                     }

data SocketHandle = 
    SocketHandle {slSocket :: Socket,
                  slAddress :: SockAddr}

data Line = Line {
  lineIpp       :: String, -- IP and port of the destination
  lineEnd       :: String, -- Hash of the ipp (endpoint)
  lineHost      :: String, -- Split out host IP
  linePort      :: String, -- Split out port
  lineRingout   :: Int, --  rand(32768),
  lineInit      :: ClockTime,
  lineSeenat    :: ClockTime,
  lineSentat    :: ClockTime,
  lineLineat    :: ClockTime,
  lineBr        :: Int,
  lineBrout     :: Int,
  lineBrin      :: Int,
  lineBsent     :: Int,
  lineNeighbors :: [String], -- lineNeighbors,
  lineVisible   :: Bool
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
       lineInit      = timeNow,
       lineSeenat    = undefined,
       lineSentat    = undefined,
       lineLineat    = undefined,
       lineBr        = 0,
       lineBrout     = 0,
       lineBrin      = 0,
       lineBsent     = 0,
       lineNeighbors = [endPointHash],
       lineVisible   = False
       }

-- ---------------------------------------------------------------------

-- JSON stuff for a TeleHashEntry
-- TODO: merge in stuff from Json.hs and get rid of that file, or refactor
-- See https://github.com/MedeaMelana/JsonGrammar for examples/howto

{-
person = $(deriveIsos ''Person)
(male, female) = $(deriveIsos ''Gender)
coords = $(deriveIsos ''Coords)
-}

data TeleHashEntry = TeleHashEntry 
                     { teleRing :: Int
                     , teleSee  :: Maybe [T.Text]
                     , teleBr   :: Int
                     , teleTo   :: T.Text
                     -- , teleLine :: Maybe T.Text
                     -- , teleHop  :: Maybe T.Text
                     } deriving (Eq, Show)


telexJson = $(deriveIsos ''TeleHashEntry)

instance Json TeleHashEntry where
  grammar = telexJson . object
    ( prop "_ring"
    . prop ".see"
    . prop "_br"
    . prop "_to"
    -- . prop "_line"
    -- . prop "_hop"
    )

  
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
main :: IO ()
main = bracket initialize disconnect loop
  where
    disconnect ss = sClose (slSocket (swH ss))
    loop st    = catch (runReaderT run st) (const $ return ())

-- ---------------------------------------------------------------------
-- Hardcoded params for now    
initialSeed = "telehash.org:42424"

initialize :: IO Switch
initialize =
    notify $ do -- Look up the hostname and port.  Either raises an exception
       -- or returns a nonempty list.  First element in that list
       -- is supposed to be the best option.
       let [hostname,port] = split ":" initialSeed
       addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
       let serveraddr = head addrinfos

       -- Establish a socket for communication
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

       -- We want to listen on all interfaces (0.0.0.0)
       bindAddr <- inet_addr "0.0.0.0"
       bindSocket sock (SockAddrInet 0 bindAddr)
    
       -- Save off the socket, and server address in a handle
       return $ Switch (SocketHandle sock (addrAddress serveraddr)) [initialSeed]
    where
       notify a = bracket_
                  (printf "Connecting to %s ... " initialSeed >> hFlush stdout)
                  (putStrLn "done.")
                  a

--
-- We're in the Switch monad now, so we've connected successfully
-- Start processing commands
--
run :: TeleHash ()
run = do
  -- write "NICK" nick
  -- write "USER" (nick++" 0 * :tutorial bot")
  -- write "JOIN" chan
  asks swH >>= dolisten

 
pingSeeds sw 
  | swConnected || swSeeds sw == [] = []
  | otherwise = pingSeed $ head (swSeeds sw)
    where
      swConnected = undefined
      
pingSeed seedIPP = 
  let
    line = getTelehashLine seedIPP
    bootTelex = mkTelex seedIPP
    -- line = undefined
  in
    undefined
   
-- ---------------------------------------------------------------------
-- TODO: Move this into the TeleHash monad, to access the stored lines
getTelehashLine :: String -> IO Line
getTelehashLine seedIPP = do
  -- TODO: check for existing line in some sort of storage first
  timeNow <- getClockTime
  let line = mkLine seedIPP timeNow
  return line
  
-- ---------------------------------------------------------------------

mkTelex seedIPP = undefined
    -- set _to = seedIPP
-- ---------------------------------------------------------------------  
--
-- Process each line from the server
--
dolisten :: SocketHandle -> TeleHash ()
dolisten h = {- forever $ -} do
    io (sendmsg h "blah")
    msg <- io (SL.recv (slSocket h) 1000)

    io (putStrLn $ show msg)
    io (putStrLn (show (parseTeleHashEntry (head $ BL.toChunks msg))))
    eval $ parseTeleHashEntry (head $ BL.toChunks msg)

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
       
sendmsg :: SocketHandle -> String -> IO ()
sendmsg socketh msg =
    sendstr sendmsg
    where code = 4 --makeCode fac pri
          sendmsg = "{\"+end\":\"3b6a6...\"}"

          -- Send until everything is done
          sendstr :: String -> IO ()
          sendstr [] = return ()
          sendstr omsg = do sent <- sendTo (slSocket socketh) omsg (slAddress socketh)
                            sendstr (genericDrop sent omsg)
          
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
   B64.encode $ BL.unpack $ SHA.bytestringDigest digest


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
