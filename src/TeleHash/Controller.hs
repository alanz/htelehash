import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
-- import Control.Exception -- for base-3, with base-4 use Control.OldException
import Control.OldException
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import qualified Data.Digest.SHA1 as SHA1
import Data.Bits
import Data.Char
import Codec.Binary.Base64 as B64
import Data.List
import Data.String.Utils
--import Data.Word
import Network.BSD
import Network.Socket 
import Numeric
import Prelude hiding (catch)
import System.Exit
import System.IO
import qualified System.Random as R
import System.Time
import TeleHash.Json 
import Text.Printf
import qualified Control.Concurrent.Actor as A
import qualified Data.ByteString.Lazy as BL
import qualified Network.Socket.ByteString.Lazy as SL

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
  ipp       :: String, -- IP and port of the destination
  end       :: String, -- Hash of the ipp (endpoint)
  host      :: String, -- Split out host IP
  port      :: String, -- Split out port
  ringout   :: Int, --  rand(32768),
  init      :: ClockTime,
  seenat    :: ClockTime,
  sentat    :: ClockTime,
  lineat    :: ClockTime,
  br        :: String,
  brout     :: String,
  brin      :: String,
  bsent     :: String,
  neighbors :: [String], -- lineNeighbors,
  visible   :: Bool
  }

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
      
pingSeed = undefined
  
--
-- Process each line from the server
--
dolisten :: SocketHandle -> TeleHash ()
dolisten h = {- forever $ -} do
    io (sendmsg h "blah")
    msg <- io (SL.recv (slSocket h) 1000)

    io (putStrLn (show (parseAll (head $ BL.toChunks msg))))
    eval $ parseAll (head $ BL.toChunks msg)
  where
    forever a = a >> forever a

-- ---------------------------------------------------------------------  
--
-- Dispatch a command
--
eval :: [TeleHashEntry] -> TeleHash ()
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
 * @constructor
 */
function Hash(value) {
    if (value != undefined) {
        var hashAlgorithm = crypto.createHash("SHA1");
        hashAlgorithm.update(value);
        this.digest = new Buffer(hashAlgorithm.digest("base64"), "base64");
    }
}
-}
mkHash str =
  let
    w160v = SHA1.hash $ B.unpack $ BC.pack str
  in  
   -- toHex $ toSwappedInteger w160v 
   -- B64.encode $ B.unpack $ BC.pack "foo"
   toHex $ SHA1.toInteger w160v 

{-
echo "foo" > /tmp/ff
sha1sum /tmp/ff 
f1d2d2f924e986ac86fdf7b36c94bcdf32beec15  /tmp/ff

*Main > mkHash "foo\n"
"f1d2d2f924e986ac86fdf7b36c94bcdf32beec15"

-}
toHex :: Integral a => a -> String
toHex v =    showIntAtBase 16 intToDigit v ""

toBase64 v = undefined

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
