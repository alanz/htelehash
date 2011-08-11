--
-- From http://telehash.org/
-- $ echo '{"+end":"3b6a6..."}' | nc -u telehash.org 42424
-- should result in 
-- {"_ring":17904, ".see":[ "208.68.163.247:42424", "208.68.160.25:55137"], "_br":52, "_to":"173.19.99.143:63681" }
{-
Got the following message from 208.68.163.247:42424
{"_ring":17867,".see":["50.19.183.3:47610","50.18.184.21:43435","50.18.184.21:48563","208.68.163.247:42424","50.18.184.21:37180"],"_br":19,"_to":"196.209.236.138:33164"}
-}

-- See http://book.realworldhaskell.org/read/sockets-and-syslog.html

-- module Main (main) where

import Data.Aeson 
import Data.Bits
import Data.List
import Network.BSD
import Network.Socket 
import qualified Network.Socket.ByteString.Lazy as SL
--import qualified Blaze.ByteString.Builder as BB
import TeleHash.Json 
import qualified Data.ByteString.Lazy as BL

-- ---------------------------------------------------------------------

data SyslogHandle = 
    SyslogHandle {slSocket :: Socket,
                  slAddress :: SockAddr}

openlog :: HostName             -- ^ Remote hostname, or localhost
        -> String               -- ^ Port number or name; 514 is default
        -> IO SyslogHandle      -- ^ Handle to use for logging
openlog hostname port =
    do -- Look up the hostname and port.  Either raises an exception
       -- or returns a nonempty list.  First element in that list
       -- is supposed to be the best option.
       addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
       let serveraddr = head addrinfos

       -- Establish a socket for communication
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

       -- We want to listen on all interfaces (0.0.0.0)
       bindAddr <- inet_addr "0.0.0.0"

       -- Bind to 0.0.0.0:30000
       --bindSocket sock (SockAddrInet 30000 bindAddr)
       bindSocket sock (SockAddrInet 0 bindAddr)
    
       -- Save off the socket, program name, and server address in a handle
       return $ SyslogHandle sock (addrAddress serveraddr)
       
syslog :: SyslogHandle -> String -> IO ()
syslog syslogh msg =
    sendstr sendmsg
    where code = 4 --makeCode fac pri
          sendmsg = "{\"+end\":\"3b6a6...\"}"

          -- Send until everything is done
          sendstr :: String -> IO ()
          sendstr [] = return ()
          sendstr omsg = do sent <- sendTo (slSocket syslogh) omsg (slAddress syslogh)
                            sendstr (genericDrop sent omsg)
          
closelog :: SyslogHandle -> IO ()
closelog syslogh = sClose (slSocket syslogh)


--doit :: IO ()
doit = do
  h <- openlog "telehash.org" "42424"
  syslog h "blah"
  
  -- Read a message of max length 1000 from some one
  -- (msg,len,from) <- recvFrom (slSocket h) 1000
  msg <- SL.recv (slSocket h) 1000

  -- putStrLn $ "Got the following message from " ++ (show from)
  -- putStrLn (show msg)
  putStrLn (show (parseAll (head $ BL.toChunks msg)))
  
  closelog h
  


{-
h <- openlog "telehash.org" "42424"
syslog h "blah"
closelog h
-}

{-
-- From http://www.haskell.org/pipermail/haskell-cafe/2010-August/082725.html

  sock <- socket AF_INET Datagram 17 -- 17 is IPPROTO_UDP
  addr <- inet_addr "127.0.0.1"
  bindSocket sock (SockAddrInet 44004 addr)
  setSocketOption sock NoDelay 1 -- <-- doesn't seem to work

  addr <- inet_addr "127.0.0.1"
  forever $ do
    sent <- sendTo sock "hello" (SockAddrInet 44005 addr)
    dat <- recv sock 5  -- <-- I'd like it to be non-blocking
    B.putStrLn dat
    threadDelay 1000000 -- 1 second
-}