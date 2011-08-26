--
-- From http://telehash.org/
-- $ echo '{"+end":"3b6a6..."}' | nc -u telehash.org 42424
-- should result in 
-- {"_ring":17904, ".see":[ "208.68.163.247:42424", "208.68.160.25:55137"], "_br":52, "_to":"173.19.99.143:63681" }
{-
Got the following message from 208.68.163.247:42424
{"_ring":17867,".see":["50.19.183.3:47610","50.18.184.21:43435","50.18.184.21:48563","208.68.163.247:42424","50.18.184.21:37180"],"_br":19,"_to":"196.209.236.138:33164"}
-}

module TeleHash.TeleHash (
  ) where

--import Data.Aeson 
--import Data.Bits
import Data.List
import Network.BSD
import Network.Socket 
import qualified Network.Socket.ByteString.Lazy as SL
import TeleHash.Json 
import qualified Data.ByteString.Lazy as BL

-- ---------------------------------------------------------------------

data SocketHandle = 
    SocketHandle {slSocket :: Socket,
                  slAddress :: SockAddr}

-- ---------------------------------------------------------------------  

opensocket :: HostName             -- ^ Remote hostname, or localhost
        -> String               -- ^ Port number or name; 514 is default
        -> IO SocketHandle      -- ^ Handle to use for logging
opensocket hostname port =
    do -- Look up the hostname and port.  Either raises an exception
       -- or returns a nonempty list.  First element in that list
       -- is supposed to be the best option.
       addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
       let serveraddr = head addrinfos

       -- Establish a socket for communication
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

       -- We want to listen on all interfaces (0.0.0.0)
       bindAddr <- inet_addr "0.0.0.0"
       bindSocket sock (SockAddrInet 0 bindAddr)
    
       -- Save off the socket, and server address in a handle
       return $ SocketHandle sock (addrAddress serveraddr)
       
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

closesocket :: SocketHandle -> IO ()
closesocket socketh = sClose (slSocket socketh)

-- ---------------------------------------------------------------------  

--doit :: IO ()
doit = do
  h <- opensocket "telehash.org" "42424"
  sendmsg h "blah"
  
  msg <- SL.recv (slSocket h) 1000

  -- putStrLn $ "Got the following message from " ++ (show from)
  -- putStrLn (show msg)
  putStrLn (show (parseAll (head $ BL.toChunks msg)))
  
  closesocket h
  


