{-# LANGUAGE OverloadedStrings #-}

module Network.TeleHash.Switch
   (
     startSwitchThread
   , switch_init
   , runApp
   , openSocketIPv4
   , testId
   , ipv4Send
   , recvTelex
   , sendAll
   , sendDgram
   , addrFromHostPort

   -- * timers
   , onesec
   , timer
   , oneShotTimer
   ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.State
import Crypto.Random
import Data.Aeson.Types
import Network.BSD
import Network.Socket
import System.Log.Logger
import System.Time

import Network.TeleHash.Crypt
import Network.TeleHash.Crypto1a
import Network.TeleHash.Paths
import Network.TeleHash.Packet
import Network.TeleHash.Types
import Network.TeleHash.SwitchApi
import Network.TeleHash.Utils

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as SB

-- ---------------------------------------------------------------------

-- localIP = "10.0.0.28"
-- localIP :: String
-- localIP = "10.2.2.83"

-- ---------------------------------------------------------------------

runApp :: Maybe SocketHandle -> TeleHash a -> IO ()
runApp ms app = do
  sw <- switch_new
  _ <- runStateT app (sw { swH = ms })
  return ()


-- ---------------------------------------------------------------------
--
-- Set up actions to run on start and end, and run the main loop in
-- its own thread
--

startSwitchThread :: IO (Chan Signal,Chan Reply,ThreadId)
startSwitchThread = do
  (ch1,ch2,st) <- initialize
  -- thread <- forkIO (io (runStateT run st))
  thread <- forkIO (doit ch1 ch2 st)
  return (ch1,ch2,thread)

  where
    doit :: Chan Signal -> Chan Reply -> Switch -> IO ()
    doit ch1 ch2 st = do
      _ <- runStateT (run ch1 ch2) st
      return ()

-- ---------------------------------------------------------------------

-- | The first point where we have entered the TeleHash Monad
run :: Chan Signal -> Chan Reply -> TeleHash ()
run ch1 ch2 = do
  -- ch1 <- io (newChan)
  _ <- io (forkIO (timer (1000 * onesec) SignalPingSeeds ch1))
  _ <- io (forkIO (timer (1000 * onesec) SignalScanLines ch1))
  _ <- io (forkIO (timer (3000 * onesec) SignalTapTap ch1))
  _ <- io (forkIO (timer (3000 * onesec) SignalShowSwitch ch1))

  h <- gets swH
  _ <- io (forkIO (dolisten h ch1))




  -- load the id
  -- loadId testId
  -- logT $ "loading id done"

  -- crypt_deopenize_1a (Binary.decode $ BL.pack p1)

  -- error "done for now"

  -- load the seeds, hardcoded for now
  -- mapM_ addSeed initialSeeds
  -- logT $ "loading seeds done"

  -- logT $ "going online.."
  -- online nullCb
  -- logT $ "online done"



  -- Process the async messages from the various sources above
  forever $ do
    s <- io (readChan ch1)
    -- timeNow <- io getClockTime
    -- io (putStrLn $ "got signal: " ++ (show s) ++ " at " ++ (show timeNow))
    -- io (putStrLn $ "got signal:at " ++ (show timeNow))
    case s of
      -- SignalPingSeeds      -> pingSeeds
      -- SignalScanLines      -> scanlines timeNow
      -- SignalTapTap         -> taptap timeNow
      -- SignalSyncPath hn    -> do
      --   logT $ "SignalSyncPath for: " ++ show hn
      --   hnSync hn
      SignalMsgRx msg addr -> recvTelex msg addr
      -- External commands
      SignalShowSwitch      -> do
        sw <- get -- Switch
        logT $ "current switch:" ++ showSwitch sw
      SignalGetSwitch      -> do
        sw <- get -- Switch
        io (writeChan ch2 (ReplyGetSwitch sw))
      _ -> logT $ "run not processing signal:" ++ show s
    -- io (putStrLn $ "done signal:at " ++ (show timeNow))

-- ---------------------------------------------------------------------

initialize :: IO (Chan Signal,Chan b,Switch)
initialize = do
  -- Look up the hostname and port.  Either raises an exception
  -- or returns a nonempty list.  First element in that list
  -- is supposed to be the best option.

  -- (serveraddr,ip,port) <- resolveToSeedIPP initialSeed
  -- let seedIPP = IPP (ip ++ ":" ++ port)

  sock <- openSocketIPv4
  ch1 <- newChan
  ch2 <- newChan

  -- Save off the socket, and server address in a handle
  sw <- switch_new
  return (ch1, ch2, sw {swH = Just (SocketHandle sock)
                       ,swChan = Just ch1})

openSocketIPv4 :: IO Socket
openSocketIPv4 = do
  -- Establish a socket for communication
  --sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  sock <- NS.socket NS.AF_INET NS.Datagram defaultProtocol

  -- We want to listen on all interfaces (0.0.0.0)
  bindAddr <- NS.inet_addr "0.0.0.0"
  NS.bindSocket sock (NS.SockAddrInet 0 bindAddr)

  socketName <- NS.getSocketName sock
  warningM "Controller" ("server listening " ++ (show socketName))
  return sock

-- ---------------------------------------------------------------------
--
-- Listen for incoming messages and drop them in the FIFO
--
dolisten :: Maybe SocketHandle -> Chan Signal -> IO ()
dolisten Nothing _ = return ()
dolisten (Just h) channel = forever $ do
    (msg,rinfo) <- (SB.recvFrom (slSocket h) 1000)

    -- (putStrLn ("dolisten:rx msg=" ++ (BC.unpack msg)))
    (writeChan channel (SignalMsgRx msg rinfo))

onesec :: Int
onesec = 1000000

timer :: Int -> a -> Chan a -> IO ()
timer timeoutVal signalValue channel  = forever $
  threadDelay timeoutVal >> writeChan channel signalValue

oneShotTimer :: Int -> Signal -> TeleHash ()
oneShotTimer timeoutVal signalValue  = do
  mchannel <- gets swChan
  let Just channel = mchannel
  void $ io $ forkIO (threadDelay timeoutVal >> writeChan channel signalValue)
  return ()

-- ---------------------------------------------------------------------

doNullSendDgram :: LinePacket -> NS.SockAddr -> TeleHash ()
doNullSendDgram _msgJson _addr = do
  --logT ("doNullSendDgram[" ++ msgJson ++ "] to " ++ (show addr))
  logT ("doNullSendDgram" )

-- ---------------------------------------------------------------------

doSendDgram :: LinePacket -> NS.SockAddr -> TeleHash ()
doSendDgram (LP msgJson) address = do
  logT $ "doSendDgram to:" ++ show address
  Just socketh <- gets swH
  io (sendDgram (slSocket socketh) msgJson address)


-- ---------------------------------------------------------------------

sendDgram :: Socket -> BC.ByteString -> NS.SockAddr -> IO ()
sendDgram sock msgJson address =
  sendstr msgJson
    where
      -- Send until everything is done
      sendstr :: BC.ByteString -> IO ()
      sendstr omsg
        | BC.length omsg == 0  = return ()
        | otherwise = do sent <- SB.sendTo sock omsg address
                         sendstr (BC.drop sent omsg)

{-
sendDgram :: SocketHandle -> String -> NS.SockAddr -> IO ()
sendDgram socketh msgJson addr =
  sendstr msgJson
    where
      -- Send until everything is done
      sendstr :: String -> IO ()
      sendstr [] = return ()
      sendstr omsg = do sent <- NS.sendTo (slSocket socketh) omsg addr
                        sendstr (genericDrop sent omsg)

-}

-- ---------------------------------------------------------------------

resolve :: String -> String -> IO (NS.AddrInfo,String,String)
resolve hostname port = do
  -- Look up the hostname and port.  Either raises an exception
  -- or returns a nonempty list.  First element in that list
  -- is supposed to be the best option.
  addrinfos <- NS.getAddrInfo Nothing (Just hostname) (Just port)
  let serveraddr = head addrinfos

  (Just resolvedhost,Just servicename) <- (NS.getNameInfo [NS.NI_NUMERICHOST] True True (NS.addrAddress serveraddr))
  --putStrLn $ "resolve:" ++ (show hostname) ++ " " ++ (show servicename)

  --return (serveraddr,port)
  return (serveraddr,resolvedhost,servicename)

-- ---------------------------------------------------------------------

addrFromHostPort :: String -> String -> IO NS.SockAddr
addrFromHostPort hostname port = do
  (serveraddr,_,_) <- resolve hostname port
  return (NS.addrAddress serveraddr)

-- ---------------------------------------------------------------------

-- Dispatch incoming raw messages

recvTelex :: BC.ByteString -> NS.SockAddr -> TeleHash ()
recvTelex msg rinfo = do
    -- logT ( ("recvTelex:" ++  (show (msg))))
    logT $ "recvTelex:rinfo=" ++  show rinfo
    logH (">>>>:recvTelex:" ++ show rinfo) msg

    (Just hostIP,Just port) <- io (NS.getNameInfo [NS.NI_NUMERICHOST] True True rinfo)
    let
      remoteipp = (hostIP ++ ":" ++ port)

    timeNow <- io getClockTime
    --console.log(["RECV from ", remoteipp, ": ", JSON.stringify(telex)].join(""));
    logT ("RECV from " ++ (show remoteipp) ++ ":" -- ++ (show $ B16.encode msg)
                  ++ " at " ++ (show timeNow))
    -- logT ("RECV msg " ++ (show $ B16.encode msg))
    let
      maybeRxTelex = fromNetworkPacket (LP msg)
    -- logT $ "recvTelex:maybeRxTelex:" ++ show maybeRxTelex
    let
       path = Path
        {
          pJson    = PIPv4 (PathIPv4 (read hostIP) (read port))
        , pType = PtIPv4
        , pId = Nothing
        , pAtIn = Just timeNow
        , pAtOut = Nothing
        , pBridgeChan = Nothing
        }

    case maybeRxTelex of
      Just rxTelex -> switch_receive rxTelex path timeNow
      Nothing -> do
        logT $ "could not parse packet, discarding:" ++ (show $ B16.encode msg)
        return ()

-- ---------------------------------------------------------------------

-- | Send all queued packets from the switch
sendAll :: TeleHash ()
sendAll = do
  mp <- switch_sending
  -- logT $ "switch_sending returned:" ++ show mp
  case mp of
    Nothing -> return ()
    Just p -> do
      case tLp p of
        Nothing -> do
          assert False undefined
        Just lp -> do
          case (tOut p) of
            (PIPv4 _) -> do
              logT $ "sendall:sending " ++ show (tOut p)
              ipv4Send (tOut p) lp Nothing
            _ -> do
              logT $ "sendall:not sending " ++ show (tOut p)
      sendAll


-- ---------------------------------------------------------------------

{-
loadId :: Id -> TeleHash ()
loadId anId = do
  logT $ "loadId:" ++ show anId
  sw <- get
  -- put $ sw { swParts = [("1a",id1a anId),("1a_secret",id1a_secret anId)] }
  let (Hash hash1a) = mkHashFromB64 $ id1a anId
  put $ sw { swId = Map.fromList [("1a",id1a anId),("1a_secret",id1a_secret anId)]
           , swParts = [("1a",hash1a)]
           }

  loadkeys

  sw' <- get
  when (Map.size (swCs sw') == 0) $ logT "missing cipher sets"
  let hashName = parts2hn (swParts sw')
  put $ sw' { swId = Just hashName }
-}

-- ---------------------------------------------------------------------

testId :: Id
testId = r
  where
    v = "{\"1a\":\"o0UL/D6qQ+dcSX7hCoyMjLDYeA6dNScZ+YY/fcX4fyCtsSO2u9L5Lg==\",\"1a_secret\":\"iollyIcHaGeD/JpUNn/7ef1QAzE=\"}"
    Just r = Aeson.decode v

-- ---------------------------------------------------------------------

testSeeds :: IO ()
testSeeds = do
  fc <- BL.readFile "../data/seeds.json"
  let mv = Aeson.decode fc :: Maybe Value
  putStrLn $ "seeds=" ++ show mv


-- initialSeeds :: [SeedInfo]
-- initialSeeds = [seed195,seed253]
-- initialSeeds = [seed253]
-- initialSeeds = [seedLocal]
-- initialSeeds = []

{-
seedLocal:: SeedInfo
seedLocal =

  SI
   { sId = "3036d9b6f9525660b71d58bacd80d0ef0f6e191f1622daefd461823c366eb4fc"
   , sAdmin =  "alanz"
   , sPaths =
       [ Path { {-
                pType = PathType "ipv4"
              , pIp = localIP
              , pPort = 42424
              , pHttp = ""
              -}

                pJson = PIPv4 (PathIPv4 localIP 42424)
              , pAtIn = Nothing
              , pAtOut = Nothing
              , pId = Nothing
              }
      ]
    , sParts =
       [
         ("3a", "d4b78855e6cee2d005753ef4abe8bd05cc014efdd2bdb9c7994d34c712020a8e")
       , ("2a", "c216b2ccb1a832f0e893c847b0ef1f81d0a00f9fd0708b845299715226c87112")
       , ("1a", "7ce35806dc84943da12ea8d3a93bbcfdcf83e6b9")
       ]


    , sKeys =
      [ ("1a", "KaOZRKU3ouxNLGBHQV4TFAGrwM8pF8PncWC9XLcx+7H+fHebOTdcyg==")
      ]
    , sIsBridge = True
    }



seed195:: SeedInfo
seed195 =

  SI
   { sId = "f50f423ce7f94fe98cdd09268c7e57001aed300b23020840a84a881c76739471"
   , sAdmin =  "http://github.com/quartzjer"
   , sPaths =
       [ Path { {-
                pType = PathType "ipv4"
              , pIp = Just "208.126.199.195"
              , pPort = 42424
              , pHttp = ""
              -}
                pJson = PIPv4 (PathIPv4 "208.126.199.195" 42424)

              , pAtIn = Nothing
              , pAtOut = Nothing
              , pId = Nothing
              }

{-
    }, {
      "type": "ipv6",
      "ip": "2001:470:c0a6:3::10",
      "port": 42424
    }, {
      "type": "http",
      "http": "http://208.126.199.195:42424"
    }],
-}
      ]
    , sParts =
      [("2a", "8a5235d7cebb82d48a945e7c4b301efed40503d50ea1063464fe839b12278d93")
      ,("1a", "b3c9341ff5d11670c1e1c918ad51631b1251448a")
      ]
    , sKeys =
      [ ("2a", "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA5mWOu3o0chHcpcxPYX43fD6DTWGkCj09QaWerHbTX1Gua5eW8VdPOM/Ki21WEY2xcBa55/s37hIRP1XZveFiWgIXft9g/L+1AsF56cO0ZGnHhrp5Wabrt+L5mVuWg2VcSAUQ/gdoSLmDRTdOc0ruzroIN4a4Wnfk6rwvFYq/LfTj2w5cBD3ziVts4XSicX9fnmWElrTKfGLWyC6W5ICbLZ0BmTt9CZLbdNmotuYqQkPXPJ0wccsWAuI8yjbmU2my+F+vakWbGFvMSCBlLlQLXMTnLbLtgnwgeujTHiJaB0Iycw5Q9FS0RiQ0QeFqUvmMX9BezKfayq2hHjcob58WbwIDAQAB")
      , ("1a", "idT0VmmEmSdDF1eMrajIVHP0XZ/8Udgeas1Zxy0va5tD/KP393Ri3w==")
      ]
    , sIsBridge = True
    }


seed253 :: SeedInfo
seed253 =
  SI
    { sId = "89a4cbc6c27eb913c1bcaf06bac2d8b872c7cbef626b35b6d7eaf993590d37de"
    , sAdmin = "http://github.com/quartzjer"
    , sPaths =
       [ Path { {- pType = PathType "ipv4"
              , pIp = Just "208.68.164.253"
              , pPort = 42424
              , pHttp = ""
              -}
                pType = PtIPv4
              , pJson = PIPv4 (PathIPv4 "208.68.164.253" 42424)

              , pAtIn = Nothing
              , pAtOut = Nothing
              , pId = Nothing
              }
{-
       , Path { pType = PathType "ipv6"
              , pIp = Just "2605:da00:5222:5269:230:48ff:fe35:6572"
              , pPort = 42424
              , pHttp = ""
              , pLastIn = Nothing
              , pLastOut = Nothing
              , pRelay = Nothing
              , pId = Nothing
              , pPriority = Nothing
              , pIsSeed = True
              }
       , Path { pType = PathType "http"
              , pIp = Nothing
              , pPort = 42424
              , pHttp = "http://208.68.164.253:42424"
              , pLastIn = Nothing
              , pLastOut = Nothing
              , pRelay = Nothing
              , pId = Nothing
              , pPriority = Nothing
              , pIsSeed = True
              }
-}
       ]
    , sParts =
        [("2a", "beb07e8864786e1d3d70b0f537e96fb719ca2bbb4a2a3791ca45e215e2f67c9a")
        ,("1a", "6c0da502755941a463454e9d478b16bbe4738e67")
        ]
    , sKeys =
        [ ("2a", "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAvJlhpi2pZZRrnf+bmnnRRAQHfzMzDwOV+s+JzamyL0X9wwJK8m2DHCFcpJQSLFIzv3v+e102+pZlIWAU6vvO5s6J60C+9UwQoKj9L3cxUL/XmEBjAnbwfs+61HGSjf8yS8Uon/0oDXxssJyJjnrzAJT7K5G+Nqf5N5IJiEfhYkfa9wkhWR4fU1ZiC3PZZoMrGurGxWAwIs4No2LlBkZXUtAC31jISWODBAahGcaRjPgHFVaDxQ+gEQFse0Aa0mWA7KCRiSbBC89Brx837AREWFa7t14UAi01BLcPSkbAIbIv1SmM+E3k7HdN6rXTjz2h7Na5DOCS4z1LgujXW460WQIDAQAB")
        , ("1a", "hhzKwBDYADu6gQdDrP2AgQJmAE5iIbEqpPmLLZI9w72rLTCCqob9sw==")
        ]
    , sIsBridge = True
    }
-}

-- ---------------------------------------------------------------------

initRNG :: IO SystemRNG
initRNG = do
  pool <- createEntropyPool
  return $ cprgCreate pool

-- ---------------------------------------------------------------------

switch_new :: IO Switch
switch_new = do
  rng <- initRNG
  return $ Switch
       { swId          = HN "foo"
       , swParts       = []
       , swExternalIPP = Nothing
       , swSeeds       = Set.empty
       , swOut         = [] -- packets waiting to be delivered
       , swLast        = Nothing
       , swChans       = Set.empty
       , swUid         = 0
       , swTxid        = 0
       , swCrid        = CR 0
       , swCap         = 256 -- default cap size
       , swWindow      = 32 -- default reliable window size
       -- , swIsSeed      = False
       , swIsSeed      = True

       , swDhtK        = 8
       , swDhtLinkMax  = 256
       , swDht         = Map.empty

       , swIndex       = Map.empty
       , swIndexChans  = Map.empty
       , swIndexCrypto = Map.empty
       , swIndexLines  = Map.empty
       , swIndexSeeks  = Map.empty
       , swHandler     = Nothing

       , swH           = Nothing
       , swChan        = Nothing
       , swSender      = doSendDgram

       , swThtp        = Nothing
       , swIndexChat   = Map.empty
       , swIndexChatR  = Map.empty
       , swLink        = Nothing

       , swCurrentChat = Nothing

       , swRNG         = rng
       , swTick        = 0
       }

{-
switch_t switch_new(uint32_t prime)
{
  switch_t s;
  if(!(s = malloc(sizeof (struct switch_struct)))) return NULL;
  memset(s, 0, sizeof(struct switch_struct));
  s->cap = 256; // default cap size
  s->window = 32; // default reliable window size
  s->index = xht_new(prime?prime:MAXPRIME);
  s->parts = packet_new();
  if(!s->index || !s->parts) return switch_free(s);
  return s;
}

-}

-- ---------------------------------------------------------------------

switch_init :: Id -> TeleHash ()
switch_init anId = do
  logT $ "loading pk " ++ id1a anId ++ " sk " ++ id1a_secret anId
  mc <- (cs_new cset_1a) (Just (id1a anId)) Nothing
  c <- crypt_private (gfromJust "switch_init" mc) (id1a_secret anId)
  logT $ "loaded " ++ show anId

  sw <- get
  let parts = [((cCsid c),(unHash $ cPart c))]
  let sw2 = sw { swIndexCrypto = Map.insert "1a" c (swIndexCrypto sw)
               , swParts = parts
               , swId = parts2hn [("1a",unHash $ cPart c)]
               }
  put sw2

  -- Make sure our own id is a known hashname
  newHN (swId sw2)
  -- putHN $ (newHashContainer (swId sw2)) { hCrypto = Just c
  --                                       , hCsid = "1a" -- hardcoded for now
  --                                       , hParts = Just parts
  --                                       }
  withHN (swId sw2) $ \hc -> hc { hCrypto = Just c
                                , hCsid = "1a" -- hardcoded for now
                                , hParts = Just parts
                                }
  return ()
{-

int switch_init(switch_t s, packet_t keys)
{
  int i = 0, err = 0;
  char *key, secret[10], csid, *pk, *sk;
  crypt_t c;

  while((key = packet_get_istr(keys,i)))
  {
    i += 2;
    if(strlen(key) != 2) continue;
    util_unhex((unsigned char*)key,2,(unsigned char*)&csid);
    sprintf(secret,"%s_secret",key);
    pk = packet_get_str(keys,key);
    sk = packet_get_str(keys,secret);
    DEBUG_PRINTF("loading pk %s sk %s",pk,sk);
    c = crypt_new(csid, (unsigned char*)pk, strlen(pk));
    if(crypt_private(c, (unsigned char*)sk, strlen(sk)))
    {
      err = 1;
      crypt_free(c);
      continue;
    }
    DEBUG_PRINTF("loaded %s",key);
    xht_set(s->index,(const char*)c->csidHex,(void *)c);
    packet_set_str(s->parts,c->csidHex,c->part);
  }
  
  packet_free(keys);
  if(err || !s->parts->json)
  {
    DEBUG_PRINTF("key loading failed");
    return 1;
  }
  s->id = hn_getparts(s->index, s->parts);
  if(!s->id) return 1;
  return 0;
}

-}

-- --------------------------------------------------------------------

-- ---------------------------------------------------------------------

-- | Send the body of the packet in the telex. It is already encrypted
ipv4Send :: PathJson -> LinePacket -> Maybe HashName -> TeleHash ()
ipv4Send path msg _ = do
  -- logT $ "ipv4Send:" ++ show (path)
  -- logT $ "ipv4Send:" ++ show (B16.encode $ lbsTocbs $ unLP msg)
  -- logT $ "ipv4Send:" ++ (show $ gfromJust "ipv4Send" $ pathIp path) ++ ":" ++ (show $ pathPort path)
  addr <- io (addrFromHostPort (show $ gfromJust "ipv4Send.1" $ pjsonIp path)
                               (show $ gfromJust "ipv4Send.2" $ pjsonPort path))

  sender <- gets swSender
  sender msg addr
  return ()
