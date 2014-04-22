{-# LANGUAGE OverloadedStrings #-}

module TeleHash.Switch
  (
    Defaults(..)
  , defaults
  , initial_switch
  , startSwitchThread
  ) where


import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Crypto.Random
import Data.Aeson
import Data.Aeson.Types
import Data.Bits
import Data.ByteString.Internal (w2c)
import Data.Char
import Data.IP
import Data.List
import Data.Maybe
import Data.String.Utils
import Data.Scientific
import Data.Typeable
import Network.BSD
import Prelude hiding (id, (.), head, either, catch)
import System.Environment
import System.IO
import System.Log.Handler.Simple
import System.Log.Logger
import System.Time
import TeleHash.Convert
import TeleHash.Crypto1a
import TeleHash.Packet
import TeleHash.Paths
import TeleHash.Utils

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as SB
import qualified System.Random as R

-- ---------------------------------------------------------------------

localIP = "10.0.0.28"
-- localIP = "10.2.2.83"

-- ---------------------------------------------------------------------
--
-- Set up actions to run on start and end, and run the main loop
--
runSwitch :: IO ((),Switch)
runSwitch = bracket initialize disconnect loop
  where
    disconnect (_,_,ss) = NS.sClose (slSocket (fromJust $ swH ss))

    loop (ch1,ch2,st) = catch (runStateT (run ch1 ch2) st) (exc)

    exc :: SomeException -> IO ((),Switch)
    exc _e = return ((),(assert False undefined))

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

  h <- gets swH
  _ <- io (forkIO (dolisten h ch1))

  -- load the id
  loadId testId
  logT $ "loading id done"

  -- crypt_deopenize_1a (Binary.decode $ BL.pack p1)

  -- error "done for now"

  -- load the seeds, hardcoded for now
  mapM_ addSeed initialSeeds
  logT $ "loading seeds done"

  logT $ "going online.."
  online nullCb
  logT $ "online done"

  -- -------------- this bit from ping.c -------------------------------
  {-
  // create/send a ping packet
  c = chan_new(s, bucket_get(seeds, 0), "seek", 0);
  p = chan_packet(c);
  packet_set_str(p,"seek",s->id->hexname);
  chan_send(c, p);
  -}
  sw <- get
  let seed0 = head (swSeeds sw)
  let Just hcs = Map.lookup seed0 (swAll sw)
  let path = Path { {- pType = PathType "ipv4"
                  -- , pIp = Just "208.68.164.253"
                  , pIp = localIP
                  , pPort = 42424
                  , pHttp = ""
                  -}
                  pJson = PIPv4 (PathIPv4 localIP 42424)

                  , pLastIn = Nothing
                  , pLastOut = Nothing
                  , pRelay = Nothing
                  , pId = Nothing
                  , pPriority = Nothing
                  , pIsSeed = True
                  , pGone = False
                  }
  let msg = Telex { tId = Nothing
                  , tType = Just "seek"
                  , tPath = Nothing
                  , tJson = HM.fromList
                       [("type",String "seek")
                       ,("c", Number 0)
                       ,("seek",String (Text.pack $ unHN $ hHashName hcs))
                       ]
                  , tCsid = Just "1a"
                  , tChanId = Nothing
                  , tTo = Just path
                  , tPacket = Nothing
                  }

  -- io $ threadDelay (5 * onesec)
{-
  logT $ "AZ starting ping inject"
  -- AZ carry on here: use send, not raw
  packet <- telexToPacket msg
  -- let (hcs',lined) = crypt_lineize_1a hcs packet
  -- send path lined Nothing

  timeNow <- io getClockTime
  let js = "{\"c\":0,\"seek\":\"" ++ (unHN $ hHashName hcs) ++ "\"}"
      Just json@(Aeson.Object jsHashMap) = Aeson.decode (cbsTolbs $ BC.pack js) :: Maybe Aeson.Value
      msg' = RxTelex { rtId = 0
                     , rtSender = path
                     , rtAt = timeNow
                     , rtJs = jsHashMap
                     , rtPacket = Packet HeadEmpty (Body BC.empty)
                     , rtChanId = Nothing
                     }

  -- c <- raw hcs "seek" msg' nullRxCb
  -- logT $ "sending msg returned :" ++ show c
  -- xxxxxxxxxxxxxxxxxxxx
-}
  -- -------------- ping.c end -----------------------------------------


  -- Process the async messages from the various sources above
  forever $ do
    s <- io (readChan ch1)
    timeNow <- io getClockTime
    -- io (putStrLn $ "got signal: " ++ (show s) ++ " at " ++ (show timeNow))
    -- io (putStrLn $ "got signal:at " ++ (show timeNow))
    case s of
      -- SignalPingSeeds      -> pingSeeds
      -- SignalScanLines      -> scanlines timeNow
      -- SignalTapTap         -> taptap timeNow
      SignalSyncPath hn    -> hnSync hn
      SignalMsgRx msg addr -> recvTelex msg addr
      -- External commands
      SignalGetSwitch      -> do
        sw <- getSwitch
        io (writeChan ch2 (ReplyGetSwitch sw))
      _ -> logT $ "run not processing signal:" ++ show s
    -- io (putStrLn $ "done signal:at " ++ (show timeNow))

-- ---------------------------------------------------------------------

getSwitch :: TeleHash Switch
getSwitch = do
  switch <- get
  return switch

-- ---------------------------------------------------------------------

initialize :: IO (Chan Signal,Chan b,Switch)
initialize = do
  -- Look up the hostname and port.  Either raises an exception
  -- or returns a nonempty list.  First element in that list
  -- is supposed to be the best option.

  -- (serveraddr,ip,port) <- resolveToSeedIPP initialSeed
  -- let seedIPP = IPP (ip ++ ":" ++ port)

  -- Establish a socket for communication
  --sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  sock <- NS.socket NS.AF_INET NS.Datagram defaultProtocol

  -- We want to listen on all interfaces (0.0.0.0)
  bindAddr <- NS.inet_addr "0.0.0.0"
  NS.bindSocket sock (NS.SockAddrInet 0 bindAddr)

  socketName <- NS.getSocketName sock
  warningM "Controller" ("server listening " ++ (show socketName))

  ch1 <- newChan
  ch2 <- newChan

  -- Save off the socket, and server address in a handle
  sw <- initial_switch
  return (ch1, ch2, sw {swH = Just (SocketHandle sock)
                       ,swChan = Just ch1})

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



-- =====================================================================
-- ---------------------------------------------------------------------

-- API


{-
int switch_init(switch_t s, packet_t keys)
{

  char *csid = crypt_supported;
  if(!keys) return 1;

  while(*csid)
  {
    loadkey(*csid,s,keys);
    csid++;
  }

  packet_free(keys);
  if(!s->parts->json) return 1;
  s->id = hn_getparts(s->index, s->parts);
  if(!s->id) return 1;
  return 0;
}

-- Result of running telehash-c ping

alanz@alanz-laptop:~/mysrc/github/telehash/telehash-c$ ./bin/ping
*** public key o0UL/D6qQ+dcSX7hCoyMjLDYeA6dNScZ+YY/fcX4fyCtsSO2u9L5Lg== ***
*** secret key iollyIcHaGeD/JpUNn/7ef1QAzE= ***
loaded hashname 7ecf6a5884d483fde2f6a027e33e6e1756efdb70925557c3e3f776b35329aef5


-}

testId :: Id
testId = r
  where
    v = "{\"1a\":\"o0UL/D6qQ+dcSX7hCoyMjLDYeA6dNScZ+YY/fcX4fyCtsSO2u9L5Lg==\",\"1a_secret\":\"iollyIcHaGeD/JpUNn/7ef1QAzE=\"}"
    Just r = decode v

testSeeds = do
  fc <- BL.readFile "../data/seeds.json"
  let mv = decode fc :: Maybe Value
  putStrLn $ "seeds=" ++ show mv


-- ---------------------------------------------------------------------

relayPid :: PathId
relayPid = PId 1

ipv4Pid :: PathId
ipv4Pid  = PId 2


initialSeeds :: [SeedInfo]
-- initialSeeds = [seed195,seed253]
-- initialSeeds = [seed253]
initialSeeds = [seedLocal]


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
              , pLastIn = Nothing
              , pLastOut = Nothing
              , pRelay = Nothing
              , pId = Nothing
              , pPriority = Nothing
              , pIsSeed = True
              , pGone = False
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

              , pLastIn = Nothing
              , pLastOut = Nothing
              , pRelay = Nothing
              , pId = Nothing
              , pPriority = Nothing
              , pIsSeed = True
              , pGone = False
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
                pJson = PIPv4 (PathIPv4 "208.68.164.253" 42424)

              , pLastIn = Nothing
              , pLastOut = Nothing
              , pRelay = Nothing
              , pId = Nothing
              , pPriority = Nothing
              , pIsSeed = True
              , pGone = False
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

-- ---------------------------------------------------------------------

data Defaults = Defaults
  { chanTimeout :: Int
  , seekTimeout :: Int
  , chanAutoAck :: Int
  , chanResend :: Int
  , chanOutBuf :: Int
  , chanInBuf :: Int
  , natTimeout :: Int
  , idleTimeout :: Int
  , linkTimer :: Int
  , linkMax :: Int
  , linkK :: Int
  }

-- ---------------------------------------------------------------------

defaults :: Defaults
defaults = Defaults
  {
  chanTimeout = 10000, -- how long before for ending durable channels w/ no acks
  seekTimeout = 3000, -- shorter tolerance for seeks, is far more lossy
  chanAutoAck = 1000, -- is how often we auto ack if the app isn't generating responses in a durable channel
  chanResend  = 2000, -- resend the last packet after this long if it wasn't acked in a durable channel
  chanOutBuf  = 100, -- max size of outgoing buffer before applying backpressure
  chanInBuf   = 50, -- how many incoming packets to cache during processing/misses
  natTimeout  = 60*1000, -- nat timeout for inactivity
  idleTimeout = 5*60*1000, -- defaults.nat_timeout; -- overall inactivity timeout
  linkTimer   = 55*1000, -- defaults.nat_timeout - (5*1000); -- how often the DHT link maintenance runs
  linkMax     = 256,  -- maximum number of links to maintain overall (minimum one packet per link timer)
  linkK       = 8  -- maximum number of links to maintain per bucket
  }

-- ---------------------------------------------------------------------

initial_switch :: IO Switch
initial_switch = do
  rng <- initRNG
  let
    sw = Switch
      { swH = Nothing
      , swChan = Nothing
      , swSender = doSendDgram
      , swSeeds = []
      , swLocals = Set.empty
      , swLines = Map.empty
      , swBridges = []
      , swBridgeLine = []
      , swBridging = False
      , swAll = Map.empty
      , swBuckets = Map.empty
      , swCapacity = []
      , swRels = Map.empty
      , swPaths = Map.empty
      , swBridgeCache = []

      , swHashname = Nothing
      , swId = Map.empty
      , swIdCrypto = Nothing
      , swCs = Map.empty
      , swKeys = Map.empty

      , swCSets = Map.fromList [("1a",cset_1a)]
      , swParts = []

      , swPub4 = Nothing
      , swPriority = Nothing
      , swLoad = loadId
      , swMake = keysgen

      -- configure defaults
      , swNat = False
      , swSeed = True

      , swLanToken = Nothing

      -- udp socket stuff
      , swPcounter = 1
      -- , swReceive = receive

      , swNetworks = Map.fromList [(PtRelay, (relayPid,relay))
                                  ,(PtIPv4,  (ipv4Pid,ipv4Send))]

      -- connect to the network, online(callback(err))
      , swIsOnline = False

      --  internal listening unreliable channels
      , swRaws = Map.fromList
               [ ("peer",   inPeer)
               , ("connect",inConnect)
               , ("seek",   inSeek)
               , ("path",   inPath)
               , ("bridge", inBridge)
               , ("link",   inLink)
               ]

      , swWaits = []
      , swWaiting = Nothing


      -- crypto
      , swRNG = rng

      , swCountOnline = 0
      , swCountTx = 0
      , swCountRx = 0
      }
{-
  put sw
  linkLoop -- should never return
  sw' <- get
  return sw'
-}
  return sw

initRNG :: IO SystemRNG
initRNG = do
  pool <- createEntropyPool
  return $ cprgCreate pool

-- ---------------------------------------------------------------------

crypt_supported :: TeleHash [String]
crypt_supported = do
  sw <- get
  return $ Map.keys (swCSets sw)

-- ---------------------------------------------------------------------
{-
  self.load = function(id)
  {
    if(!id || !id.parts) return "bad keys";
    self.parts = id.parts;
    self.id = id;
    var err = loadkeys(self);
    if(err) return err;
    if(Object.keys(self.cs).length == 0) return "missing cipher sets";
    self.hashname = parts2hn(self.parts);
    return false;
  }
-}

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
  put $ sw' { swHashname = Just hashName }


-- These three are set in crypto 1a loadkey
{-
id.key = bs
id.public
id.private
-}

{-
  csids <- crypt_supported
  Just hn <- crypt_loadkey_1a (id1a anId) (Just $ id1a_secret anId)
  logT $ "loadId:got hn=" ++ show (hcHashName hn)
  put $ sw {swParts = hcParts hn, swHashname = Just (hcHashName hn)}
  return ()
-}

loadIdOld :: Id -> TeleHash ()
loadIdOld anId = do
  logT $ "loadId:" ++ show anId
  sw <- get
  csids <- crypt_supported
  Just hn <- crypt_loadkey_1a (id1a anId) (Just $ id1a_secret anId)
  logT $ "loadId:got hn=" ++ show (hcHashName hn)
  put $ sw {swParts = hcParts hn, swHashname = Just (hcHashName hn)}
  return ()


-- ---------------------------------------------------------------------

loadkeys :: TeleHash ()
loadkeys = do
  sw <- get
  put $ sw { swCs = Map.empty, swKeys = Map.empty }

  -- logT $ "loadkeys:swId=" ++ show (swId sw)

  let
    doOne (csid,_v) = do
      sw <- get
      let cs' = Map.delete csid (swCs sw)
      put sw {swCs = cs'}

      case Map.lookup csid (swCSets sw) of
        Nothing -> logT $ csid ++ " not supported"

        Just cset -> do
          let mpub = Map.lookup csid (swId sw)
              mpriv = Map.lookup (csid ++ "_secret") (swId sw)
          case mpub of
            Nothing -> return ()
            Just pub -> do
              mhc <- (csLoadkey cset) pub mpriv
              case mhc of
                Just hc -> do
                  -- timeNow <- io getClockTime
                  -- randomHexVal <- randomHEX 16
                  let keys = Map.insert csid pub (swKeys sw)
                      -- h = mkHashContainer (hcHashName hc) timeNow randomHexVal
                      -- h' = h { hSelf = Just hc }
                      -- allHc = Map.insert (hcHashName hc) h' (swAll sw)
                  -- put $ sw {swKeys = keys, swAll = allHc }
                  put $ sw { swKeys = keys
                           , swIdCrypto = Just hc
                           , swCs = Map.insert csid hc (swCs sw)
                            }
                Nothing -> return ()
              return ()
      return ()

  mapM_ doOne (swParts sw)
  sw' <- get

  -- logT $ "loadkeys: (swKeys,swCs)=" ++ show (swKeys sw',swCs sw')

  return ()

{-
function loadkeys(self)
{
  self.cs = {};
  self.keys = {};
  var err = false;
  Object.keys(self.parts).forEach(function(csid){
    self.cs[csid] = {};
    if(!self.CSets[csid]) err = csid+" not supported";
    err = err||self.CSets[csid].loadkey(self.cs[csid], self.id[csid], self.id[csid+"_secret"]);
    self.keys[csid] = self.id[csid];
  });
  return err;
}
-}

-- ---------------------------------------------------------------------

-- |process the incoming packet
-- was swReceive
receive :: NetworkPacket -> Path -> ClockTime -> TeleHash ()
receive rxPacket path timeNow = do
  -- logT $ "receive: (rxTelex,remoteipp,timeNow):" ++ show (rxPacket,path,timeNow)
  if (networkPacketLen rxPacket) == 2
    then return () -- Empty packets are NAT pings
    else do
      counterVal <- incPCounter
      let packet = NetworkTelex
                    { ntSender = path
                    , ntId     = counterVal
                    , ntAt     = timeNow
                    , ntPacket = rxPacket
                    }
      -- debug(">>>>",Date(),msg.length, packet.head_length, packet.body_length,[path.type,path.ip,path.port,path.id].join(","));
      logT $ ">>>>" ++ show (timeNow, networkPacketLen rxPacket,(showPath path))

      case rxPacket of
       {-
        HeadJson _j -> do

          -- handle any LAN notifications
          logT $ "receive: not processing JSON"
          assert False undefined

          return ()
        -}
        OpenPacket b bs -> do
          -- process open packet
          open <- deopenize packet
          case open of
            DeOpenizeVerifyFail -> do
              logT $ "receive.deopenize: couldn't decode open"
              return ()
            _ -> do
              -- logT $ "receive.deopenize verified ok " -- ++ show open
              let (Aeson.Object js) = doJs open
              if not (expectedKeysPresent (doJs open) ["line","to","from","at"])
                then do
                  logT $ "deopenize missing required js fields, have only:" ++ show (HM.keys js)
                  return ()
                else do
                  if not (isHEX (valToBs (js HM.! "line")) 32)
                    then do
                      logT $ "invalid line id enclosed" ++ show (js HM.! "line")
                      return ()
                    else do
                      sw <- get
                      if (valToString (js HM.! "to") /= unHN (gfromJust "deopenize" $ swHashname sw))
                        then do
                          logT $ "open for wrong hashname" ++ show (js HM.! "to")
                          return ()
                        else do
                          let (Aeson.Object jsparts) = js HM.! "from"
                              jsparts2 = map (\(k,v) -> (Text.unpack k,valToString v)) $ HM.toList jsparts
                          -- logT $ "deopenize:jsparts=" ++ show jsparts2
                          let mcsid = partsMatch (swParts sw) jsparts2
                          -- logT $ "deopenize:mcsid=" ++ show mcsid
                          if mcsid /= Just (doCsid open)
                            then do
                              logT $ "open with mismatch CSID" ++ show (mcsid,doCsid open)
                              return ()
                            else do
                              -- var from = self.whokey(open.js.from,open.key);
                              -- if (!from) return warn("invalid hashname", open.js.from);
                              let keyVal = BC.unpack $ B64.encode (doKey open)
                              mfrom <- whokey jsparts2 (Left keyVal)
                              case mfrom of
                                Nothing -> do
                                  logT $ "deopenize:invalid hashname=" ++ show jsparts2
                                  return ()
                                Just from -> do
                                  -- // make sure this open is legit
                                  -- if (typeof open.js.at != "number") return warn("invalid at", open.js.at);
                                  -- logT $ "deopenize:js.at=" ++ show (js HM.! "at")
                                  case (js HM.! "at") of
                                    (Data.Aeson.Number atVal) -> do
                                      -- duplicate open and there's newer line packets, ignore it
                                      -- if(from.openAt && open.js.at <= from.openAt && from.lineAt == from.openAt) return;
                                      let showTod (TOD a b) = "(TOD" ++ show a ++ " " ++ show b ++ ")"
                                          showmTod Nothing = "Nothing"
                                          showmTod (Just tod) = "Just " ++ showTod tod
                                      let jsAt = TOD (round (atVal / 1000)) (((round  atVal) `mod` 1000) * 10^9)
                                      -- logT $ "deopenize:jsAt=" ++ showTod jsAt
                                      -- logT $ "deopenize:hOpenAt=" ++ showmTod (hOpenAt from)
                                      -- logT $ "deopenize:hLineAt=" ++ showmTod (hLineAt from)
                                      if (isJust (hOpenAt from) && jsAt <= (fromJust (hOpenAt from))
                                         && (hLineAt from == hOpenAt from))
                                        then do
                                          logT $ "deopenize:duplicate open and newer line packets, ignoring"
                                          return ()
                                        else do
                                          -- open is legit!
                                          logT $ "inOpen verified" ++ show (hHashName from)
                                          -- add this path in
                                          path2 <- hnPathIn from path

                                          -- if new line id, reset incoming channels
                                          if Just (valToBs (js HM.! "line")) /= (hLineIn from)
                                            then do
                                              logT $ "new line"
                                              putHN from
                                              forM_ (Map.keys (hChans from)) $ \id1 -> do
                                                if channelSlot id1 == channelSlot (hChanOut from)
                                                  then return () -- our ids
                                                  else do
                                                    fm <- getHNsafe (hHashName from) "deopenize"
                                                    case Map.lookup id1 (hChans fm) of
                                                      Nothing -> return ()
                                                      Just c -> chanFail (hHashName from) c Nothing
                                                    delChan (hHashName from) id1
                                                return ()
                                            else return ()

                                          -- update values
                                          from2 <- getHNsafe (hHashName from) "deopenize"
                                          let from3 = from2
                                                        { hOpenAt = Just jsAt
                                                        , hLineIn = Just (b16Tobs $ valToBs (js HM.! "line"))
                                                        }
                                          putHN from3

                                          -- send an open back
                                          mpacket <- hnOpen from3
                                          case mpacket of
                                            Nothing -> do
                                              logT $ "deopenize: hnOpen returned Nothing"
                                              return ()
                                            Just msg -> do
                                              sw2 <- get
                                              send path msg (Just from3)
                                          -- self.send(path,from.open(),from);

                                          -- line is open now!
                                          from4 <- getHNsafe (hHashName from) "deopenize"
                                          sw3 <- get
                                          let from5 = from4 { hCsid = Just (doCsid open) }
                                              mcset = Map.lookup (doCsid open) (swCSets sw3)
                                          case mcset of
                                            Nothing -> do
                                              logT $ "deopenize: cset lookup failed"
                                            Just cset -> do
                                              (csOpenLine cset) from5 open
                                              logT $ "line open " ++ show (hHashName from5,hLineOut from5,B16.encode $ gfromJust "deopenize" $ hLineIn from5)
                                              from8 <- getHNsafe (hHashName from) "deopenize.8"
                                              -- logT $ "deopenize:hEncKey from8=" ++ show (hEncKey from8,hDecKey from8)
                                              sw4 <- get
                                              put $ sw4 { swLines
                                                         = Map.insert (hLineOut from8) (hHashName from8) (swLines sw3)}

                                          -- resend the last sent packet again
                                          -- logT $ "deopenize:not doing last packet resend"
                                          case (hLastPacket from5) of
                                            Nothing -> return ()
                                            Just msg -> do
                                              from6 <- getHNsafe (hHashName from5) "deopenize.9"
                                              putHN $ from6 { hLastPacket = Nothing }
                                              from7 <- getHNsafe (hHashName from5) "deopenize.10"
                                              logT $ "deopenize:resending packet"
                                              -- logT $ "deopenize:hEncKey from6=" ++ show (hEncKey from6)
                                              -- logT $ "deopenize:hEncKey from7=" ++ show (hEncKey from7)

                                              void $ hnSend from7 msg

                                          -- if it was a lan seed, add them
                                          sw4 <- get
                                          if hIsLocal from5 && Set.notMember (hHashName from5) (swLocals sw4)
                                            then put $ sw4 { swLocals = Set.insert (hHashName from5) (swLocals sw4)}
                                            else return ()

                                    _ -> do
                                     logT $ "deopenize:invalid is, need Number:" ++ show (js HM.! "at")
                                     return ()
              return ()
        LinePacket pbody -> do
          -- its a line
          logT $ "receive:got line msg"
          let lineID = BC.unpack $ B16.encode $ BC.take 16 pbody
          logT $ "receive:lineID=" ++ lineID
          sw <- get
          case Map.lookup lineID (swLines sw) of
            Nothing -> do
              -- a matching line is required to decode the packet
              assert False undefined
            Just lineHn -> do
              -- decrypt and process
              line <- getHNsafe lineHn "receive.line"
              case Map.lookup (gfromJust "receive.line" $ hCsid line) (swCSets sw) of
                Nothing -> do
                  logT $ "receive.line:couldn't load cset for:" ++ show (hCsid line)
                Just cset -> do
                  res <- (csDelineize cset) line packet
                  case res of
                    Left err -> do
                      logT $ "couldn't decrypt line:" ++ err
                      assert False undefined
                    Right pkt -> do
                      putHN $ line { hLineAt = hOpenAt line }
                      line2 <- getHNsafe (hHashName line) "receive.line"
                      hnReceive line2 pkt
          return ()
          {-
          // or it's a line
          if(packet.head.length == 0)
          {
            var lineID = packet.body.slice(0,16).toString("hex");
            var line = packet.from = self.lines[lineID];

            // a matching line is required to decode the packet
            if(!line) {
              if(!self.bridgeLine[lineID]) return debug("unknown line received", lineID, JSON.stringify(packet.sender));
              debug("BRIDGE",JSON.stringify(self.bridgeLine[lineID]),lineID);
              var id = crypto.createHash("sha256").update(packet.body).digest("hex")
              if(self.bridgeCache[id]) return; // drop duplicates
              self.bridgeCache[id] = true;
              // flat out raw retransmit any bridge packets
              return self.send(self.bridgeLine[lineID],msg);
            }

            // decrypt and process
            var err;
            if((err = self.CSets[line.csid].delineize(line, packet))) return debug("couldn't decrypt line",err,packet.sender);
            line.lineAt = line.openAt;
            line.receive(packet);
            return;
          }

          -}

      return ()


{-
// self.receive, raw incoming udp data
function receive(msg, path)
{
  var self = this;
  var packet = pdecode(msg);
  if(!packet) return warn("failed to decode a packet from", path, msg.toString());
  if(packet.length == 2) return; // empty packets are NAT pings

  packet.sender = path;
  packet.id = self.pcounter++;
  packet.at = Date.now();
  debug(">>>>",Date(),msg.length, packet.head_length, packet.body_length,[path.type,path.ip,path.port,path.id].join(","));

  // handle any LAN notifications
  if(packet.js.type == "lan") return inLan(self, packet);
  if(packet.js.type == "seed") return inLanSeed(self, packet);

  // either it's an open
  if(packet.head.length == 1)
  {
    var open = deopenize(self, packet);
    if (!open || !open.verify) return warn("couldn't decode open",open);
    if (!isHEX(open.js.line, 32)) return warn("invalid line id enclosed",open.js.line);
    if(open.js.to !== self.hashname) return warn("open for wrong hashname",open.js.to);
    var csid = partsMatch(self.parts,open.js.from);
    if(csid != open.csid) return warn("open with mismatch CSID",csid,open.csid);

    var from = self.whokey(open.js.from,open.key);
    if (!from) return warn("invalid hashname", open.js.from);

    // make sure this open is legit
    if (typeof open.js.at != "number") return warn("invalid at", open.js.at);

    // duplicate open and there's newer line packets, ignore it
    if(from.openAt && open.js.at <= from.openAt && from.lineAt == from.openAt) return;

    // open is legit!
    debug("inOpen verified", from.hashname);
    from.recvAt = Date.now();

    // add this path in
    path = from.pathIn(path);

    // if new line id, reset incoming channels
    if(open.js.line != from.lineIn)
    {
      debug("new line");
      Object.keys(from.chans).forEach(function(id){
        if(id % 2 == from.chanOut % 2) return; // our ids
        if(from.chans[id]) from.chans[id].fail({js:{err:"reset"}});
        delete from.chans[id];
      });
    }

    // update values
    var line = {};
    from.openAt = open.js.at;
    from.lineIn = open.js.line;

    // send an open back
    self.send(path,from.open(),from);

    // line is open now!
    from.csid = open.csid;
    self.CSets[open.csid].openline(from, open);
    debug("line open",from.hashname,from.lineOut,from.lineIn);
    self.lines[from.lineOut] = from;

    // resend the last sent packet again
    if (from.lastPacket) {
      var packet = from.lastPacket;
      delete from.lastPacket;
      from.send(packet)
    }

    // if it was a lan seed, add them
    if(from.local && self.locals.indexOf(from) == -1) self.locals.push(from);

    return;
  }

  // or it's a line
  if(packet.head.length == 0)
  {
    var lineID = packet.body.slice(0,16).toString("hex");
    var line = packet.from = self.lines[lineID];

    // a matching line is required to decode the packet
    if(!line) {
      if(!self.bridgeLine[lineID]) return debug("unknown line received", lineID, JSON.stringify(packet.sender));
      debug("BRIDGE",JSON.stringify(self.bridgeLine[lineID]),lineID);
      var id = crypto.createHash("sha256").update(packet.body).digest("hex")
      if(self.bridgeCache[id]) return; // drop duplicates
      self.bridgeCache[id] = true;
      // flat out raw retransmit any bridge packets
      return self.send(self.bridgeLine[lineID],msg);
    }

    // decrypt and process
    var err;
    if((err = self.CSets[line.csid].delineize(line, packet))) return debug("couldn't decrypt line",err,packet.sender);
    line.lineAt = line.openAt;
    line.receive(packet);
    return;
  }

  if(Object.keys(packet.js).length > 0) warn("dropping incoming packet of unknown type", packet.js, packet.sender);
}

-}

-- ---------------------------------------------------------------------
{-
  self.deliver = function(type, callback){ self.networks[type] = callback};
-}
-- was swDeliver
deliver :: String -> () -> ()
deliver = (assert False undefined)

-- ---------------------------------------------------------------------
{-
  self.networks["relay"] = function(path,msg){
    if(path.relay.ended) return debug("dropping dead relay");
    path.relay.send({body:msg});
  };
-}
relay :: Path -> LinePacket -> Maybe HashContainer -> TeleHash ()
relay path msg _ = (assert False undefined)

-- ---------------------------------------------------------------------

{-
  self.send = function(path, msg, to){
    if(!msg) return debug("send called w/ no packet, dropping");
    if(to) path = to.pathOut(path);
    if(!path) return debug("send called w/ no valid network, dropping");
    debug("<<<<",Date(),msg.length,[path.type,path.ip,path.port,path.id].join(","),to&&to.hashname);

    // try to send it via a supported network
    if(self.networks[path.type]) self.networks[path.type](path,msg,to);

    // if the path has been active in or out recently, we're done
    if(Date.now() - path.lastIn < defaults.nat_timeout || Date.now() - path.lastOut < (defaults.chan_timeout / 2)) return;

    // no network support or unresponsive path, try a bridge
    self.bridge(path,msg,to);
  };
-}

-- | Do the send, where the Telex has a fully lineized packet in it
-- was swSend
send :: Path -> LinePacket -> Maybe HashContainer -> TeleHash ()
send mpath msg mto = do
  -- logT $ "send entered for path:" ++ showPath mpath
  timeNow <- io getClockTime
  sw <- get
  path <- case mto of
    Just to -> do
               mpath <- hnPathOut to mpath
               return $ gfromJust "send" mpath
    Nothing -> return mpath
    -- if(!path) return debug("send called w/ no valid network, dropping");
    -- debug("<<<<",Date(),msg.length,[path.type,path.ip,path.port,path.id].join(","),to&&to.hashname);
  logT $ "<<<<" ++ show (timeNow,BC.length $ unLP msg) ++ "," ++ showPath mpath
  -- try to send it via a supported network
  -- if(self.networks[path.type])
     -- self.networks[path.type](path,msg,to);
  logT $ "send:path=" ++ showPath path
  mpid <- case Map.lookup (pathType path) (swNetworks sw) of
    Nothing -> return Nothing
    Just (pid,sender) -> do
      sender path msg mto
      return $ Just pid

  logT $ "send: must still update stats, and bridge if necessary"
{-
  case mpid of
    Nothing -> do
       logT "send called w/ no valid network, dropping"
       return ()
    Just pid -> do
       -- if the path has been active in or out recently (ideally by the
       -- send process), we're done
       timeNow <- io getClockTime
       sw' <- get -- get a fresh one, send may have updated it
       let Just p = Map.lookup pid (swPaths sw')
       if (isTimeOut timeNow (pLastIn p)  (natTimeout defaults)) ||
          (isTimeOut timeNow (pLastOut p) (chanTimeout defaults))
         then
           -- no network support or unresponsive path, try a bridge
           (swBridge sw') path msg mto
         else
           return ()
-}

{-
    if(to) path = to.pathOut(path);
    if(!path) return debug("send called w/ no valid network, dropping");
    debug("<<<<",Date(),msg.length,[path.type,path.ip,path.port,path.id].join(","),to&&to.hashname);

    // try to send it via a supported network
    if(self.networks[path.type]) self.networks[path.type](path,msg,to);

    // if the path has been active in or out recently, we're done
    if(Date.now() - path.lastIn < defaults.nat_timeout || Date.now() - path.lastOut < (defaults.chan_timeout / 2)) return;

    // no network support or unresponsive path, try a bridge
    self.bridge(path,msg,to);
  };
-}


-- ---------------------------------------------------------------------

-- was swPathSet
pathSet :: Path -> TeleHash ()
pathSet path = (assert False undefined)

{-
  self.pathSet = function(path)
  {
    var updated = (self.paths[path.type] && JSON.stringify(self.paths[path.type]) == JSON.stringify(path));
    self.paths[path.type] = path;
    // if ip4 and local ip, set nat mode
    if(path.type == "ipv4") self.nat = isLocalIP(path.ip);
    // trigger pings if our address changed
    if(self.isOnline && !updated)
    {
      debug("local network updated, checking links")
      linkMaint(self);
    }
  }

-}

-- ---------------------------------------------------------------------


-- | this creates a hashname identity object (or returns existing)
-- If it creates a new one, this is inserted into the index
-- was swWhois
whois :: HashName -> TeleHash (Maybe HashContainer)
whois hn = do
  -- logT $ "whois entered for:" ++ show hn
  sw <- get

  -- never return ourselves
  if (fromMaybe (HN "") (swHashname sw)) == hn
    then do
      -- logT "whois called for self"
      return Nothing
    else do
      -- logT "whois not called for self"
      -- if we already have it, return it
      case Map.lookup hn (swAll sw) of
        Just hc -> do
          -- logT $ "whois got cached val" -- ++ show hc
          return (Just hc)
        Nothing -> do
          -- logT "whois not seen value"
          timeNow <- io getClockTime
          randomHexVal <- randomHEX 16
          -- logT $ "whois:randomHexVal=" ++ show randomHexVal
          let hc = mkHashContainer hn timeNow randomHexVal
              hc' = hc {hBucket = dhash (fromJust $ swHashname sw) hn }
          -- to create a new channels to this hashname

              chanOut = if (head $ sort [fromJust $ swHashname sw,hn]) == (fromJust $ swHashname sw)
                          then 2 else 1
              hc'' = hc' { hChanOut = chanOut }
              swAll' = Map.insert hn hc'' (swAll sw)
          put $ sw {swAll = swAll'}
          return $ Just hc''

{-
// this creates a hashname identity object (or returns existing)
function whois(hashname)
{
  var self = this;
  // validations
  if(!hashname) { warn("whois called without a hashname", hashname, new Error().stack); return false; }
  if(typeof hashname != "string") { warn("wrong type, should be string", typeof hashname,hashname); return false; }
  hashname = hashname.split(",")[0]; // convenience if an address is passed in
  if(!isHEX(hashname, 64)) { warn("whois called without a valid hashname", hashname); return false; }

  // never return ourselves
  if(hashname === self.hashname) { debug("whois called for self"); return false; }

  var hn = self.all[hashname];
  if(hn) return hn;

  // make a new one
  hn = self.all[hashname] = {hashname:hashname, chans:{}, self:self, paths:[], isAlive:0};
  hn.at = Date.now();
  hn.bucket = dhash(self.hashname, hashname);
  if(!self.buckets[hn.bucket]) self.buckets[hn.bucket] = [];

  // to create a new channels to this hashname
  var sort = [self.hashname,hashname].sort();
  hn.chanOut = (sort[0] == self.hashname) ? 2 : 1;
  hn.start = channel;
  hn.raw = raw;

  // manage network information consistently, called on all validated incoming packets
  hn.pathIn = function(path)
  {
    path = hn.pathGet(path);

    // first time we've seen em
    if(!path.lastIn && !path.lastOut)
    {
      debug("PATH INNEW",JSON.stringify(path.json),hn.paths.map(function(p){return JSON.stringify(p.json)}));
      // for every new incoming path, trigger a sync (delayed so caller can continue/respond first)
      setTimeout(hn.sync,1);

      // update public ipv4 info
      if(path.type == "ipv4" && !isLocalIP(path.ip))
      {
        hn.ip = path.ip;
        hn.port = path.port;
      }

      // cull any invalid paths of the same type
      hn.paths.forEach(function(other){
        if(other == path) return;
        if(other.type != path.type) return;
        if(!pathValid(other)) hn.pathEnd(other);
      });

      // "local" custom paths, we must bridge for
      if(path.type == "local") hn.bridging = true;

      // track overall if we trust them as local
      if(isLocalPath(path)) hn.isLocal = true;
    }

    path.lastIn = Date.now();
    self.recvAt = Date.now();

    // end any active relay
    if(hn.to && hn.to.type == "relay" && path.type != "relay") hn.to.relay.fail();

    // update default if better
    if(!pathValid(hn.to) || pathValid(path)) hn.to = path;
    hn.alive = pathValid(hn.to);

    return path;
  }


  // handle all incoming line packets
  hn.receive = function(packet)
  {
//    if((Math.floor(Math.random()*10) == 4)) return warn("testing dropping randomly!");
    if(!packet.js || typeof packet.js.c != "number") return warn("dropping invalid channel packet",packet.js);

    debug("LINEIN",JSON.stringify(packet.js));
    hn.recvAt = Date.now();
    // normalize/track sender network path
    packet.sender = hn.pathIn(packet.sender);

    // find any existing channel
    var chan = hn.chans[packet.js.c];
    if(chan === false) return; // drop packet for a closed channel
    if(chan) return chan.receive(packet);

    // start a channel if one doesn't exist, check either reliable or unreliable types
    var listening = {};
    if(typeof packet.js.seq == "(assert False undefined)") listening = self.raws;
    if(packet.js.seq === 0) listening = self.rels;
    if(!listening[packet.js.type])
    {
      // bounce error
      if(!packet.js.end && !packet.js.err)
      {
        warn("bouncing unknown channel/type",packet.js);
        var err = (packet.js.type) ? "unknown type" : "unknown channel"
        hn.send({js:{err:err,c:packet.js.c}});
      }
      return;
    }

    // verify incoming new chan id
    if(packet.js.c % 2 == hn.chanOut % 2) return warn("channel id incorrect",packet.js.c,hn.chanOut)

    // make the correct kind of channel;
    var kind = (listening == self.raws) ? "raw" : "start";
    var chan = hn[kind](packet.js.type, {bare:true,id:packet.js.c}, listening[packet.js.type]);
    chan.receive(packet);
  }

  // track who told us about this hn
  hn.via = function(from, address)
  {
    if(typeof address != "string") return warn("invalid see address",address);
    if(!hn.vias) hn.vias = {};
    if(hn.vias[from.hashname]) return;
    hn.vias[from.hashname] = address;
  }

  // just make a seek request conveniently
  hn.seek = function(hashname, callback)
  {
    var bucket = dhash(hn.hashname, hashname);
    var prefix = hashname.substr(0, Math.ceil((255-bucket)/4)+2);
    hn.raw("seek", {timeout:defaults.seek_timeout, retry:3, js:{"seek":prefix}}, function(err, packet, chan){
      callback(packet.js.err,Array.isArray(packet.js.see)?packet.js.see:[]);
    });
  }

  // return our address to them
  hn.address = function(to)
  {
    if(!to) return "";
    var csid = partsMatch(hn.parts,to.parts);
    if(!csid) return "";
    if(!hn.ip) return [hn.hashname,csid].join(",");
    return [hn.hashname,csid,hn.ip,hn.port].join(",");
  }


  // send a simple lossy peer request, don't care about answer
  hn.peer = function(hashname, csid)
  {
    if(!csid || !self.parts[csid]) return;
    var js = {"peer":hashname};
    js.paths = hn.pathsOut();
    hn.raw("peer",{js:js, body:getkey(self,csid)}, function(err, packet, chan){
      if(err) return;
      if(!packet.body) return warn("relay in w/ no body",packet.js,packet.from.hashname);
      // create a network path that maps back to this channel
      var path = {type:"relay",relay:chan,json:{type:"relay",relay:packet.from.hashname}};
      if(packet.js.bridge) path = packet.sender; // sender is offering to bridge, use them!
      self.receive(packet.body, path);
    });
  }

  // return the current open packet
  hn.open = function()
  {
    if(!hn.parts) return false; // can't open if no key
    if(hn.opened) return hn.opened;
    hn.opened = openize(self,hn);
    return hn.opened;
  }

  // generate current paths array to them, for peer and paths requests
  hn.pathsOut = function()
  {
    var paths = [];
    if(self.paths.pub4) paths.push({type:"ipv4", ip:self.paths.pub4.ip, port:self.paths.pub4.port});
    if(self.paths.pub6) paths.push({type:"ipv6", ip:self.paths.pub6.ip, port:self.paths.pub6.port});
    if(self.paths.http)
    {
      if(self.paths.http.http) paths.push({type:"http", http:self.paths.http.http});
      else if(self.paths.pub4) paths.push({type:"http", http:"http://"+self.paths.pub4.ip+":"+self.paths.http.port});
    }
    if(self.paths.webrtc) paths.push({type:"webrtc"});
    if(hn.isLocal)
    {
      if(self.paths.lan4) paths.push({type:"ipv4", ip:self.paths.lan4.ip, port:self.paths.lan4.port});
      if(self.paths.lan6) paths.push({type:"ipv6", ip:self.paths.lan6.ip, port:self.paths.lan6.port});
    }
    return paths;
  }


  return hn;
}

-}
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------

-- |handle all incoming line packets, post decryption
hnReceive :: HashContainer -> RxTelex -> TeleHash ()
hnReceive hn rxTelex = do
  let packet = rtPacket rxTelex
      jsHashMap = rtJs rxTelex
  case (HM.lookup "c" jsHashMap) of
    Nothing -> logT $ "dropping invalid channel packet, c missing"
    Just (Aeson.Number c) -> do
      logT $ "LINEIN " ++ showJson jsHashMap
      timeNow <- io getClockTime
      putHN hn { hRecvAt = Just timeNow }
      hn2 <- getHNsafe (hHashName hn) "hnReceive"
      -- normalize/track sender network path
      path <- hnPathIn hn2 (rtSender rxTelex)

      -- find any existing channel
      case Map.lookup (CID (round c)) (hChans hn2) of
        Just chan -> do
          if (chDone chan)
            then return () -- drop packet for a closed channel
            else chanReceive hn chan (rxTelex { rtSender = path })
        Nothing -> do
          -- start a channel if one doesn't exist, check either reliable or unreliable types
          sw <- get
          let (listening,cKind)
               = case HM.lookup "seq" jsHashMap of
                  Nothing -> (swRaws sw,raw)
                  Just (Aeson.Number n) -> if (round n) == 0
                                             then ((swRels sw),start)
                                             else (Map.empty,assert False undefined)
              mptype = HM.lookup "type" jsHashMap
              ptype = case mptype of
                        Nothing -> "*unknown*"
                        Just v -> valToString v
          case Map.lookup ptype listening of
            Nothing -> do
              -- bounce error
              assert False undefined
            Just fn -> do
              -- verify incoming new chan id
              if channelSlot (CID (round c)) == channelSlot (hChanOut hn)
                then do
                  logT $ "channel id incorrect" ++ show (c,hChanOut hn)
                  return ()
                else do
                  -- make the correct kind of channel
                  let cb = Map.findWithDefault (assert False undefined) ptype listening
                  -- var chan = hn[kind](packet.js.type, {bare:true,id:packet.js.c}, listening[packet.js.type]);
                  chan <- cKind hn2 ptype (rxTelexToTelex rxTelex) cb
                  putChan (hHashName hn2) chan
                  hn3 <- getHNsafe (hHashName hn2) "hnReceive"
                  chanReceive hn3 chan rxTelex
    Just _ -> logT $ "dropping invalid channel packet, c not numeric"

{-
  // handle all incoming line packets
  hn.receive = function(packet)
  {
//    if((Math.floor(Math.random()*10) == 4)) return warn("testing dropping randomly!");
    if(!packet.js || typeof packet.js.c != "number") return warn("dropping invalid channel packet",packet.js);

    debug("LINEIN",JSON.stringify(packet.js));
    hn.recvAt = Date.now();
    // normalize/track sender network path
    packet.sender = hn.pathIn(packet.sender);

    // find any existing channel
    var chan = hn.chans[packet.js.c];
    if(chan === false) return; // drop packet for a closed channel
    if(chan) return chan.receive(packet);

    // start a channel if one doesn't exist, check either reliable or unreliable types
    var listening = {};
    if(typeof packet.js.seq == "undefined") listening = self.raws;
    if(packet.js.seq === 0) listening = self.rels;
    if(!listening[packet.js.type])
    {
      // bounce error
      if(!packet.js.end && !packet.js.err)
      {
        warn("bouncing unknown channel/type",packet.js);
        var err = (packet.js.type) ? "unknown type" : "unknown channel"
        hn.send({js:{err:err,c:packet.js.c}});
      }
      return;
    }

    // verify incoming new chan id
    if(packet.js.c % 2 == hn.chanOut % 2) return warn("channel id incorrect",packet.js.c,hn.chanOut)

    // make the correct kind of channel;
    var kind = (listening == self.raws) ? "raw" : "start";
    var chan = hn[kind](packet.js.type, {bare:true,id:packet.js.c}, listening[packet.js.type]);
    chan.receive(packet);
  }


-}
-- ---------------------------------------------------------------------

hnPathOut :: HashContainer -> Path -> TeleHash (Maybe Path)
hnPathOut hn path = do
  path' <- hnPathGet hn path
  if (pathType path' == PtRelay)
     && isJust (pRelay path') && chEnded (fromJust (pRelay path'))
    then do
      hnPathEnd hn path'
      return Nothing
    else do
      timeNow <- io getClockTime
      let path'' = path' { pLastOut = Just timeNow }
          hn' = if (not $ pathValid timeNow (hTo hn)) &&
                   (pathValid timeNow (Just path''))
                  then hn { hTo = Just path''}
                  else hn
      putHN hn'
      return $ Just path''
{-
  hn.pathOut = function(path)
  {
    if(!path) return false;
    path = hn.pathGet(path);
    if(path.type == "relay" && path.relay.ended) return hn.pathEnd(path);
    path.lastOut = Date.now();
    if(!pathValid(hn.to) && pathValid(path)) hn.to = path;
    return path;
  }

-}

-- ---------------------------------------------------------------------

hnPathEnd :: HashContainer -> Path -> TeleHash ()
hnPathEnd hn path = do
  if (pIsSeed path) -- never remove a seed path
    then return ()
    else do
      let hn2 = if hTo hn == Just path
                 then hn {hTo = Nothing}
                 else hn
          paths = filter (/=path) (hPaths hn)
      putHN $ hn2 { hPaths = paths }
      logT $ "PATH END" ++ show path

{-
  hn.pathEnd = function(path)
  {
    if(path.seed) return false; // never remove a seed-path
    if(hn.to == path) hn.to = false;
    path.gone = true;
    var index = hn.paths.indexOf(path);
    if(index >= 0) hn.paths.splice(index,1);
    debug("PATH END",JSON.stringify(path.json));
    return false;
  }


-}
-- ---------------------------------------------------------------------

hnPathIn :: HashContainer -> Path -> TeleHash Path
hnPathIn hn path = do
  path1 <- hnPathGet hn path
  timeNow <- io getClockTime

  -- first time we've seen em
  if (pLastIn path1 == Nothing && pLastOut path1 == Nothing)
    then do
      -- debug("PATH INNEW",JSON.stringify(path.json),hn.paths.map(function(p){return JSON.stringify(p.json)}));
      logT $ "PATH INNEW " ++ showPath path1
      -- for every new incoming path, trigger a sync (delayed so caller can continue/respond first)
      oneShotTimer (1 * onesec) (SignalSyncPath (hHashName hn))
      -- update public ipv4 info
      let hn1 = if (pathType path1 == PtIPv4 && not (isLocalIP (gfromJust "hnPathIn" (pathIp path1))))
                  then hn {hIp = pathIp path1, hPort = pathPort path1}
                  else hn
      putHN hn1
      -- cull any invalid paths of the same type
      forM_ (hPaths hn1) $ \other -> do
        if ((other == path1) -- ++AZ++ TODO: check what we define as equality
           || (pathType other /= pathType path1))
          then return ()
          else do
            if not (pathValid timeNow (Just other))
              then do
                hnNow <- getHNsafe (hHashName hn1) "hnPathIn"
                void $ hnPathEnd hnNow other
                return ()
              else return ()

      -- "local" custom paths we must bridge for
      if pathType path1 == PtLocal
        then do
          hnNow <- getHNsafe (hHashName hn1) "hnPathIn.1"
          putHN $ hnNow {hBridging = True}
        else return ()

      -- track overall if we trust them as local
      if isLocalPath path1
        then do
          hnNow <- getHNsafe (hHashName hn1) "hnPathIn.2"
          putHN $ hnNow {hIsLocal = True}
        else return ()
    else return ()

  hnNow <- getHNsafe (hHashName hn) "hnPathIn.3"

  -- end any active relay
  logT $ "hnPathIn: must still code 'end any active relay'"
  -- if(hn.to && hn.to.type == "relay" && path.type != "relay") hn.to.relay.fail();

  --  update default if better
  let hnNow2 = if (not (pathValid timeNow (hTo hnNow))
                  || pathValid timeNow (Just path1))
                 then hnNow {hTo = Just path1}
                 else hnNow

  putHN hnNow2 { hIsAlive = pathValid timeNow (hTo hnNow2)
               , hRecvAt = Just timeNow
               }


  return $ path1 {pLastIn = Just timeNow }
{-
  hn.pathIn = function(path)
  {
    path = hn.pathGet(path);

    // first time we've seen em
    if(!path.lastIn && !path.lastOut)
    {
      debug("PATH INNEW",JSON.stringify(path.json),hn.paths.map(function(p){return JSON.stringify(p.json)}));
      // for every new incoming path, trigger a sync (delayed so caller can continue/respond first)
      setTimeout(hn.sync,1);

      // update public ipv4 info
      if(path.type == "ipv4" && !isLocalIP(path.ip))
      {
        hn.ip = path.ip;
        hn.port = path.port;
      }

      // cull any invalid paths of the same type
      hn.paths.forEach(function(other){
        if(other == path) return;
        if(other.type != path.type) return;
        if(!pathValid(other)) hn.pathEnd(other);
      });

      // "local" custom paths, we must bridge for
      if(path.type == "local") hn.bridging = true;

      // track overall if we trust them as local
      if(isLocalPath(path)) hn.isLocal = true;
    }

    path.lastIn = Date.now();
    self.recvAt = Date.now();

    // end any active relay
    if(hn.to && hn.to.type == "relay" && path.type != "relay") hn.to.relay.fail();

    // update default if better
    if(!pathValid(hn.to) || pathValid(path)) hn.to = path;
    hn.alive = pathValid(hn.to);

    return path;
  }


-}
-- ---------------------------------------------------------------------

-- | Try to send the packet, return True on success
-- try to send a packet to a hashname, doing whatever is possible/necessary
hnSend :: HashContainer -> Telex -> TeleHash Bool
hnSend hn packet = do
  sw <- get
  sent <- case hLineIn hn of
    Just lineIn -> do
      logT $ "line sending " ++ show (hHashName hn, lineIn)
      timeNow <- io getClockTime
      -- TODO: dispatch this via CSets
      msg <- telexToPacket packet
      (hn',lined) <- crypt_lineize_1a hn msg
      putHN hn'
      -- directed packets are preferred, just dump and done
      case tTo packet of
        Just to -> do send to lined (Just hn')
                      return True
        Nothing -> do
          -- send to the default best path
          case hTo hn of
            Just to -> do send to lined (Just hn')
                          return True
            Nothing -> do return False -- need to fall through
    Nothing -> do
      logT $ "hnSend:hLineIn = Nothing"
      return False

  -- logT $ "hnSend:sent=" ++ show sent
  if sent
    then return True -- we're done
    else do
      -- we've fallen through, either no line, or no valid paths
      -- logT $ "alive failthrough" ++ show (hSendSeek hn, Map.keys (hVias hn))
      let hn' = hn { hIsAlive = False
                   , hLastPacket = Just packet -- will be resent if/when an open is received
                   }
      putHN hn'

      -- always send to all known paths, increase resiliency
      mp <- hnOpen hn'
      hn2 <- getHNsafe (hHashName hn) "hnSend"
      case mp of
        Nothing -> do
          logT $ "hnSend: hnOpen returned Nothing"
          return False
        Just lpacket -> do
          -- logT $ "hnSend: hnOpen returned packet" -- ++ show lpacket
          forM_ (hPaths hn2) $ \path -> do
            send path lpacket (Just hn2)
          return True

      logT $ "hnSend: must still do send using vias, and retry backoff"
      return True

  -- return False
{-
  // try to send a packet to a hashname, doing whatever is possible/necessary
  hn.send = function(packet){
    // if there's a line, try sending it via a valid network path!
    if(hn.lineIn)
    {
      debug("line sending",hn.hashname,hn.lineIn);
      var lined = packet.msg || self.CSets[hn.csid].lineize(hn, packet);
      hn.sentAt = Date.now();

      // directed packets are preferred, just dump and done
      if(packet.to) return self.send(packet.to, lined, hn);

      // send to the default best path
      if(hn.to) self.send(hn.to, lined, hn);

      // if it was good, we're done, if not fall through
      if(pathValid(hn.to)) return;
    }

    // we've fallen through, either no line, or no valid paths
    debug("alive failthrough",hn.sendSeek,Object.keys(hn.vias||{}));
    hn.alive = false;
    hn.lastPacket = packet; // will be resent if/when an open is received

    // always send to all known paths, increase resiliency
    hn.paths.forEach(function(path){
      self.send(path, hn.open(), hn);
    });

    // also try using any via informtion to create a new line
    function vias()
    {
      if(!hn.vias) return;
      hn.sentOpen = false; // whenever we send a peer, we'll always need to resend any open regardless
      // try to connect vias
      var todo = hn.vias;
      delete hn.vias; // never use more than once
      Object.keys(todo).forEach(function(via){
        var address = todo[via].split(",");
        if(address.length <= 1) return;
        if(address.length == 4 && address[2].split(".").length == 4 && parseInt(address[3]) > 0)
        {
          // NAT hole punching
          var path = {type:"ipv4",ip:address[2],port:parseInt(address[3])};
          self.send(path,pencode());
          // if possibly behind the same NAT, set flag to allow/ask to relay a local path
          if(self.nat && address[2] == (self.paths.pub4 && self.paths.pub4.ip)) hn.isLocal = true;
        }
        // send the peer request
        self.whois(via).peer(hn.hashname, address[1]);
      });
    }

    // if there's via information, just try that
    if(hn.vias) return vias();


    // never too fast, worst case is to try to seek again
    if(!hn.sendSeek || (Date.now() - hn.sendSeek) > 5000)
    {
      hn.sendSeek = Date.now();
      self.seek(hn, function(err){
        if(!hn.lastPacket) return; // packet was already sent elsewise
        vias(); // process any new vias
      });
    }

  }

-}

-- ---------------------------------------------------------------------

-- |send a full network path sync
hnSync :: HashName -> TeleHash ()
hnSync hn = do
  hc <- getHNsafe hn "hnSync"
  logT $ "SYNCING:" ++ show hn ++ " " ++ (intercalate "," $ map showPath (hPaths hc))

  -- compose all of our known paths we can send to them
  paths <- hnPathsOut hn
  logT $ "hnSync:paths=" ++ (intercalate "," $ map showPath paths)

  timeNow <- io getClockTime

  -- check all paths at once
  forM_ paths $ \path -> do
    logT $ "PATHLOOP" ++ show (length (hPaths hc)) ++ "," ++ showPath path
    logT $ "hnSync: must process local/relay paths when implemented"
    -- if(["relay","local"].indexOf(path.type) == -1) js.path = path.json;

    -- our outgoing priority of this path
    --  js.priority = (path.type == "relay") ? 0 : 1;
    let js = HM.fromList [("priority",Number 1)] :: (HM.HashMap Text.Text Aeson.Value)
    let js2 = HM.insert "paths" (toJSON paths) js

    let cb :: Bool -> RxTelex -> Channel -> TeleHash ()
        cb err packet chan = do
          -- when it actually errored and hasn't been active, invalidate it
          if err
            then do
              logT $ "hnSync:must check for lastIn activity since the send"
            else do
              inPath True packet chan

    let msg = RxTelex { rtId = 0
                      , rtSender = path
                      , rtAt = timeNow
                      , rtJs = js2
                      , rtPacket = newPacket
                      , rtChanId = Nothing
                      }

    -- raw :: HashContainer -> String -> RxTelex -> RxCallBack -> TeleHash Channel
    hc2 <- getHNsafe hn "hnSync.2"
    raw hc2 "path" (rxTelexToTelex msg) cb

{-

  // send a full network path sync
  hn.sync = function()
  {
    debug("SYNCING",hn.hashname,hn.paths.map(function(p){return JSON.stringify(p.json)}));

    // compose all of our known paths we can send to them
    var paths = hn.pathsOut();

    // check all paths at once
    hn.paths.forEach(function(path){
      debug("PATHLOOP",hn.paths.length,JSON.stringify(path.json));
      var js = {};
      if(["relay","local"].indexOf(path.type) == -1) js.path = path.json;
      // our outgoing priority of this path
      js.priority = (path.type == "relay") ? 0 : 1;
      if(paths.length > 0) js.paths = paths;
      var lastIn = path.lastIn;
      hn.raw("path",{js:js, timeout:3000, to:path}, function(err, packet){
        // when it actually errored and hasn't been active, invalidate it
        if(err && err !== true && path.lastIn == lastIn) path.lastIn = 0;
        else inPath(true, packet); // handles any response .priority and .paths
      });
    });
  }

-}


-- ---------------------------------------------------------------------

-- |return the current open packet
hnOpen :: HashContainer -> TeleHash (Maybe LinePacket)
hnOpen hn = do
  case hParts hn of
    Nothing -> return Nothing
    _ -> if isJust (hOpened hn)
           then return $ hOpened hn
           else do
             op <- openize (hHashName hn)
             hn2 <- getHNsafe (hHashName hn) "hnOpen"
             putHN $ hn2 { hOpened = op}
             -- logT $ "hnOpen:hEcc=" ++ show (hEcc hn2)
             return op
{-
  // return the current open packet
  hn.open = function()
  {
    if(!hn.parts) return false; // can't open if no key
    if(hn.opened) return hn.opened;
    hn.opened = openize(self,hn);
    return hn.opened;
  }
-}

-- ---------------------------------------------------------------------

hnPathsOut :: HashName -> TeleHash [Path]
hnPathsOut hn = do
  hc <- getHNsafe hn "hnPathsOut"
  if (hIsLocal hc)
    then return []
    else return $ filter isLocalPath (hPaths hc)

{-
  // generate current paths array to them, for peer and paths requests
  hn.pathsOut = function()
  {
    var paths = [];
    self.paths.forEach(function(path){
      if(isLocalPath(path) && !hn.isLocal) return;
      paths.push(path);
    });
    return paths;
  }

-}

-- ---------------------------------------------------------------------

telexToPacket :: Telex -> TeleHash Telex
telexToPacket telex = do
  case (HM.toList $ tJson telex) of
    [] -> return $ telex { tPacket = Just newPacket}
    js -> do
      -- logT $ "telexToPacket: encoded js=" ++ show (encode (tJson telex))
      let packet = newPacket { paHead = HeadJson (lbsTocbs $ encode (tJson telex)) }
      return $ telex { tPacket = Just packet}

-- ---------------------------------------------------------------------
{-
function whokey(parts, key, keys)
{
  var self = this;
  if(typeof parts != "object") return false;
  var csid = partsMatch(self.parts,parts);
  if(!csid) return false;
  hn = self.whois(parts2hn(parts));
  if(!hn) return false;
  hn.parts = parts;
  if(keys) key = keys[csid]; // convenience for addSeed
  var err = loadkey(self,hn,csid,key);
  if(err)
  {
    warn("whokey err",hn.hashname,err);
    return false;
  }
  return hn;
}

-}

-- was swWhokey
whokey :: Parts -> Either String (Map.Map String String) -> TeleHash (Maybe HashContainer)
whokey parts keyVal = do
  sw <- get
  let mcsid = partsMatch (swParts sw) parts
  -- logT $ "whokey:mcsid=" ++ show mcsid
  -- TODO: put this bit in the Maybe Monad
  r <- case mcsid of
    Nothing -> return Nothing
    Just csid -> do
      mhn <- whois (parts2hn parts)
      -- logT $ "whokey:whois returned:" ++ show mhn
      case mhn of
        Nothing -> return Nothing
        Just hn -> do
          mhc <- case keyVal of
            Left k -> loadkey csid k
            Right keys ->
              case Map.lookup csid keys of
               Nothing -> return Nothing
               Just k -> loadkey csid k
          return (Just $ hn { hSelf = mhc, hCsid = Just csid })
  case r of
    Nothing -> do
      logT $ "whokey err:" ++ show (parts,keyVal)
      return Nothing
    Just hn -> do
      let hn' = hn {hParts = Just parts}
      putHN hn'
      return (Just hn')

-- ---------------------------------------------------------------------

{-
function partsMatch(parts1, parts2)
{
  if(typeof parts1 != "object" || typeof parts2 != "object") return false;
  var ids = Object.keys(parts1).sort();
  var csid;
  while(csid = ids.pop()) if(parts2[csid]) return csid;
  return false;
}

-}

partsMatch :: Parts -> Parts -> Maybe String
partsMatch parts1 parts2 = r
  where
    ids = sort $ map fst parts1
    p2 = Set.fromList $ map fst parts2
    common = filter (\k -> Set.member k p2) ids
    r = if common == [] then Nothing
                        else Just $ head common

-- ---------------------------------------------------------------------

{-
  self.start = function(hashname,type,arg,cb)
  {
    var hn = self.whois(hashname);
    if(!hn) return cb("invalid hashname");
    return hn.start(type,arg,cb);
  }

-}
-- TODO: This is the switch level one, that just calls the hn one
-- was swStart
start :: HashContainer -> String -> a -> RxCallBack -> TeleHash Channel
start hashname typ arg cb = (assert False undefined)

-- ---------------------------------------------------------------------

-- |configures or checks
isBridge :: Maybe Bool -> Maybe HashName -> TeleHash Bool
isBridge arg mhn = do
  sw <- get
  if arg == Just True
    then put $ sw {swBridging = True}
    else return ()
  sw2 <- get
  if (swBridging sw2)
    then return True
    else do
      case mhn of
        Nothing -> return $ swBridging sw2
        Just hn -> do
          mhc <- whois hn
          case mhc of
            Nothing -> return False
            Just hc -> return (hBridging hc)

{-
// configures or checks
function isBridge(arg)
{
  var self = this;
  if(arg === true) self.bridging = true;
  if(self.bridging) return true;
  if(!arg) return self.bridging;

  var check = (typeof arg == "string")?self.whois(arg):arg;
  if(check && check.bridging) return true;
  return false;
}

-}
-- ---------------------------------------------------------------------
{-

function addSeed(arg) {
  var self = this;
  if(!arg.parts) return warn("invalid args to addSeed",arg);
  var seed = self.whokey(arg.parts,false,arg.keys);
  if(!seed) return warn("invalid seed info",arg);
  if(Array.isArray(arg.paths)) arg.paths.forEach(function(path){
    path = seed.pathGet(path);
    path.seed = true;
  });
  seed.isSeed = true;
  self.seeds.push(seed);
}
-}

-- was swAddSeed
addSeed :: SeedInfo -> TeleHash ()
addSeed arg = do
  logT $ "addSeed:args=" ++ show (sId arg)
  sw <- get
  mseed <- whokey (sParts arg) (Right (Map.fromList (sKeys arg)))
  -- logT $ "addSeed:mseed=" ++ show mseed
  case mseed of
    Nothing -> do
      logT $ "invalid seed info:" ++ show arg
      return ()
    Just seed -> do
      forM_ (sPaths arg) $ \path -> do
        void $ hnPathGet seed path
      sw' <- get
      put $ sw' { swSeeds = (swSeeds sw') ++ [(hHashName seed)] }


-- ---------------------------------------------------------------------

-- was swOnline
online :: CallBack -> TeleHash ()
online callback = do
  sw <- get
  if swWaits sw /= []
    then put $ sw {swWaiting = Just (online callback)}
    else do
      setIsOnline True
      -- ping lan
      token <- randomHEX 16
      setLanToken $ token

      -- TODO: send the lan packet too
      logT $ "online: must still send the lan token message"
      -- (swSend sw) (PathType "lan") (pencode Telex BL.empty) Nothing
      -- error "call swSend"

      case (swSeeds sw) of
        [] -> do
          logT "no seeds"
          callback
        seeds -> do
          let
            -- safely callback only once or when all seeds return
            done = (assert False undefined)
          forM_ seeds $ \ seed -> do
            logT $ "online:processing:" ++ show seed
            let fn hn = do
                  hc <- getHNsafe hn "online"
                  if hIsAlive hc
                    then hnSync hn
                    else return ()
                  done
            Just hcSeed <- whois seed
            hnLink (hHashName hcSeed) (Just fn)

{-

function online(callback)
{
  var self = this;
  if(self.waits.length > 0) return self.waiting = function(){self.online(callback)};
  self.isOnline = true;
  // ping lan
  self.lanToken = randomHEX(16);
  self.send({type:"lan"}, pencode({type:"lan",lan:self.lanToken,from:self.parts}));

  var dones = self.seeds.length;
  if(!dones) {
    warn("no seeds");
    return callback(null,0);
  }

  // safely callback only once or when all seeds return
  function done()
  {
    if(!dones) return; // already called back
    var alive = self.seeds.filter(function(seed){return seed.alive}).length;
    if(alive)
    {
      callback(null,alive);
      dones = 0;
      return;
    }
    dones--;
    // failed
    if(!dones) callback("offline",0);
  }

  self.seeds.forEach(function(seed){
    seed.link(function(){
      if(seed.alive) seed.sync();
      done();
    });
  });
}

-}
  return ()

-- ---------------------------------------------------------------------

-- | Set the swIsOnline flag
setIsOnline :: Bool -> TeleHash ()
setIsOnline v = do
  sw <- get
  put $ sw {swIsOnline = v}

-- | Set the swLanToken value
setLanToken :: String -> TeleHash ()
setLanToken v = do
  sw <- get
  put $ sw {swLanToken = Just v}

-- ---------------------------------------------------------------------
{-
  self.listen = function(type, callback){
    if(typeof type != "string" || typeof callback != "function") return warn("invalid arguments to listen");
    if(type.substr(0,1) !== "_") type = "_"+type;
    self.rels[type] = callback;
  };
-}

-- was swListen
listen :: String -> () -> IO ()
listen typ callback = (assert False undefined)

-- ---------------------------------------------------------------------

-- | Network layer. Send an `open` network packet to the given hashname
openize :: HashName -> TeleHash (Maybe LinePacket)
openize toHn = do
  to <- getHNsafe toHn "openize"
  sw <- get
  case hCsid to of
    Nothing -> do
      logT $ "can't open without key"
      return Nothing
    Just _ -> do
      timeNow <- io getClockTime
      let
        to2 = if hLineAt to == Nothing
                then to {hLineAt = Just timeNow }
                else to
      putHN to2
      let inner = OpenizeInner
            { oiAt = gfromJust "openize" $ hLineAt to2
            , oiTo = hHashName to2
            , oiFrom = swParts sw
            , oiLine = hLineOut to2
            }
      lp <- crypt_openize_1a to2 inner
      return (Just lp)


{-
function openize(self, to)
{
  if(!to.csid)
  {
    console.log("can't open w/ no key");
    return undefined;
  }
  if(!to.lineOut) to.lineOut = randomHEX(16);
  if(!to.lineAt) to.lineAt = Date.now();
  var inner = {}
  inner.at = to.lineAt; // always the same for the generated line id/key
  inner.to = to.hashname;
  inner.from = self.parts;
  inner.line = to.lineOut;
  return self.CSets[to.csid].openize(self, to, inner);
}


-}

-- ---------------------------------------------------------------------

deopenize :: NetworkTelex -> TeleHash DeOpenizeResult
deopenize open = do
  let p = ntPacket open
  logT $ "DEOPEN :" ++ show (networkPacketLen p)

  case p of
    LinePacket _ -> return DeOpenizeVerifyFail
    OpenPacket csHex pbody -> do
      sw <- get
      let csid = BC.unpack $ B16.encode (BC.pack [w2c csHex])
      -- logT $ "deopenize got cs:" ++ csid
      case Map.lookup csid (swCSets sw) of
        Nothing -> return DeOpenizeVerifyFail
        Just cs -> do
          ret <- (csDeopenize cs) p
          return ret


{-

function deopenize(self, open)
{
//  console.log("DEOPEN",open.body.length);
  var ret;
  var csid = open.head.charCodeAt().toString(16);
  if(!self.CSets[csid]) return {err:"unknown CSID of "+csid};
  try{ret = self.CSets[csid].deopenize(self, open);}catch(E){return {err:E};}
  ret.csid = csid;
  return ret;
}


-}

-- ---------------------------------------------------------------------

{-
function deopenize(self, open)
{
//  console.log("DEOPEN",open.body.length);
  var ret;
  var csid = open.head.charCodeAt().toString(16);
  if(!self.CSets[csid]) return {err:"unknown CSID of "+csid};
  try{ret = self.CSets[csid].deopenize(self, open);}catch(E){return {err:E};}
  ret.csid = csid;
  return ret;
}


-}
-- ---------------------------------------------------------------------
{-
  self.raw = function(type, callback){
    if(typeof type != "string" || typeof callback != "function") return warn("invalid arguments to raw");
    self.raws[type] = callback;
  };

-}

-- raw :: String -> () -> IO ()
-- raw typ callback = (assert False undefined)

-- ---------------------------------------------------------------------

{-
-- from the c version

chan_t chan_new(switch_t s, hn_t to, char *type, uint32_t id)
{
  chan_t c;
  if(!s || !to || !type) return NULL;

  // use new id if none given
  if(!to->chanOut) chan_reset(s, to);
  if(!id)
  {
    id = to->chanOut;
    to->chanOut += 2;
  }

  DEBUG_PRINTF("channel new %d %s",id,type);
  c = malloc(sizeof (struct chan_struct));
  memset(c,0,sizeof (struct chan_struct));
  c->type = strdup(type);
  c->s = s;
  c->to = to;
  c->state = STARTING;
  c->id = id;
  util_hex((unsigned char*)&(s->uid),4,(unsigned char*)c->uid); // switch-wide unique id
  s->uid++;
  util_hex((unsigned char*)&(c->id),4,(unsigned char*)c->hexid);
  if(!to->chans) to->chans = xht_new(17);
  xht_set(to->chans,(char*)c->hexid,c);
  xht_set(s->index,(char*)c->uid,c);
  return c;
}

-}

-- create an unreliable channel
-- was swRaw
raw :: HashContainer -> String -> Telex -> RxCallBack -> TeleHash Channel
raw hn typ arg callback = do
  sw <- get
  (hn',chanId) <- case tChanId arg of
    Just i  -> return (hn,i)
    Nothing -> return (hn { hChanOut = (hChanOut hn) + 2},hChanOut hn)
  let
    chan = Chan { chType = typ
                , chCallBack = callback
                , chId = chanId
                , chHashName = hHashName hn
                , chLast = Nothing
                , chSentAt = Nothing
                , chRxAt = Nothing
                , chEnded = False
                , chDone = False
                }
  putHN hn'
  putChan (hHashName hn) chan
  hn2 <- getHNsafe (hHashName hn) "raw"

  logT "raw:must implement timeout"

  -- debug("new unreliable channel",hn.hashname,chan.type,chan.id);
  logT $ "new unreliable channel" ++ show (hHashName hn,chType chan,chId chan)

  if not (HM.null (tJson arg))
    then do
      -- let msg = rxTelexToTelex arg
      let msg = arg
      chanSendRaw hn2 chan msg
    else return ()

  -- WIP: carry on here with the send
{-
  // send optional initial packet with type set
  if(arg.js)
  {
    arg.js.type = type;
    chan.send(arg);
    // retry if asked to, TODO use timeout for better time
    if(arg.retry)
    {
      var at = 1000;
      function retry(){
        if(chan.ended || chan.recvAt) return; // means we're gone or received a packet
        chan.send(arg);
        if(at < 4000) at *= 2;
        arg.retry--;
        if(arg.retry) setTimeout(retry, at);
      };
      setTimeout(retry, at);
    }
  }
-}
  logT "raw not implemented"
  return chan

{-

// create an unreliable channel
function raw(type, arg, callback)
{
  var hn = this;
  var chan = {type:type, callback:callback};
  chan.id = arg.id;
  if(!chan.id)
  {
    chan.id = hn.chanOut;
    hn.chanOut += 2;
  }
  hn.chans[chan.id] = chan;

  // raw channels always timeout/expire after the last sent/received packet
  if(!arg.timeout) arg.timeout = defaults.chan_timeout;
  function timer()
  {
    if(chan.timer) clearTimeout(chan.timer);
    chan.timer = setTimeout(function(){
      chan.fail({js:{err:"timeout"}});
    }, arg.timeout);
  }
  chan.timeout = function(timeout)
  {
    arg.timeout = timeout;
    timer();
  }

  chan.hashname = hn.hashname; // for convenience

  debug("new unreliable channel",hn.hashname,chan.type,chan.id);



  // send optional initial packet with type set
  if(arg.js)
  {
    arg.js.type = type;
    chan.send(arg);
    // retry if asked to, TODO use timeout for better time
    if(arg.retry)
    {
      var at = 1000;
      function retry(){
        if(chan.ended || chan.recvAt) return; // means we're gone or received a packet
        chan.send(arg);
        if(at < 4000) at *= 2;
        arg.retry--;
        if(arg.retry) setTimeout(retry, at);
      };
      setTimeout(retry, at);
    }
  }

  return chan;
}
-}

-- ---------------------------------------------------------------------

-- !process packets at a raw level, very little to do
chanReceive :: HashContainer -> Channel -> RxTelex -> TeleHash ()
chanReceive hn chan packet = do
  logT $ "chanReceive on " ++ show (hHashName hn,chId chan)
  case (Map.lookup (chId chan) (hChans hn)) of
    Nothing -> do
      logT $ "dropping receive packet to dead channel" ++ show (chId chan,rtJs packet)
      return ()
    Just _ -> do
      --  if err'd or ended, delete ourselves
      let errOrFail = (HM.member "err" (rtJs packet)) || (HM.member "end" (rtJs packet))
      if errOrFail
        then do
          chanFail (hHashName hn) chan Nothing
        else return ()
      -- cache last received network
      timeNow <- io getClockTime
      let chan' = chan { chLast = Just (rtSender packet)
                       , chRxAt = Just timeNow
                       }
      putChan (hHashName hn) chan'
      (chCallBack chan') errOrFail packet chan'
      logT $ "chanReceive:must do timer()"

{-
  // process packets at a raw level, very little to do
  chan.receive = function(packet)
  {
    if(!hn.chans[chan.id]) return debug("dropping receive packet to dead channel",chan.id,packet.js)
    // if err'd or ended, delete ourselves
    if(packet.js.err || packet.js.end) chan.fail();
    chan.last = packet.sender; // cache last received network
    chan.recvAt = Date.now();
    chan.callback(packet.js.err||packet.js.end, packet, chan);
    timer();
  }
-}
-- ---------------------------------------------------------------------

-- | minimal wrapper to send raw packets
chanSendRaw :: HashContainer -> Channel -> Telex -> TeleHash ()
chanSendRaw hn chan packet = do
  logT $ "chanSendRaw entered for " ++ show (chId chan)
  sw <- get
  case Map.lookup (chId chan) (hChans hn) of
    Nothing -> do
      logT $ "dropping send packet to dead channel " ++ show (chId chan,packet)
    Just ch -> do
      logT $ "chanSendRaw got chan to:" ++ show (chHashName ch)
      let js = showJson $ HM.insert "c" (Number $ fromIntegral $ unChannelId (chId chan)) (tJson packet)
      logT $ "SEND " ++ show (chType chan) ++ "," ++ js
      timeNow <- io getClockTime
      let ch' = ch { chSentAt = Just timeNow }
      putChan (hHashName hn) ch'
      let packet' =
           case tTo packet of
             Just _ -> packet
             Nothing -> -- always send back to the last received for this channel
                        if pathValid timeNow (chLast ch')
                          then packet { tTo = chLast ch' }
                          else packet
      sentChan <- hnSend hn packet'
      if HM.member "err" (tJson packet') || HM.member "end" (tJson packet')
        then chanFail (hHashName hn) ch' Nothing
        else return ()
      chanTimer ch'

{-
  // minimal wrapper to send raw packets
  chan.send = function(packet)
  {
    if(!hn.chans[chan.id]) return debug("dropping send packet to dead channel",chan.id,packet.js);
    if(!packet.js) packet.js = {};
    packet.js.c = chan.id;
    debug("SEND",chan.type,JSON.stringify(packet.js));
    chan.sentAt = Date.now();
    if(!packet.to && pathValid(chan.last)) packet.to = chan.last; // always send back to the last received for this channel
    hn.send(packet);
    // if err'd or ended, delete ourselves
    if(packet.js.err || packet.js.end) chan.fail();
    timer();
  }


-}

-- ---------------------------------------------------------------------


chanFail :: HashName -> Channel -> Maybe RxTelex -> TeleHash ()
chanFail hn chan mpacket = do
  if chEnded chan
    then return ()
    else do
      hnChanDone hn (chId chan)
      case mpacket of
        Nothing -> return ()
        Just packet -> do
          (chCallBack chan) True packet chan

{-
  chan.fail = function(packet){
    if(chan.ended) return; // prevent multiple calls
    hn.chanDone(chan.id);
    chan.ended = true;
    if(packet)
    {
      packet.from = hn;
      chan.callback(packet.js.err, packet, chan, function(){});
    }
  }
-}

hnChanDone :: HashName -> ChannelId -> TeleHash ()
hnChanDone hn chid = do
  mchan <- getChan hn chid
  case mchan of
    Nothing -> do
      logT $ "hnChanDone: channel not found:" ++ show chid
      return ()
    Just chan -> do
      putChan hn (chan { chDone = True })

{-
  hn.chanDone = function(id)
  {
    hn.chans[id] = false;
  }
-}

chanTimer :: Channel -> TeleHash ()
chanTimer chan = do
  logT $ "chanTimer unimplemented"
  return ()


-- ---------------------------------------------------------------------

-- |validate if a network path is acceptable to stop at
pathValid :: ClockTime -> Maybe Path -> Bool
pathValid _ Nothing = False
pathValid timeNow (Just path) =
  if pGone path
    then False
    else
      if (pathType path == PtRelay) && pRelay path == Nothing
        then True -- active relays are always valid
        else
          if pLastIn path == Nothing
            then False -- all the rest must have received to be valid
            else (not $ isTimeOut timeNow (pLastIn path) (natTimeout defaults))
{-
// validate if a network path is acceptable to stop at
function pathValid(path)
{
  if(!path || path.gone) return false;
  if(path.type == "relay" && !path.relay.ended) return true; // active relays are always valid
  if(!path.lastIn) return false; // all else must receive to be valid
  if(Date.now() - path.lastIn < defaults.nat_timeout) return true; // received anything recently is good
  return false;
}

-}
-- ---------------------------------------------------------------------

{-
// create a reliable channel with a friendlier interface
function channel(type, arg, callback)
{
  var hn = this;
  var chan = {inq:[], outq:[], outSeq:0, inDone:-1, outConfirmed:-1, lastAck:-1, callback:callback};
  chan.id = arg.id;
  if(!chan.id)
  {
    chan.id = hn.chanOut;
    hn.chanOut += 2;
  }
  hn.chans[chan.id] = chan;
  chan.timeout = arg.timeout || defaults.chan_timeout;
  // app originating if not bare, be friendly w/ the type, don't double-underscore if they did already
  if(!arg.bare && type.substr(0,1) !== "_") type = "_"+type;
  chan.type = type; // save for debug
  if(chan.type.substr(0,1) != "_") chan.safe = true; // means don't _ escape the json
  chan.hashname = hn.hashname; // for convenience

  debug("new channel",hn.hashname,chan.type,chan.id);

  // used by app to change how it interfaces with the channel
  chan.wrap = function(wrap)
  {
    if(!channelWraps[wrap]) return false;
    return channelWraps[wrap](chan);
  }

  // called to do eventual cleanup
  chan.done = function(){
    if(chan.ended) return; // prevent multiple calls
    chan.ended = true;
    debug("channel done",chan.id);
    hn.chanDone(chan.id);
  };

  // used to internally fail a channel, timeout or connection failure
  chan.fail = function(packet){
    if(chan.errored) return; // prevent multiple calls
    chan.errored = packet;
    packet.from = hn;
    chan.callback(packet.js.err, packet, chan, function(){});
    chan.done();
  }

  // simple convenience wrapper to end the channel
  chan.end = function(){
    chan.send({end:true});
    chan.done();
  };

  // errors are hard-send-end
  chan.err = function(err){
    if(chan.errored) return;
    chan.errored = {js:{err:err,c:chan.id}};
    hn.send(chan.errored);
    chan.done();
  };

  // process packets at a raw level, handle all miss/ack tracking and ordering
  chan.receive = function(packet)
  {
    // if it's an incoming error, bail hard/fast
    if(packet.js.err) return chan.fail(packet);

    // in errored state, only/always reply with the error and drop
    if(chan.errored) return chan.send(chan.errored);
    chan.lastIn = Date.now();

    // process any valid newer incoming ack/miss
    var ack = parseInt(packet.js.ack);
    if(ack > chan.outSeq) return warn("bad ack, dropping entirely",chan.outSeq,ack);
    var miss = Array.isArray(packet.js.miss) ? packet.js.miss : [];
    if(miss.length > 100) {
      warn("too many misses", miss.length, chan.id, packet.from.hashname);
      miss = miss.slice(0,100);
    }
    if(miss.length > 0 || ack > chan.lastAck)
    {
      debug("miss processing",ack,chan.lastAck,miss,chan.outq.length);
      chan.lastAck = ack;
      // rebuild outq, only keeping newer packets, resending any misses
      var outq = chan.outq;
      chan.outq = [];
      outq.forEach(function(pold){
        // packet acknowleged!
        if(pold.js.seq <= ack) {
          if(pold.callback) pold.callback();
          return;
        }
        chan.outq.push(pold);
        if(miss.indexOf(pold.js.seq) == -1) return;
        // resend misses but not too frequently
        if(Date.now() - pold.resentAt < 1000) return;
        pold.resentAt = Date.now();
        chan.ack(pold);
      });
    }

    // don't process packets w/o a seq, no batteries included
    var seq = packet.js.seq;
    if(!(seq >= 0)) return;

    // auto trigger an ack in case none were sent
    if(!chan.acker) chan.acker = setTimeout(function(){ delete chan.acker; chan.ack();}, defaults.chan_autoack);

    // drop duplicate packets, always force an ack
    if(seq <= chan.inDone || chan.inq[seq-(chan.inDone+1)]) return chan.forceAck = true;

    // drop if too far ahead, must ack
    if(seq-chan.inDone > defaults.chan_inbuf)
    {
      warn("chan too far behind, dropping", seq, chan.inDone, chan.id, packet.from.hashname);
      return chan.forceAck = true;
    }

    // stash this seq and process any in sequence, adjust for yacht-based array indicies
    chan.inq[seq-(chan.inDone+1)] = packet;
    debug("INQ",Object.keys(chan.inq),chan.inDone,chan.handling);
    chan.handler();
  }

  // wrapper to deliver packets in series
  chan.handler = function()
  {
    if(chan.handling) return;
    var packet = chan.inq[0];
    // always force an ack when there's misses yet
    if(!packet && chan.inq.length > 0) chan.forceAck = true;
    if(!packet) return;
    chan.handling = true;
    if(!chan.safe) packet.js = packet.js._ || {}; // unescape all content json
    chan.callback(packet.js.end, packet, chan, function(ack){
      // catch whenever it was ended to start cleanup
      if(packet.js.end) chan.endIn = true;
      if(chan.endOut && chan.endIn) chan.done();
      chan.inq.shift();
      chan.inDone++;
      chan.handling = false;
      if(ack) chan.ack(); // auto-ack functionality
      chan.handler();
    });
  }

  // resend the last sent packet if it wasn't acked
  chan.resend = function()
  {
    if(chan.ended) return;
    if(!chan.outq.length) return;
    var lastpacket = chan.outq[chan.outq.length-1];
    // timeout force-end the channel
    if(Date.now() - lastpacket.sentAt > chan.timeout)
    {
      chan.fail({js:{err:"timeout"}});
      return;
    }
    debug("channel resending");
    chan.ack(lastpacket);
    setTimeout(chan.resend, defaults.chan_resend); // recurse until chan_timeout
  }

  // add/create ack/miss values and send
  chan.ack = function(packet)
  {
    if(!packet) debug("ACK CHECK",chan.id,chan.outConfirmed,chan.inDone);

    // these are just empty "ack" requests
    if(!packet)
    {
      // drop if no reason to ack so calling .ack() harmless when already ack'd
      if(!chan.forceAck && chan.outConfirmed == chan.inDone) return;
      packet = {js:{}};
    }
    chan.forceAck = false;

    // confirm only what's been processed
    if(chan.inDone >= 0) chan.outConfirmed = packet.js.ack = chan.inDone;

    // calculate misses, if any
    delete packet.js.miss; // when resending packets, make sure no old info slips through
    if(chan.inq.length > 0)
    {
      packet.js.miss = [];
      for(var i = 0; i < chan.inq.length; i++)
      {
        if(!chan.inq[i]) packet.js.miss.push(chan.inDone+i+1);
      }
    }

    // now validate and send the packet
    packet.js.c = chan.id;
    debug("SEND",chan.type,JSON.stringify(packet.js));
    hn.send(packet);

    // catch whenever it was ended to start cleanup
    if(packet.js.end) chan.endOut = true;
    if(chan.endOut && chan.endIn) chan.done();
  }

  // send content reliably
  chan.send = function(arg)
  {
    if(chan.ended) return warn("can't send to an ended channel");

    // create a new packet from the arg
    if(!arg) arg = {};
    var packet = {};
    packet.js = chan.safe ? arg.js : {_:arg.js};
    if(arg.type) packet.js.type = arg.type;
    if(arg.end) packet.js.end = arg.end;
    packet.body = arg.body;
    packet.callback = arg.callback;

    // do durable stuff
    packet.js.seq = chan.outSeq++;

    // reset/update tracking stats
    packet.sentAt = Date.now();
    chan.outq.push(packet);

    // add optional ack/miss and send
    chan.ack(packet);

    // to auto-resend if it isn't acked
    if(chan.resender) clearTimeout(chan.resender);
    chan.resender = setTimeout(chan.resend, defaults.chan_resend);
    return chan;
  }

  // send optional initial packet with type set
  if(arg.js)
  {
    arg.type = type;
    chan.send(arg);
  }

  return chan;
}


-}


-- ---------------------------------------------------------------------

inPeer :: Bool -> RxTelex -> Channel -> TeleHash ()
inPeer = (assert False undefined)
{-
// be the middleman to help NAT hole punch
function inPeer(err, packet, chan)
{
  if(err) return;
  var self = packet.from.self;
  if(chan.relay) return relay(self, chan, chan.relay, packet);

  if(!isHEX(packet.js.peer, 64)) return;
  var peer = self.whois(packet.js.peer);
  if(!peer || !peer.lineIn) return; // these happen often as lines come/go, ignore dead peer requests
  var js = {from:packet.from.parts};

  // sanity on incoming paths array
  if(!Array.isArray(packet.js.paths)) packet.js.paths = [];

  // insert in incoming IP path
  if(packet.sender.type.indexOf("ip") == 0) packet.js.paths.push(packet.sender.json);

  // load/cleanse all paths
  js.paths = [];
  packet.js.paths.forEach(function(path){
    if(typeof path.type != "string") return;
    if(pathMatch(js.paths,path)) return; // duplicate
    if(isLocalPath(path) && !peer.isLocal) return; // don't pass along local paths to public
    js.paths.push(path);
  });

  // must bundle the senders key so the recipient can open them
  chan.relay = peer.raw("connect",{js:js, body:packet.body},function(err, packet, chan2){
    if(err) return;
    relay(self, chan2, chan, packet);
  });
}
-}

-- ---------------------------------------------------------------------

inConnect :: Bool -> RxTelex -> Channel -> TeleHash ()
inConnect = (assert False undefined)

{-
// someone's trying to connect to us, send an open to them
function inConnect(err, packet, chan)
{
  if(err || !packet.body) return;
  var self = packet.from.self;

  // if this channel is acting as a relay
  if(chan.relay)
  {
    // create a virtual network path that maps back to this channel
    var path = {type:"relay",relay:chan,json:{type:"relay",relay:packet.from.hashname}};
    if(packet.js.bridge) path = packet.sender; // sender is offering to bridge, use them!
    self.receive(packet.body, path);
    return;
  }

  var to = chan.relay = self.whokey(packet.js.from,packet.body);
  if(!chan.relay) return warn("invalid connect request from",packet.from.hashname,packet.js);

  // try the suggested paths
  if(Array.isArray(packet.js.paths)) packet.js.paths.forEach(function(path){
    if(typeof path.type != "string") return debug("bad path",JSON.stringify(path));
    self.send(path,to.open(),to);
  });

  // send back an open through the connect too
  chan.send({body:to.open()});
}

-}
-- ---------------------------------------------------------------------

inSeek :: Bool -> RxTelex -> Channel -> TeleHash ()
inSeek = (assert False undefined)
{-

// return a see to anyone closer
function inSeek(err, packet, chan)
{
  if(err) return;
  if(!isHEX(packet.js.seek)) return warn("invalid seek of ", packet.js.seek, "from:", packet.from.hashname);
  var self = packet.from.self;
  var seek = packet.js.seek;

  var see = [];
  var seen = {};

  // see if we have any seeds to add
  var bucket = dhash(self.hashname, packet.js.seek);
  var links = self.buckets[bucket] ? self.buckets[bucket] : [];

  // first, sort by age and add the most wise one
  links.sort(function(a,b){ return a.age - b.age}).forEach(function(seed){
    if(see.length) return;
    if(!seed.seed) return;
    see.push(seed.address(packet.from));
    seen[seed.hashname] = true;
  });

  // sort by distance for more
  links.sort(function(a,b){ return dhash(seek,a.hashname) - dhash(seek,b.hashname)}).forEach(function(link){
    if(seen[link.hashname]) return;
    if(link.seed || link.hashname.substr(seek.length) == seek)
    {
      see.push(link.address(packet.from));
      seen[link.hashname] = true;
    }
  });

  var answer = {end:true, see:see.filter(function(x){return x}).slice(0,8)};
  chan.send({js:answer});
}
-}

-- ---------------------------------------------------------------------


-- parseMaybe :: (a -> Parser b) -> a -> Maybe b
-- fromJSON :: FromJSON a => Value -> Result a
{-
data Result a

  The result of running a Parser.

Constructors
   Error String
   Success a

-}

-- update/respond to network state
inPath :: Bool -> RxTelex -> Channel -> TeleHash ()
inPath err packet chan = do
  logT $ "inPath:" ++ show (err,chId chan) ++ showPath (rtSender packet)
  hn <- getHNsafe (chHashName chan) "inPath"

  -- check/try any alternate paths
  case (HM.lookup "paths" (rtJs packet)) of
    Nothing -> return ()
    Just p -> do
      let mp = (fromJSON p) :: Result [PathJson] -- :: Result [PathJson]
      case mp of
        Error err -> do
          logT $ "inPath: could not parse paths:" ++ err
          return ()
        Success paths -> do
          -- logT $ "inPath:packet=" ++ show packet
          forM_ paths $ \pathJson -> do
            let path = pathFromPathJson pathJson
            case pathMatch path (hPaths hn) of
              Just _ -> return ()
              Nothing -> do
                -- a new one, experimentally send it a path
                logT $ "inPath: must still build alternate path:" ++ show path
                -- packet.from.raw("path",{js:{priority:1},to:path}, inPath);
                let msg = packet { rtJs = HM.empty }
                -- logT $ "inPath:sending 1" ++ (show $ rxTelexToTelex msg)
                void $ raw hn "path" (rxTelexToTelex msg) inPath
            return ()

  -- if path info from a seed, update our public ip/port
  if hIsSeed hn
    then do
      case HM.lookup "path" (rtJs packet) of
        Nothing -> return ()
        Just p -> do
          let mp = (fromJSON p) :: Result PathJson -- :: Result [PathJson]
          case mp of
            Error err -> do
              logT $ "inPath: could not parse path:" ++ err
              return ()
            Success pj@(PIPv4 (PathIPv4 ip _)) -> do
              if not (isLocalIP ip)
                then do
                  sw <- get
                  logT $ "updating public ipv4" ++ show (swPub4 sw,ip)
                  pathSet (pathFromPathJson pj)
                else return ()
            Success _ -> return ()
    else return ()

  -- update any optional priority information
  case HM.lookup "priority" (rtJs packet) of
    Nothing -> return ()
    Just (Number p) -> do
      putHN $ hn { hPriority = Just (round p)}
      logT $ "inPath:must still adjust relative priorities. Once it clarifies."
      -- if(packet.from.to && packet.sender.priority > packet.from.to.priority) packet.from.to = packet.sender; // make the default!
  if err
    then return () -- bye bye bye!
    else do
      -- need to respond, prioritize everything above relay
      let priority = 2
      logT $ "inPath: must still prioritise over relay"
      -- var priority = (packet.sender.type == "relay") ? 0 : 2;

      hn2 <- getHNsafe (hHashName hn) "inPath"
      let rxPathJson = HM.lookupDefault (Object HM.empty) "path" (rtJs packet)
          msg1 = rxTelexToTelex packet
          msg2 = msg1 { tJson = HM.fromList [("end",toJSON True)
                                            ,("priority",Number priority)
                                            ,("path", rxPathJson)
                                            ] }
          msg3 = msg2 { tTo = Just (rtSender packet) }
      -- chan.send({js:{end:true, priority:priority, path:packet.sender.json}});
      -- logT $ "inPath:sending 2" ++ (show msg3)
      chanSendRaw hn2 chan msg3

{-
// update/respond to network state
function inPath(err, packet, chan)
{
  var self = packet.from.self;

  // check/try any alternate paths
  if(Array.isArray(packet.js.paths)) packet.js.paths.forEach(function(path){
    if(typeof path.type != "string") return; // invalid
    // don't send to ones we know about
    if(pathMatch(path, packet.from.paths)) return;
    // a new one, experimentally send it a path
    packet.from.raw("path",{js:{priority:1},to:path}, inPath);
  });

  // if path info from a seed, update our public ip/port
  if(packet.from.isSeed && typeof packet.js.path == "object" && packet.js.path.type == "ipv4" && !isLocalIP(packet.js.path.ip))
  {
    debug("updating public ipv4",JSON.stringify(self.paths.pub4),JSON.stringify(packet.js.path));
    self.pathSet({type:"pub4", ip:packet.js.path.ip, port:parseInt(packet.js.path.port)})
  }

  // update any optional priority information
  if(typeof packet.js.priority == "number"){
    packet.sender.priority = packet.js.priority;
    if(packet.from.to && packet.sender.priority > packet.from.to.priority) packet.from.to = packet.sender; // make the default!
  }

  if(err) return; // bye bye bye!

  // need to respond, prioritize everything above relay
  var priority = (packet.sender.type == "relay") ? 0 : 2;

  // if bridging, and this path is from the bridge, flag it for lower priority
  if(packet.from.bridge && pathMatch(packet.sender, self.whois(packet.from.bridge).paths)) priority = 1;

  chan.send({js:{end:true, priority:priority, path:packet.sender.json}});
}

-}

-- ---------------------------------------------------------------------

inBridge :: Bool -> RxTelex -> Channel -> TeleHash ()
inBridge = (assert False undefined)

{-
// handle any bridge requests, if allowed
function inBridge(err, packet, chan)
{
  if(err) return;
  var self = packet.from.self;

  // ensure valid request
  if(!isHEX(packet.js.to,32) || !isHEX(packet.js.from,32) || typeof packet.js.path != "object") return warn("invalid bridge request",JSON.stringify(packet.js),packet.from.hashname);

  // must be allowed either globally or per hashname
  if(!self.bridging && !packet.from.bridging) return chan.send({js:{err:"not allowed"}});

  // don't bridge for types we don't know
  if(!self.networks[packet.js.path.type]) return chan.send({js:{err:"bad path"}});

  // ignore fool line ids
  if(self.lines[packet.js.to] || self.lines[packet.js.from]) return chan.send({js:{err:"bad line"}});

  // set up the actual bridge paths
  debug("BRIDGEUP",JSON.stringify(packet.js));
  self.bridgeLine[packet.js.to] = packet.js.path;
  self.bridgeLine[packet.js.from] = packet.sender;

  chan.send({js:{end:true}});
}

-}

-- ---------------------------------------------------------------------

inLink :: Bool -> RxTelex -> Channel -> TeleHash ()
inLink = (assert False undefined)

{-

// accept a dht link
function inLink(err, packet, chan)
{
  if(err) return;
  var self = packet.from.self;
  chan.timeout(defaults.nat_timeout*2); // two NAT windows to be safe

  // add in this link
  if(!packet.from.age) packet.from.age = Date.now();
  packet.from.linked = chan;
  packet.from.seed = packet.js.seed;
  if(self.buckets[packet.from.bucket].indexOf(packet.from) == -1) self.buckets[packet.from.bucket].push(packet.from);

  // send a response if this is a new incoming
  if(!chan.sentAt) packet.from.link();

  // look for any see and check to see if we should create a link
  if(Array.isArray(packet.js.see)) packet.js.see.forEach(function(address){
    if(!address) return; // garbage
    var hn = self.whois(address);
    if(!hn || hn.linked) return;
    if(self.buckets[hn.bucket].length < defaults.link_k) hn.link();
  });

  // check for bridges
  if(Array.isArray(packet.js.bridges)) packet.js.bridges.forEach(function(type){
    if(!self.bridges[type]) self.bridges[type] = {};
    self.bridges[type][packet.from.hashname] = Date.now();
  });

  // let mainteanance handle
  chan.callback = inMaintenance;
}

-}

-- ---------------------------------------------------------------------

{-

From https://github.com/telehash/telehash.org/blob/master/switch.md#seek

The response is a compact "see":[...] array of addresses that are
closest to the hash value (based on the DHT rules). The addresses are
a compound comma-delimited string containing the "hash,cs,ip,port"
(these are intentionally not JSON as the verbosity is not helpful
here), for example
"1700b2d3081151021b4338294c9cec4bf84a2c8bdf651ebaa976df8cff18075c,1a,123.45.67.89,10111".
The "cs" is the Cipher Set ID and is required. The ip and port parts
are optional and only act as hints for NAT hole punching.

-}

hnAddress :: HashName -> HashName -> TeleHash Aeson.Value
hnAddress hn to = assert False undefined
{-

  // return our address to them
  hn.address = function(to)
  {
    if(!to) return "";
    var csid = partsMatch(hn.parts,to.parts);
    if(!csid) return "";
    if(!hn.ip) return [hn.hashname,csid].join(",");
    return [hn.hashname,csid,hn.ip,hn.port].join(",");
  }



-}
-- ---------------------------------------------------------------------

-- |DHT action: request to be stored on their side
hnLink :: HashName -> Maybe HnCallBack -> TeleHash ()
hnLink hn mcb = do
  sw <- get
  hc <- getHNsafe hn "hnLink"

  let callback = case mcb of
                   Just cb -> cb
                   Nothing -> nullHnCb

  -- TODO:
    -- Set the JS 'see' value to
      -- sort the buckets by age
      -- pull out the seed values
      -- for each seed get the address associated with this hn
      -- take the first 8 -- (0,8)
  let buckets = (Map.findWithDefault [] (hBucket hc) (swBuckets sw))
      ageComp a b = compare (lineAge a) (lineAge b)
      buckets2 = sortBy ageComp buckets
      seeds = map lineSeed buckets2
  see1 <- mapM (hnAddress hn) seeds
  logT $ "hnLink:see1=" ++ show see1

  -- add some distant ones if none or too few
  let allBuckets = Set.fromList $ concat $ Map.elems (swBuckets sw)
      buckets3 = Set.fromList $ take 8 buckets2
      allOtherBuckets = sortBy ageComp $ Set.toList (allBuckets Set.\\ buckets3)
  let see = take 8 (buckets2 ++ allOtherBuckets)
  logT $ "hnLink:see=" ++ show see
  seeVal <- mapM (hnAddress hn) $ map lineSeed see

  -- TODO: sort out relay/bridge
  isBr <- isBridge Nothing (Just hn)
  let  toBridge :: PathType -> [Aeson.Value]
       toBridge (PtRelay) = ["relay"]
       toBridge (PtLocal) = ["local"]
       toBridge _ = []

       bridges = concatMap toBridge $ nub
                   $ filter (\pt -> pt == PtRelay || pt == PtLocal)
                   $ Map.keys (swNetworks sw)

       brVals
        = if isBr
            then [("bridges",Array $ V.fromList bridges)]
            else []

  let js = HM.fromList $ ([("seed", toJSON (hIsSeed hc))
                          ,("see",Aeson.Array $ V.fromList seeVal)]
                          ++ brVals
                         )
  let msg = emptyTelex
             {
               tJson = js
             }

  case hLinked hc of
    Just linkHn -> do
      linkHc <- getHNsafe linkHn "hnLink.2"
      hnSend linkHc msg
      callback hn
    Nothing -> do
      let rawCb err packet chan = do
            inLink err packet chan
            -- TODO: return packet.js.err in the callback
            callback hn
      logT $ "hnLink: must set retries"
      c <- raw hc "link" msg rawCb
      logT $ "link: raw returned c=" ++ show c
      return ()

{-

  // request a new link to them
  hn.link = function(callback)
  {
    if(!callback) callback = function(){}

    var js = {seed:self.seed};
    js.see = self.buckets[hn.bucket].sort(function(a,b){ return a.age - b.age }).filter(function(a){ return a.seed }).map(function(seed){ return seed.address(hn) }).slice(0,8);
    // add some distant ones if none
    if(js.see.length < 8) Object.keys(self.buckets).forEach(function(bucket){
      if(js.see.length >= 8) return;
      self.buckets[bucket].sort(function(a,b){ return a.age - b.age }).forEach(function(seed){
        if(js.see.length >= 8 || !seed.seed || js.see.indexOf(seed.address(hn)) != -1) return;
        js.see.push(seed.address(hn));
      });
    });

    if(self.isBridge(hn)) js.bridges = Object.keys(self.networks).filter(function(type){return (["local","relay"].indexOf(type) >= 0)?false:true});

    if(hn.linked)
    {
      hn.linked.send({js:js});
      return callback();
    }

    hn.raw("link", {retry:3, js:js}, function(err, packet, chan){
      inLink(err, packet, chan);
      callback(packet.js.err);
    });
  }

-}
-- ---------------------------------------------------------------------

-- was swSeek
seek :: String -> () -> IO ()
seek = (assert False undefined)

{-
// seek the dht for this hashname
function seek(hn, callback)
{
  var self = this;
  if(typeof hn == "string") hn = self.whois(hn);
  if(!callback) callback = function(){};
  if(!hn) return callback("invalid hashname");

  var did = {};
  var doing = {};
  var queue = [];
  var wise = {};
  var closest = 255;

  // load all seeds and sort to get the top 3
  var seeds = []
  Object.keys(self.buckets).forEach(function(bucket){
    self.buckets[bucket].forEach(function(link){
      if(link.hashname == hn) return; // ignore the one we're (re)seeking
      if(link.seed) seeds.push(link);
    });
  });
  seeds.sort(function(a,b){ return dhash(hn.hashname,a.hashname) - dhash(hn.hashname,b.hashname) }).slice(0,3).forEach(function(seed){
    wise[seed.hashname] = true;
    queue.push(seed.hashname);
  });

  debug("seek starting with",queue,seeds.length);

  // always process potentials in order
  function sort()
  {
    queue = queue.sort(function(a,b){
      return dhash(hn.hashname,a) - dhash(hn.hashname,b)
    });
  }

  // track when we finish
  function done(err)
  {
    // get all the hashnames we used/found and do final sort to return
    Object.keys(did).forEach(function(k){ if(queue.indexOf(k) == -1) queue.push(k); });
    Object.keys(doing).forEach(function(k){ if(queue.indexOf(k) == -1) queue.push(k); });
    sort();
    while(cb = hn.seeking.shift()) cb(err, queue.slice());
  }

  // track callback(s);
  if(!hn.seeking) hn.seeking = [];
  hn.seeking.push(callback);
  if(hn.seeking.length > 1) return;

  // main loop, multiples of these running at the same time
  function loop(onetime){
    if(!hn.seeking.length) return; // already returned
    debug("SEEK LOOP",queue);
    // if nothing left to do and nobody's doing anything, failed :(
    if(Object.keys(doing).length == 0 && queue.length == 0) return done("failed to find the hashname");

    // get the next one to ask
    var mine = onetime||queue.shift();
    if(!mine) return; // another loop() is still running

    // if we found it, yay! :)
    if(mine == hn.hashname) return done();
    // skip dups
    if(did[mine] || doing[mine]) return onetime||loop();
    var distance = dhash(hn.hashname, mine);
    if(distance > closest) return onetime||loop(); // don't "back up" further away
    if(wise[mine]) closest = distance; // update distance if trusted
    doing[mine] = true;
    var to = self.whois(mine);
    to.seek(hn.hashname, function(err, see){
      see.forEach(function(item){
        var sug = self.whois(item);
        if(!sug) return;
        // if this is the first entry and from a wise one, give them wisdom too
        if(wise[to.hashname] && see.indexOf(item) == 0) wise[sug.hashname] = true;
        sug.via(to, item);
        queue.push(sug.hashname);
      });
      sort();
      did[mine] = true;
      delete doing[mine];
      onetime||loop();
    });
  }

  // start three of them
  loop();loop();loop();

  // also force query any locals
  self.locals.forEach(function(local){loop(local.hashname)});
}

-}

-- ---------------------------------------------------------------------

-- was swBridge
bridge :: Path -> LinePacket -> Maybe HashContainer -> TeleHash ()
bridge = (assert False undefined)

{-
// try finding a bridge
function bridge(path, msg, to)
{
  var self = this;
  var packet = pdecode(msg);
  if(packet.head.length) return; // only bridge line packets
  if(!to) return; // require to for line info

  // check for existing bridge
  var existing = pathMatch(path,to.bridges);
  if(existing)
  {
    if(existing.bridged) return self.send(existing.bridged,msg); // leave off to to prevent loops
    existing.bridgeq = msg; // queue most recent packet;
    return;
  }

  if(!self.bridges[path.type]) return;
  debug("bridging",JSON.stringify(path.json),to.hashname);

  // TODO, better selection of a bridge?
  var via;
  Object.keys(self.bridges[path.type]).forEach(function(id){
    if(id == to.hashname) return; // lolz
    var hn = self.whois(id);
    if(hn.alive) via = hn;
  });

  if(!via) return debug("couldn't find a bridge host");

  // stash this so that any more bridge's don't spam
  if(!to.bridges) to.bridges = [];
  path.bridgeq = msg;
  to.bridges.push(path);

  // create the bridge
  via.raw("bridge", {js:{to:to.lineIn,from:to.lineOut,path:path}}, function(end, packet){
    // TODO we can try another one if failed?
    if(end !== true) return debug("failed to create bridge",end,via.hashname);
    // create our mapping!
    path.bridged = packet.sender;
    self.send(packet.sender,path.bridgeq);
    delete path.bridgeq;
  });
}

-}

-- ---------------------------------------------------------------------

-- |Encode the packet into a bytestring
-- |This should return the ByteString, ready for encryption
pencode :: Telex -> Body -> Telex
pencode = (assert False undefined)


{-
// encode a packet
function pencode(js, body)
{
  var head = (typeof js == "number") ? new Buffer(String.fromCharCode(js)) : new Buffer(js?JSON.stringify(js):"", "utf8");
  if(typeof body == "string") body = new Buffer(body, "binary");
  body = body || new Buffer(0);
  var len = new Buffer(2);
  len.writeInt16BE(head.length, 0);
  return Buffer.concat([len, head, body]);
}
-}

-- ---------------------------------------------------------------------

pdecode :: Packet -> (Telex,Body)
pdecode = (assert False undefined)

{-
// packet decoding
function pdecode(packet)
{
  if(!packet) return (assert False undefined);
  var buf = (typeof packet == "string") ? new Buffer(packet, "binary") : packet;

  // read and validate the json length
  var len = buf.readUInt16BE(0);
  if(len > (buf.length - 2)) return undefined;
  var head = buf.slice(2, len+2);
  var body = buf.slice(len + 2);

  // parse out the json
  var js = {};
  if(len > 1)
  {
    try {
      js = JSON.parse(head.toString("utf8"));
    } catch(E) {
      console.log("couldn't parse JS",head.toString("hex"),E,packet.sender);
      return undefined;
    }
  }
  return {js:js, length:buf.length, head:head.toString("binary"), body:body};
}


-}

-- ---------------------------------------------------------------------

pathMatch :: Path -> [Path] -> Maybe Path
pathMatch path1 paths = r
  where
    mtypes = filter (\p -> pathType path1 == pathType p) paths
    m :: Path -> Path -> Maybe Path
    m p1 p2
      | pathType p1 == PtRelay
         && pRelay p1 == pRelay p2 = Just p2

      | (pathType p1 == PtIPv4 ||
         pathType p1 == PtIPv6)
         && (pathIp p1 == pathIp p2)
         && (pathPort p1 == pathPort p2)
         = Just p2

      | pathType p1 == PtHttp
         && pathHttp p1 == pathHttp p2 = Just p2

      | pathType p1 == PtLocal
         && pId p1 == pId p2 = Just p2

      -- webrtc always matches
      | pathType p1 == PtWebRtc = Just p2

      | otherwise = Nothing

    r = case catMaybes $ map (m path1) mtypes of
          [] -> Nothing
          xs -> Just $ head xs

{-

function pathMatch(path1, paths)
{
  var match;
  if(!Array.isArray(paths)) return match;
  paths.forEach(function(path2){
    if(path2.type != path1.type) return;
    switch(path1.type)
    {
    case "relay":
      if(path1.relay == path2.relay) match = path2;
    case "ipv4":
    case "ipv6":
      if(path1.ip == path2.ip && path1.port == path2.port) match = path2;
      break;
    case "http":
      if(path1.http == path2.http) match = path2;
      break;
    case "local":
      if(path1.id == path2.id) match = path2;
      break;
    case "webrtc":
      match = path2; // always matches
      break;
    }
  });
  return match;
}

-}

-- ---------------------------------------------------------------------
{-

// validate if a network path is acceptable to stop at
function pathValid(path)
{
  if(!path || path.gone) return false;
  if(path.type == "relay" && !path.relay.ended) return true; // active relays are always valid
  if(!path.lastIn) return false; // all else must receive to be valid
  if(Date.now() - path.lastIn < defaults.nat_timeout) return true; // received anything recently is good
  return false;
}

-}

-- ---------------------------------------------------------------------
{-

function partsMatch(parts1, parts2)
{
  if(typeof parts1 != "object" || typeof parts2 != "object") return false;
  var ids = Object.keys(parts1).sort();
  var csid;
  while(csid = ids.pop()) if(parts2[csid]) return csid;
  return false;
}

-}

-- ---------------------------------------------------------------------

isLocalPath :: Path -> Bool
isLocalPath path
  | pathType path == PtBlueTooth = True
  | (pathType path == PtIPv4) ||
    (pathType path == PtIPv6) = isLocalIP (fromJust $ pathIp path)
  -- http?
  | otherwise = False

{-

function isLocalPath(path)
{
  if(!path || !path.type) return false;
  if(path.type == "bluetooth") return true;
  if(["ipv4","ipv6"].indexOf(path.type) >= 0) return isLocalIP(path.ip);
  // http?
  return false;
}

-}

-- ---------------------------------------------------------------------

isLocalIP :: IP -> Bool

{-

// return if an IP is local or public
function isLocalIP(ip)
{
  // ipv6 ones
  if(ip.indexOf(":") >= 0)
  {
    if(ip.indexOf("::") == 0) return true; // localhost
    if(ip.indexOf("fc00") == 0) return true;
    if(ip.indexOf("fe80") == 0) return true;
    return false;
  }

  var parts = ip.split(".");
  if(parts[0] == "0") return true;
  if(parts[0] == "127") return true; // localhost
  if(parts[0] == "10") return true;
  if(parts[0] == "192" && parts[1] == "168") return true;
  if(parts[0] == "172" && parts[1] >= 16 && parts[1] <= 31) return true;
  if(parts[0] == "169" && parts[1] == "254") return true; // link local
  return false;
}
-}

isLocalIP ip@(IPv4 _) = r
  where
    r127 =  makeAddrRange ((read "127.0.0.0")::IPv4) 8
    r10  =  makeAddrRange ((read "10.0.0.0")::IPv4) 8
    r192 =  makeAddrRange ((read "192.168.0.0")::IPv4) 16
    r172 =  makeAddrRange ((read "172.16.0.0")::IPv4) 9
    r169 =  makeAddrRange ((read "169.254.0.0")::IPv4) 16

    r = any (isMatchedTo (ipv4 ip)) [r127,r10,r192,r172,r169]

-- ---------------------------------------------------------------------

{-

function getkey(id, csid)
{
  return id.cs && id.cs[csid] && id.cs[csid].key;
}
-}
-- ---------------------------------------------------------------------

mkHashContainer :: HashName -> ClockTime -> String -> HashContainer
mkHashContainer hn timeNow randomHexVal =
  H { hHashName = hn
    , hChans = Map.empty
    , hSelf = Nothing
    , hPaths = []
    , hIsAlive = False
    , hIsPublic = False
    , hAt = timeNow
    , hBucket = -1
    , hChanOut = 0
    , hIsSeed = False
    , hTo = Nothing
    , hLineIn = Nothing
    , hLineAt = Nothing
    , hSendSeek = Nothing
    , hVias = Map.empty
    , hLastPacket = Nothing
    , hParts = Nothing
    , hOpened = Nothing
    , hOpenAt = Nothing
    , hRecvAt = Nothing
    , hCsid = Nothing
    , hPriority = Nothing

    , hIp = Nothing
    , hPort = Nothing
    , hBridging = False
    , hIsLocal = False
    , hLinked = Nothing

    , hLineOut = randomHexVal
    , hLineIV = 0
    , hEncKey = Nothing
    , hDecKey = Nothing
    , hEcc = Nothing
    }

-- ---------------------------------------------------------------------

{-
function loadkey(self, id, csid, key)
{
  id.csid = csid;
  return self.CSets[csid].loadkey(id, key);
}
-}

loadkey :: String -> String -> TeleHash (Maybe HashCrypto)
loadkey csid key = do
  sw <- get
  let set = Map.lookup csid (swCSets sw)
  case set of
    Nothing -> do
      logT $ "missing CSet for " ++ csid
      return Nothing
    Just cs -> do
      (csLoadkey cs) key Nothing


-- ---------------------------------------------------------------------

{-
function keysgen(cbDone,cbStep)
{
  var self = this;
  var ret = {parts:{}};
  var todo = Object.keys(self.CSets);
  if(todo.length == 0) return cbDone("no sets supported");
  function pop(err)
  {
    if(err) return cbDone(err);
    var csid = todo.pop();
    if(!csid){
      self.load(ret);
      return cbDone(null, ret);
    }
    self.CSets[csid].genkey(ret,pop,cbStep);
  }
  pop();
}
-}
keysgen :: () -> () -> IO ()
keysgen cbDone cbStep = (assert False undefined)

-- ---------------------------------------------------------------------

randomHEX :: Int -> TeleHash String
randomHEX len = do
  sw <- get
  let (bytes,newRNG) = cprgGenerate len (swRNG sw)
  put $ sw {swRNG = newRNG}
  -- return $ BU.toString $ B16.encode bytes
  return $ BC.unpack $ B16.encode bytes
{-
// return random bytes, in hex
function randomHEX(len)
{
  return crypto.randomBytes(len).toString("hex");
}
-}

-- ---------------------------------------------------------------------

uriparse :: String -> String
uriparse = (assert False undefined)

{-
var urllib = require("url");
function uriparse(uri)
{
  // node's uri parser enforces dns max 63 chars per label, grr!
  if(typeof uri !== "string") uri = "";
  var hashname = uri.match(/[0-9A-Fa-f]{64}/);
  if(!hashname) return urllib.parse(uri);
  var full = hashname[0];
  var part = full.substr(0,32);
  var u = urllib.parse(uri.replace(full,part));
  if(u.hostname != part) return urllib.parse(uri); // hashname was not the hostname
  Object.keys(u).forEach(function(k){
    if(typeof u[k] != "string") return;
    u[k] = u[k].replace(part,full);
  });
  return u;
}


-}

-- ---------------------------------------------------------------------

isHashName :: String -> String
isHashName = (assert False undefined)

{-
  self.isHashname = function(hex){return isHEX(hex, 64)};
-}

-- ---------------------------------------------------------------------

{-
/* CHANNELS API
hn.channel(type, arg, callback)
  - used by app to create a reliable channel of given type
  - arg contains .js and .body for the first packet
  - callback(err, arg, chan, cbDone)
    - called when any packet is received (or error/fail)
    - given the response .js .body in arg
    - cbDone when arg is processed
    - chan.send() to send packets
    - chan.wrap(bulk|stream) to modify interface, replaces this callback handler
      - chan.bulk(str, cbDone) / onBulk(cbDone(err, str))
      - chan.read/write
hn.raw(type, arg, callback)
  - arg contains .js and .body to create an unreliable channel
  - callback(err, arg, chan)
    - called on any packet or error
    - given the response .js .body in arg
    - chan.send() to send packets

self.channel(type, callback)
  - used to listen for incoming reliable channel starts
  - callback(err, arg, chan, cbDone)
    - called for any answer or subsequent packets
    - chan.wrap() to modify
self.raw(type, callback)
  - used to listen for incoming unreliable channel starts
  - callback(err, arg, chan)
    - called for any incoming packets
*/
-}

channelWraps :: IO ()
channelWraps = (assert False undefined)

{-
// these are called once a reliable channel is started both ways to add custom functions for the app
var channelWraps = {
  "bulk":function(chan){
    // handle any incoming bulk flow
    var bulkIn = "";
    chan.callback = function(end, packet, chan, cb)
    {
      cb();
      if(packet.body) bulkIn += packet.body;
      if(!chan.onBulk) return;
      if(end) chan.onBulk(end!==true?end:false, bulkIn);
    }
    // handle (optional) outgoing bulk flow
    chan.bulk = function(data, callback)
    {
      // break data into chunks and send out, no backpressure yet
      while(data)
      {
        var chunk = data.substr(0,1000);
        data = data.substr(1000);
        var packet = {body:chunk};
        if(!data) packet.callback = callback; // last packet gets confirmed
        chan.send(packet);
      }
      chan.end();
    }
  }
}

-}

-- ---------------------------------------------------------------------

wait :: Bool -> IO ()
wait = (assert False undefined)

{-
  self.wait = function(bool){
    if(bool) return self.waits.push(true);
    self.waits.pop();
    if(self.waiting && self.waits.length == 0) self.waiting();
  }

-}

-- ---------------------------------------------------------------------

--  do the maintenance work for links
linkLoop :: TeleHash ()
linkLoop = do
  sw <- get
  put sw {swBridgeCache = []} -- reset cache for any bridging
  hnReap
  linkMaint -- ping all of them
  io $ threadDelay $ milliToMicro (linkTimer defaults)
  linkLoop

-- | convert a millisecond value to a microsecond one
milliToMicro :: Num a => a -> a
milliToMicro x = 1000 * x

-- ---------------------------------------------------------------------

hnReap :: TeleHash ()
hnReap = do return ()
{-
// delete any defunct hashnames!
function hnReap(self)
{
  var hn;
  function del(why)
  {
    if(hn.lineOut) delete self.lines[hn.lineOut];
    delete self.all[hn.hashname];
    debug("reaping ", hn.hashname, why);
  }
  Object.keys(self.all).forEach(function(h){
    hn = self.all[h];
    debug("reap check",hn.hashname,Date.now()-hn.sentAt,Date.now()-hn.recvAt,Object.keys(hn.chans).length);
    if(hn.isSeed) return;
    if(Object.keys(hn.chans).length > 0) return; // let channels clean themselves up
    if(Date.now() - hn.at < hn.timeout()) return; // always leave n00bs around for a while
    if(!hn.sentAt) return del("never sent anything, gc");
    if(!hn.recvAt) return del("sent open, never received");
    if(Date.now() - hn.sentAt > hn.timeout()) return del("we stopped sending to them");
    if(Date.now() - hn.recvAt > hn.timeout()) return del("they stopped responding to us");
  });
}
-}

-- ---------------------------------------------------------------------

{-
// every link that needs to be maintained, ping them
function linkMaint(self)
{
  // process every bucket
  Object.keys(self.buckets).forEach(function(bucket){
    // sort by age and send maintenance to only k links
    var sorted = self.buckets[bucket].sort(function(a,b){ return a.age - b.age });
    if(sorted.length) debug("link maintenance on bucket",bucket,sorted.length);
    sorted.slice(0,defaults.link_k).forEach(function(hn){
      if(!hn.linked || !hn.alive) return;
      if((Date.now() - hn.linked.sentAt) < Math.ceil(defaults.link_timer/2)) return; // we sent to them recently
      hn.linked.send({js:{seed:self.seed}});
    });
  });
}
-}

-- ---------------------------------------------------------------------

{-
validPathTypes :: Set.Set PathType
validPathTypes
   = Set.fromList
     $ map (\pt -> PathType pt) ["ipv4","ipv6","http","relay","webrtc","local"]
-}

hnPathGet :: HashContainer -> Path -> TeleHash Path
hnPathGet hc path = do

{-
  if Set.notMember (pType path) validPathTypes
    then do
      logT $ "unknown path type:" ++ show (pType path)
      return path
    else do
-}
      case pathMatch path (hPaths hc) of
        Just p -> return path
        Nothing -> do
          logT $ "adding new path:" ++ show (length $ hPaths hc) ++ "," ++ showPath path
          -- always default to minimum priority
          let path' = path { pPriority = Just (if pathType path == PtRelay then (-1) else 0)}
          putHN $ hc { hPaths = (hPaths hc) ++ [path']
                     , hIsPublic = not $ isLocalPath path
                     }
          return path'
{-

  hn.pathGet = function(path)
  {
    if(["ipv4","ipv6","http","relay","webrtc","local"].indexOf(path.type) == -1)
    {
      warn("unknown path type", JSON.stringify(path));
      return path;
    }

    var match = pathMatch(path, hn.paths);
    if(match) return match;

    // preserve original
    if(!path.json) path.json = JSON.parse(JSON.stringify(path));

    debug("adding new path",hn.paths.length,JSON.stringify(path.json));
    info(hn.hashname,path.type,JSON.stringify(path.json));
    hn.paths.push(path);

    // always default to minimum priority
    if(typeof path.priority != "number") path.priority = (path.type=="relay")?-1:0;

    // track overall if they have a public IP network
    if(!isLocalPath(path)) hn.isPublic = true;

    return path;
  }

-}
-- ---------------------------------------------------------------------

-- every link that needs to be maintained, ping them
linkMaint :: TeleHash ()
linkMaint = do
  sw <- get
  -- process every bucket
  forM (Map.elems $ swBuckets sw) $ \bucket -> do
    -- sort the bucket contents on age
    let sorted = sortBy sf bucket
        sf a b = compare (lineAge a) (lineAge b)
    when (not $ null sorted) $ logT $ "link maintenance on bucket " ++ show (bucket,length sorted)
    forM (take (linkK defaults) sorted) $ \hn -> do
      if (lineLinked hn == Nothing) || (not $ lineAlive hn)
        then return ()
        else do
          timeNow <- io getClockTime
          -- if (timeNow - (lineSentat hn) < ((linkTimer defaults) `div` 2))
          if isTimeOut timeNow (lineSentat hn) ((linkTimer defaults) `div` 2)
            then return () -- we sent to them recently
            else send (fromJust $ lineLinked hn) (seedMsg (swSeed sw)) Nothing
  return ()

-- ---------------------------------------------------------------------

isTimeOut :: ClockTime -> Maybe ClockTime -> Int -> Bool
isTimeOut (TOD secs _picos) mt millis
 = case mt of
     Nothing -> True
     Just (TOD s _) -> (secs - s) < (fromIntegral millis `div` 1000)

-- ---------------------------------------------------------------------

seedMsg :: Bool -> LinePacket
seedMsg = (assert False undefined)


-- ---------------------------------------------------------------------

-- TODO: consider memoising this result, will be used a LOT
-- TODO: check that the algorithm is implemented correctly
--distanceTo :: Num a => Hash -> Hash -> a
dhash :: HashName -> HashName -> HashDistance
dhash (HN this) (HN h) = go 252 (reverse diffs)
  where
    go acc [] = acc
    go _acc (-1:[]) = -1
    go acc (-1:xs) = go (acc - 4) xs
    go acc (x:_xs) = acc + x

    diffs = map (\(a,b) -> sbtab !! (xor (digitToInt a) (digitToInt b))) $ zip this h
    sbtab = [-1,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3]

{-
// XOR distance between two hex strings, high is furthest bit, 0 is closest bit, -1 is error
function dhash(h1, h2) {
  // convert to nibbles, easier to understand
  var n1 = hex2nib(h1);
  var n2 = hex2nib(h2);
  if(!n1.length || !n2.length) return -1;
  // compare nibbles
  var sbtab = [-1,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3];
  var ret = 252;
  for (var i = 0; i < n1.length; i++) {
    if(!n2[i]) return ret;
    var diff = n1[i] ^ n2[i];
    if (diff) return ret + sbtab[diff];
    ret -= 4;
  }
  return ret;
}

-- ---------------------------------------------------------------------

// convert hex string to nibble array
function hex2nib(hex)
{
  var ret = [];
  for (var i = 0; i < hex.length / 2; i ++) {
      var bite = parseInt(hex.substr(i * 2, 2), 16);
      if (isNaN(bite)) return [];
      ret[ret.length] = bite >> 4;
      ret[ret.length] = bite & 0xf;
  }
  return ret;
}



-}

-- ---------------------------------------------------------------------

-- | Send the body of the packet in the telex. It is already encrypted
ipv4Send :: Path -> LinePacket -> Maybe HashContainer -> TeleHash ()
ipv4Send path msg _ = do
  -- logT $ "ipv4Send:" ++ show (path)
  -- logT $ "ipv4Send:" ++ show (B16.encode $ lbsTocbs $ unLP msg)
  -- logT $ "ipv4Send:" ++ (show $ gfromJust "ipv4Send" $ pathIp path) ++ ":" ++ (show $ pathPort path)
  addr <- io (addrFromHostPort (show $ gfromJust "ipv4Send.1" $ pathIp path)
                               (show $ gfromJust "ipv4Send.2" $ pathPort path))

  sender <- gets swSender
  sender msg addr
  return ()

-- ---------------------------------------------------------------------

{-
sendTelex :: Telex -> TeleHash ()
sendTelex msg = do
  timeNow <- io getClockTime
  res <- prepareTelex msg timeNow
  case (res) of
    Nothing -> return ()
    Just (line,msgJson) -> do
      -- console.log(["SEND[", telex._to, "]\t", msg].join(""));
      logT ( "SEND[:" ++ (show $ teleTo msg) ++ "]\t" ++ (msgJson))

      switch <- get
      put switch {swCountTx = (swCountTx switch) + 1 }

      addr <- io (addrFromHostPort (lineHost line) (linePort line))
      --Just socketh <- gets swH
      --io (sendDgram socketh msgJson addr)
      sender <- gets swSender
      sender msgJson addr

      updateTelehashLine(line)
-}

-- ---------------------------------------------------------------------

doNullSendDgram :: LinePacket -> NS.SockAddr -> TeleHash ()
doNullSendDgram msgJson addr = do
  --logT ("doNullSendDgram[" ++ msgJson ++ "] to " ++ (show addr))
  logT ("doNullSendDgram" )

-- ---------------------------------------------------------------------

doSendDgram :: LinePacket -> NS.SockAddr -> TeleHash ()
doSendDgram (LP msgJson) address = do
  -- logT $ "doSendDgram to:" ++ show addr
  Just socketh <- gets swH
  io (sendDgram socketh msgJson address)


-- ---------------------------------------------------------------------

sendDgram :: SocketHandle -> BC.ByteString -> NS.SockAddr -> IO ()
sendDgram socketh msgJson address =
  sendstr msgJson
    where
      -- Send until everything is done
      sendstr :: BC.ByteString -> IO ()
      sendstr omsg
        | BC.length omsg == 0  = return ()
        | otherwise = do sent <- SB.sendTo (slSocket socketh) omsg address
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
    -- logT $ "recvTelex:rinfo=" ++  show rinfo

    switch' <- get
    put switch' { swCountRx = (swCountRx switch') + 1 }
    -- switch <- get
    -- seedsIndex <- gets swSeedsIndex

    (Just hostIP,Just port) <- io (NS.getNameInfo [NS.NI_NUMERICHOST] True True rinfo)
    let
      remoteipp = IPP (hostIP ++ ":" ++ port)

    timeNow <- io getClockTime
    --console.log(["RECV from ", remoteipp, ": ", JSON.stringify(telex)].join(""));
    logT ("RECV from " ++ (show remoteipp) ++ ":" -- ++ (show $ B16.encode msg)
                  ++ " at " ++ (show timeNow))
    let
      maybeRxTelex = fromNetworkPacket (LP msg)
    -- logT $ "recvTelex:maybeRxTelex:" ++ show maybeRxTelex
    let
       path = Path
        {
          pJson    = PIPv4 (PathIPv4 (read hostIP) (read port))
        , pRelay   = Nothing
        , pId      = Nothing
        , pLastIn  = Nothing
        , pLastOut = Nothing
        , pPriority = Nothing
        , pIsSeed = False
        , pGone = False
        }

    case maybeRxTelex of
      Just rxTelex -> receive rxTelex path timeNow
      Nothing -> do
        logT $ "could not parse packet, discarding:" ++ (show $ B16.encode msg)
        return ()


-- ---------------------------------------------------------------------

isHEX :: BC.ByteString -> Int -> Bool
isHEX str len = r
  where
   (_f,b) = B16.decode str
   r = BC.length b == 0 && BC.length str == len

-- ---------------------------------------------------------------------

expectedKeysPresent :: Aeson.Value -> [String] -> Bool
expectedKeysPresent (Aeson.Object hm) keys = all present keys
  where
    present k = HM.member (Text.pack k) hm

