{-# LANGUAGE MultiWayIf #-}


import Control.Concurrent
import Control.Exception
import Control.Monad.State
import Data.List
import Network.Socket
import System.Environment
import System.IO
import System.Log.Handler.Simple
import System.Log.Logger
import System.Remote.Monitoring
import System.Time

import Network.TeleHash.Ext.Chat
import Network.TeleHash.Ext.Connect
import Network.TeleHash.Ext.Link
import Network.TeleHash.Ext.Peer
import Network.TeleHash.Ext.Seek
import Network.TeleHash.Ext.Thtp
import Network.TeleHash.Ext.Path
import Network.TeleHash.Crypt
import Network.TeleHash.Periodic
import Network.TeleHash.Switch
import Network.TeleHash.SwitchApi
import Network.TeleHash.Types
import Network.TeleHash.Utils
import Network.TeleHash.SwitchUtils

import qualified Data.ByteString.Char8 as BC
-- import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Network.Socket.ByteString as SB

-- ---------------------------------------------------------------------

data Input = IConsole String | IUdp BC.ByteString SockAddr
             | ITick
           deriving (Show)

-- ---------------------------------------------------------------------

-- TODO: use System.Console.ReadLine for this
readConsole :: (Chan Input) -> IO ()
readConsole ch = forever $ do
  s <- getLine
  putStrLn $ "readConsole: putting [" ++ s ++ "]"
  writeChan ch (IConsole s)

logg :: String -> String -> IO ()
logg nick str = do
  if str /= ""
    then putStrLn $ str
    else return ()
  putStr $ nick ++ "> "

{-
char nick[16];
void logg(char * format, ...)
{
    char buffer[1024];
    va_list args;
    va_start (args, format);
    vsnprintf (buffer, 1024, format, args);
    if(strlen(buffer))
    {
      printf("\n%s\n%s> ", buffer, nick);
    }else{
      printf("%s> ",nick);
    }
    va_end (args);
}

-}

recvUdpMsg :: (Chan Input) -> Socket -> IO ()
recvUdpMsg ch sock = forever $ do
  (msg,rinfo) <- (SB.recvFrom sock 1000)
  writeChan ch (IUdp msg rinfo)



-- ---------------------------------------------------------------------

main :: IO ()
main = do
  -- forkServer (BC.pack "localhost") 8000

  s <- streamHandler stdout DEBUG
  updateGlobalLogger rootLoggerName (setHandlers [s])

  h1 <- fileHandler "line.log" DEBUG
  updateGlobalLogger lineLoggerName (addHandler h1)

  h2 <- fileHandler "line-hex.log" DEBUG
  updateGlobalLogger lineHexLoggerName (addHandler h2)


{-
  Log priorities

  DEBUG         Debug messages               logT
  INFO          Information
  NOTICE        Normal runtime conditions    logR
  WARNING       General Warnings             logP
  ERROR General Errors
  CRITICAL      Severe situations
  ALERT         Take immediate action
  EMERGENCY     System is unusable
-}

  args <- getArgs
  if null args
    then do
      updateGlobalLogger mainLoggerName (setLevel NOTICE)
    else do
      updateGlobalLogger mainLoggerName (setLevel DEBUG)

  updateGlobalLogger lineLoggerName    (setLevel DEBUG)
  updateGlobalLogger lineHexLoggerName (setLevel DEBUG)
  updateGlobalLogger rootLoggerName    (setLevel ERROR)

  -- sock <- util_server 42425 100
  sock <- util_server 0 100

  -- (ch1,ch2,thread) <- startSwitchThread
  runApp (Just $ SocketHandle sock)  app

  threadDelay 3000000


app :: TeleHash ()
app = do
  timeNow <- io $ getClockTime
  logP $ "-------------------------------starting new run "++ show timeNow ++ "---------------------------"
  logH ("-------------------------------starting new run "++ show timeNow ++ "---------------------------") BC.empty

  crypt_init

  switch_init testId
  sw1 <- get
  logT $ "self:" ++ show (swId sw1,swParts sw1)

  seek_auto
  myId <- io myThreadId
  let nick = show myId
  logT $ "nick=" ++ nick

  -- make a dummy thtp response
  let p1 = packet_new (HN "null")
      p2 = packet_set_int p1 "status" 200
      p3 = packet_body p2 (BC.pack "bar\n")
      note = packet_new (HN "null")
      note2 = packet_link (Just note) p3
  thtp_path "/foo" note2

  util_loadjson

  sw <- get
  let (Just (SocketHandle sock)) = swH sw
  logT $ "about to launch threads"
  chInput <- io newChan
  threadConsole <- io $ forkIO (readConsole chInput)
  threadUdp     <- io $ forkIO (recvUdpMsg chInput sock)
  threadTimer   <- io $ forkIO (timer (1 * onesec) ITick chInput)
  logT $ "threads launched"


  logT $ "loaded hashname " ++ show (swId sw)

  -- new chat, must be after-init
  mchat <- chat_get (Just "tft")
  let chat = (gfromJust "app" mchat)
  putChatCurrent (ecId chat)
  logT $ "app:chat=" ++ show chat
  void $ chat_add (ecId chat) "*" "invite"
  mp <- chat_message (ecId chat)
  let p1a = gfromJust "app" mp
      p2a = packet_set_str p1a "text" nick
  void $ chat_join (ecId chat) p2a

  chat2 <- getChat (ecId chat)
  logT $ "created chat:" ++ show (chatIdToString $ ecId chat2,packet_get_str p2a "id",unCH $ ecRHash chat2)
  logT $ nick ++ ">"

  sw2 <- get
  logT $ "seeds:" ++ show (swSeeds sw2)
  -- link_hn bucket_get(s->seeds, 0));
  -- void $ link_hn (head $ Set.toList (swSeeds sw2)) Nothing
  forM_ (Set.toList (swSeeds sw2)) $ \seed -> do
    void $ link_hn seed Nothing


  util_sendall sock

  -- create an admin channel for notes
  admin <- chan_new (swId sw2) ".admin" Nothing
  logT $ "admin channel:" ++ showChan admin

  let
    adminHandler :: Uid -> TeleHash ()
    adminHandler cid = do
      c <- getChan cid
      notes <- chan_notes_all c
      forM_ notes $ \note1 -> do
        logT $ "admin note " ++ showJson (tJs note1)

    chat_handler :: Uid -> TeleHash ()
    chat_handler cid = do
      mchat2 <- ext_chat cid
      case mchat2 of
        Nothing -> return ()
        Just ch -> do
          msgs <-chat_pop_all (ecId ch)
          forM_ msgs $ \p -> do
            logT $ "processing chat msg:" ++ show p

            if (packet_get_str_always p "type") == "state"
              then io $ logg nick (packet_get_str_always p "text" ++ " joined")
              else return ()

            if (packet_get_str_always p "type") == "chat"
              then do
                mparticipant <- chat_participant (ecId ch) (packet_get_str_always p "from")
                logT $ "rx_loop:(mparticipant,from)=" ++ show (mparticipant,packet_get_str_always p "from")
                let participant = case mparticipant of
                                    Nothing -> "*UNK*"
                                    Just pa  -> packet_get_str_always pa "text"
                io $ logg nick $ participant ++ "> " ++ (packet_get_str_always p "text")
              else return ()

    chanHandlers = Map.fromList [(chUid admin,adminHandler)]
    typeHandlers = Map.fromList
                  [("connect" , ext_connect)
                  ,("thtp"    , ext_thtp)
                  ,("link"    , ext_link)
                  ,("seek"    , ext_seek)
                  ,("path"    , ext_path)
                  ,("peer"    , ext_peer)
                  ,("chat"    , chat_handler)
                  ]

  logT $ "about to enter forever loop"
  void $ forever $ do
    logT $ "top of forever loop"
    inp <- io $ readChan chInput

    case inp of
      ITick -> do
        switch_loop

      IUdp msg rinfo -> do
        recvTelex msg rinfo
        switch_loop
        -- rx_loop
        util_mainloop chanHandlers typeHandlers

      IConsole l -> do
        logT $ "console gave:" ++ l
        if | isPrefixOf "/quit" l -> do
              io $ killThread threadConsole
              io $ killThread threadUdp
              io $ killThread threadTimer
              me <- io $ myThreadId
              io $ killThread me

           | isPrefixOf "/nick " l -> do
              mp1 <- chat_message (ecId chat)
              case mp1 of
                Just p -> do
                  let nick = (drop (length ("/nick ")) l)
                  let p2b = packet_set_str p "text" nick
                  cid <- getChatCurrent
                  void $ chat_join cid p2b
                  io $ logg nick ""
                Nothing -> return ()

           | isPrefixOf "/get " l -> do
              io $ logg nick ("get " ++ drop 5 l)
              p <- chan_note admin Nothing
              let p2c = rxTelexToTxTelex p (swId sw2)
              let p3c = packet_set_str p2c "uri" (drop 5 l)
              void $ thtp_req p3c

           | isPrefixOf "/chat " l -> do
              logT $ "processing chat:" ++ show (drop 6 l)
              cid <- getChatCurrent
              chat_free cid
              mchat2 <- chat_get (Just (drop 6 l))
              case mchat2 of
                Nothing -> do
                  logT $ "/chat: chat_get returned Nothing"
                  return ()
                Just chat3 -> do
                  putChat chat3
                  putChatCurrent (ecId chat3)
                  mp2 <- chat_message (ecId chat3)
                  case mp2 of
                    Nothing -> do
                      logT $ "/chat: chat_message returned Nothing"
                      return ()
                    Just p -> do
                      let p2d = packet_set_str p "text" nick
                      void $ chat_join (ecId chat3) p2d
                      io $ logg nick ("joining chat " ++ show (ecId chat3,packet_get_str p2d "id", ecRHash chat3))

           | isPrefixOf "/chans" l -> do
              logR $ "Chans"
              chStr <- showAllChans
              logR chStr

           | isPrefixOf "/hns" l -> do
              logR $ "HashNames"
              chStr <- showAllHashNames
              logR chStr

           | isPrefixOf "/lines" l -> do
              logR $ "Lines"
              chStr <- showAllLines
              logR chStr

           | isPrefixOf "/dht" l -> do
              logR $ "DHT"
              chStr <- showAllDht
              logR chStr

           | isPrefixOf "/seek" l -> do
              logR $ "Seeking all seeds"
              manual_seek

           | isPrefixOf "/dump" l -> do
              logR $ "Dumping all seeds"
              now <- io getClockTime
              let fn = "./seeds.json." ++ show now
              dump_seeds fn

           | otherwise -> do
              -- default send as message
              cid <- getChatCurrent
              mp3 <- chat_message cid
              case mp3 of
                Nothing -> return ()
                Just p -> do
                  let p2e = packet_set_str p "text" l
                  chat_send cid p2e
                  io $ logg nick ""

    util_mainloop chanHandlers typeHandlers
    util_sendall sock


  assert False undefined

