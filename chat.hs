import Control.Concurrent
import Control.Exception
import Prelude hiding (id, (.), head, either, catch)
import System.IO
import System.Log.Handler.Simple
import System.Log.Logger
import TeleHash.Chat
import TeleHash.Web
import qualified TeleHash.Controller as C

main :: IO ()
main = do

  let logPath = "/tmp/telehash.log"
  myFileHandler <- fileHandler logPath DEBUG
  updateGlobalLogger rootLoggerName (setHandlers [myFileHandler])

  -- s <- streamHandler stdout DEBUG
  -- updateGlobalLogger rootLoggerName (setHandlers [s])

  -- C.runSwitch
  (ch1,ch2,ch3,thread) <- C.startMasterThread

  --runWeb
  webThread <- forkIO (runWeb ch1 ch2)

  catch (runChat ch1 ch3) (exc thread)

    where
      exc :: ThreadId -> SomeException -> IO ()
      exc thread _e = do
        -- TODO: controlled shutdown. OTP where are you
        killThread thread
        -- return ((),undefined)

chatRoom = "telechat:lobby"
nickName = "@user"

-- EOF
