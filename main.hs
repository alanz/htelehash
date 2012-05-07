import Control.Concurrent
import Control.Exception
import Prelude hiding (id, (.), head, either, catch)
import System.IO
import System.Log.Handler.Simple
import System.Log.Logger
import TeleHash.Web
import qualified TeleHash.Controller as C

{-
main :: IO ((),C.Switch)
main = do
    s <- streamHandler stdout DEBUG
    -- updateGlobalLogger rootLoggerName (addHandler s) -- setHandlers [s]
    updateGlobalLogger rootLoggerName (setHandlers [s])

    C.runSwitch
-}

main :: IO ()
main = do
    s <- streamHandler stdout DEBUG
    -- updateGlobalLogger rootLoggerName (addHandler s) -- setHandlers [s]
    updateGlobalLogger rootLoggerName (setHandlers [s])

    -- C.runSwitch
    (ch1,ch2,ch3,thread) <- C.startMasterThread

    --runWeb
    catch (runWeb ch1 ch2) (exc thread)

    where
      exc :: ThreadId -> SomeException -> IO ()
      exc thread _e = do
        -- TODO: controlled shutdown. OTP where are you
        killThread thread
        -- return ((),undefined)

-- EOF

