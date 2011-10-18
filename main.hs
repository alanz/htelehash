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

--main :: IO ((),C.Switch)
main = do
    s <- streamHandler stdout DEBUG
    -- updateGlobalLogger rootLoggerName (addHandler s) -- setHandlers [s]
    updateGlobalLogger rootLoggerName (setHandlers [s])

    -- C.runSwitch
    (ch,thread) <- C.startSwitchThread

    runWeb
    catch (runWeb) (exc thread)

    where
      exc :: ThreadId -> SomeException -> IO ()
      exc thread _e = do 
        -- TODO: controlled shutdown. OTP where are you
        killThread thread
        -- return ((),undefined)
    
-- EOF

