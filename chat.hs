import Control.Concurrent
import Control.Exception
import Prelude hiding (id, (.), head, either, catch)
import System.IO
import System.Log.Handler.Simple
import System.Log.Logger
import TeleHash.WebNew
import qualified TeleHash.TeleHash as T

main :: IO ()
main = do
    s <- streamHandler stdout DEBUG
    updateGlobalLogger rootLoggerName (setHandlers [s])

    (ch1,ch2,thread) <- T.seed Nothing

    catch (runWeb ch1 ch2) (exc thread)

    where
      exc :: ThreadId -> SomeException -> IO ()
      exc thread _e = do
        -- TODO: controlled shutdown. OTP where are you
        killThread thread
        -- return ((),undefined)



chatRoom = "telechat:lobby"
nickName = "@user"
