import System.IO
import System.Log.Handler.Simple
import System.Log.Logger
import qualified TeleHash.TeleHash as T.

main :: IO ()
main = do
    s <- streamHandler stdout DEBUG
    -- updateGlobalLogger rootLoggerName (addHandler s) -- setHandlers [s]
    updateGlobalLogger rootLoggerName (setHandlers [s])

    T.seed



chatRoom = "telechat:lobby"
nickName = "@user"
