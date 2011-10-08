import System.IO
import System.Log.Handler.Simple  
import System.Log.Logger
import qualified TeleHash.Controller as C

main :: IO ((),C.Switch)
main = do
    s <- streamHandler stdout DEBUG
    -- updateGlobalLogger rootLoggerName (addHandler s) -- setHandlers [s]
    updateGlobalLogger rootLoggerName (setHandlers [s])

    C.runSwitch

-- EOF

