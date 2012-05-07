module TeleHash.Chat
       (
       runChat
       ) where

import Control.Concurrent
import Control.Monad
import TeleHash.Controller
import System.IO

-- ---------------------------------------------------------------------

runChat :: Chan Signal -> Chan ReplyUser -> String -> IO ()
runChat ch1 ch3 nickName = do
  putStr $ ": "
  hFlush stdout
  str <- getLine
  putStrLn $ "msg:" ++ str
  sendUserMsg ch1 [("txt",str),("nick",nickName)]
  runChat ch1 ch3 nickName

-- EOF