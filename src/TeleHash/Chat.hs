module TeleHash.Chat
       (
       runChat
       ) where

import Control.Concurrent
import Control.Monad
import TeleHash.Controller
import System.IO

-- ---------------------------------------------------------------------

runChat :: Chan Signal -> Chan ReplyUser -> IO ()
runChat ch1 ch3  = do
  putStr $ ": "
  hFlush stdout
  str <- getLine
  putStrLn $ "msg:" ++ str
  runChat ch1 ch3

-- EOF