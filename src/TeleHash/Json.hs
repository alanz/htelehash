{-# LANGUAGE OverloadedStrings #-}
module TeleHash.Json 
       (
         TeleHashEntry(..)
       , parseAll         
       ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Attoparsec
--import Data.Iso
--import Language.JsonGrammar


import qualified Data.ByteString as B
import qualified Data.Text as T

-- ---------------------------------------------------------------------
{-
Telehash bootstrap replies

{"_ring":17904, 
 ".see":[ "208.68.163.247:42424", "208.68.160.25:55137"], 
 "_br":52, 
  "_to":"173.19.99.143:63681" }

{"_ring":17867,
 ".see":["50.19.183.3:47610","50.18.184.21:43435","50.18.184.21:48563",
          "208.68.163.247:42424","50.18.184.21:37180"],
"_br":19,
"_to":"196.209.236.138:33164"}

-}
data TeleHashEntry = TeleHashEntry 
                     { teleRing :: Int,
                       teleSee  :: Maybe [T.Text],
                       teleBr   :: Int,
                       teleTo   :: T.Text,
                       teleLine :: Maybe T.Text,
                       teleHop  :: Maybe T.Text
                     } deriving (Eq, Show)

instance FromJSON TeleHashEntry
  where
    parseJSON (Object v) = TeleHashEntry <$> 
                           v .:  "_ring" <*>
                           v .:? ".see"  <*>
                           v .:  "_br"   <*>
                           v .:  "_to"   <*>
                           v .:? "_line" <*>
                           v .:? "_hop"
    parseJSON _          = mzero

{-
instance ToJSON Coord where
   toJSON (Coord x y) = object ["x" .= x, "y" .= y]
-}
   
instance ToJSON TeleHashEntry where
   toJSON (TeleHashEntry teleRing teleSee teleBr teleTo teleLine teleHop)  
      = object ["_ring" .= teleRing, ".see" .= teleSee]   

-- ---------------------------------------------------------------------

parseAll :: B.ByteString -> [TeleHashEntry]
parseAll s = case (parse (fromJSON <$> json) s) of
  Done _ (Error err)  -> error err
  Done ss (Success e) -> e:(parseAll ss)
  _                   -> []

main :: IO ()
main = do s <- B.readFile "jsonth.txt"
          let p = parseAll s
          print p
          