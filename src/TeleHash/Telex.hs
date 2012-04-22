{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BL


data Telex = Telex
             { teleRing   :: Maybe Int
             -- , teleSee    :: Maybe [IPP]
             , teleBr     :: Int
             -- , teleTo     :: IPP
             , teleLine   :: Maybe Int
             , teleHop    :: Maybe Int
             -- , teleSigEnd :: Maybe Hash
             , teleSigPop :: Maybe String
             -- , teleTap    :: Maybe [Tap]
             -- , teleRest   :: Map.Map String String
             -- , teleMsgLength :: Maybe Int -- length of received Telex
             } deriving (Show)

-- A ToJSON instance allows us to encode a value as JSON.

instance ToJSON Telex where
  toJSON (Telex xV yV) = object $ stripNulls
                         [
                           "x" .= xV
                         , "y" .= yV
                         ]

-- A FromJSON instance allows us to decode a value from JSON.  This
-- should match the format used by the ToJSON instance.

instance FromJSON Telex where
  parseJSON (Object v) = Telex <$>
                         v .: "x" <*>
                         v .: "y"
  parseJSON _          = empty

stripNulls :: [Pair] -> [Pair]
stripNulls xs = filter (\(_,v) -> v /= Null) xs

main :: IO ()
main = do
  let req = decode "{\"x\":3.0,\"y\":-1.0}" :: Maybe Telex
  print req
  let reply = Telex (Just 1) 2 (Just 3) (Just 4) (Just "h")
  BL.putStrLn (encode reply)
