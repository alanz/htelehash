{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BL

data Coord = Coord { x :: Double, y :: Double, c :: Maybe String }
             deriving (Show)

-- A ToJSON instance allows us to encode a value as JSON.

instance ToJSON Coord where
  toJSON (Coord xV yV cV) = object $ stripNulls
                            [
                              "x" .= xV,
                              "y" .= yV,
                              "c" .= cV
                            ]

stripNulls :: [Pair] -> [Pair]
stripNulls xs = filter (\(_,v) -> v /= Null) xs

-- A FromJSON instance allows us to decode a value from JSON.  This
-- should match the format used by the ToJSON instance.

instance FromJSON Coord where
  parseJSON (Object v) = Coord <$>
                         v .:  "x" <*>
                         v .:  "y" <*>
                         v .:? "c"
  parseJSON _          = empty

main :: IO ()
main = do
  -- let req1 = decode "{\"x\":3.0,\"y\":-1.0,\"c\":\"foo\"}" :: Maybe Coord
  let req1 = decode "{\"y\":-1.0,\"x\":3.0,\"c\":\"foo\"}" :: Maybe Coord
  print req1
  let req = decode "{\"x\":3.0,\"y\":-1.0}" :: Maybe Coord
  print req
  let reply1 = Coord 123.4 20 (Just "foo")
  BL.putStrLn (encode reply1)
  let reply = Coord 123.4 20 Nothing
  BL.putStrLn (encode reply)
