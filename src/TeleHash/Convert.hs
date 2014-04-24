module TeleHash.Convert where

import Data.ByteString as S (ByteString, unpack)
import Data.ByteString.Char8 as C8 (pack)
import Data.Char (chr)

import Data.ByteString.Lazy as BL (ByteString, unpack,fromChunks)
import Data.ByteString.Char8 as BC (ByteString,pack)
import Data.ByteString.Internal (w2c)
-- import Prelude hiding (scanl)

-- ---------------------------------------------------------------------

strToBS :: String -> S.ByteString
strToBS = C8.pack

bsToStr :: S.ByteString -> String
bsToStr = map (chr . fromEnum) . S.unpack


lbsTocbs :: BL.ByteString -> BC.ByteString
lbsTocbs lbs = r
  where
    r = BC.pack $ map w2c (BL.unpack lbs)

cbsTolbs :: BC.ByteString -> BL.ByteString
cbsTolbs cbs = r
  where
    r = BL.fromChunks [cbs]


