module TeleHash.Packet
  (
    Packet(..)
  , Head (..)
  , Body(..)
  , unBody
  , newPacket
  , headLen
  , bodyLen
  , packetLen
  , LinePacket(..)
  , unLP
  , toLinePacket
  , fromLinePacket

  -- debug
  , p1, p2
  , myencode
  ) where

import Crypto.Number.Serialize
import Data.Binary
import Data.Binary.Get
import TeleHash.Convert

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL


{-
HEAD

A length of 0 means there is no HEAD included and the packet is all
binary (only BODY).

A length of 1 means there is a single byte value that is not JSON.

A length of 2+ means the HEAD must be a UTF-8 encoded JSON object or
array (not any bare string/bool/number value). If the JSON parsing
fails, the parser must return an error.
-}

data Head = HeadEmpty | HeadByte Word8 | HeadJson BL.ByteString
          deriving (Show,Eq)

{-
BODY

The optional BODY is always a raw binary of the remainder bytes
between the packet's total length and that of the HEAD.

Often packets are attached inside other packets as the BODY, enabling
simple packet wrapping/relaying usage patterns.

The BODY is also used as the raw content transport for channels and
any app-specific usage.

 -}

data Body = Body BC.ByteString
          deriving Show
unBody (Body b) = b

data Packet = Packet { paHead :: Head
                     , paBody :: Body
                     }
            deriving Show

newPacket :: Packet
newPacket = Packet { paHead = HeadEmpty
                   , paBody = Body BC.empty
                   }

headLen :: Packet -> Int
headLen (Packet HeadEmpty _)     = 2
headLen (Packet (HeadByte _) _)  = 3
headLen (Packet (HeadJson bs) _) = 2 + (fromIntegral $ BL.length bs)

bodyLen :: Packet -> Int
bodyLen (Packet _ (Body bs)) = fromIntegral $ BC.length bs

packetLen p = headLen p + bodyLen p

-- ---------------------------------------------------------------------

instance Binary Packet where
  put p = do put (paHead p)
             put (paBody p)

  get = do h  <- get
           -- pb <- getRemainingLazyByteString

           -- return (newPacket { paHead = h, paBody = lbsTocbs pb})
           pb <- get
           return (newPacket { paHead = h, paBody = pb})

-- ---------------------------------------------------------------------

instance Binary Head where

  put HeadEmpty    = put (0 :: Word16)
  put (HeadByte b) = do put (1 :: Word16)
                        put b
  put (HeadJson x) = do put ((fromIntegral $ BL.length x) :: Word16)
                        mapM_ put $ BL.unpack x

  get = do hb <- get :: Get Word16
           h <- case hb of
                 0 -> return HeadEmpty
                 1 -> do b <- get
                         return (HeadByte b)
                 x -> do b <- getLazyByteString (fromIntegral x)
                         return (HeadJson b)
           return h

instance Binary Body where
  put (Body bs) = mapM_ put $ BC.unpack bs

  get = do bs <- getRemainingLazyByteString
           return (Body $ lbsTocbs bs)


-- ---------------------------------------------------------------------

data LinePacket = LP BL.ByteString
                deriving Show
unLP :: LinePacket -> BL.ByteString
unLP (LP x) = x

-- ---------------------------------------------------------------------

toLinePacket :: Packet -> LinePacket
-- toLinePacket p = LP $ encode p
toLinePacket (Packet h (Body b)) = LP $ BL.append (myencode h) (cbsTolbs b)

myencode :: Head -> BL.ByteString
myencode (HeadEmpty) = BL.pack [0,0]
myencode (HeadByte b) = BL.pack [0,1,b]
myencode (HeadJson x) = BL.append (cbsTolbs bb) x
  where
    xlen :: Integer
    xlen = fromIntegral (BL.length x)

    Just bb = i2ospOf 2 xlen

-- ---------------------------------------------------------------------

-- |Note: this will throw an exception is the decode fails
fromLinePacket :: LinePacket -> Maybe Packet
fromLinePacket (LP bs) = Just $ decode bs

-- ---------------------------------------------------------------------

{-

Examples

First packet out

  This encodes a startup packet of
   {"type":"seek","c":0,"seek":"89a4cbc6c27eb913c1bcaf06bac2d8b872c7cbef626b35b6d7eaf993590d37de"}
-}

p1 :: [Word8]
p1 =
 [
 0x00, 0x01,   -- head length = 1 (BE 16 bit)

 0x1a,         -- Single byte head means 'open' packet, using crypto "1a"

 -- HMAC (20 bytes)
 0x70, 0xf0, 0xd6, 0x5a, 0xc1, 0xae, 0xae, 0x58, 0xe4, 0xaf,
 0x0e, 0x58, 0x27, 0xa4, 0x4b, 0x4b, 0x0b, 0x0d, 0x39, 0x41,

 -- Public Key (40 bytes)
 0x15, 0x97, 0xb6, 0x35, 0x55, 0xf0, 0xf0, 0x99, 0x48, 0xce,
 0x81, 0xf5, 0xba, 0xd9, 0xdc, 0x3b, 0x05, 0xc5, 0x81, 0xce,
 0x2e, 0x6d, 0xc9, 0x1a, 0xb9, 0x87, 0xdc, 0xd9, 0x13, 0x44,
 0x37, 0xb0, 0x68, 0x25, 0x62, 0xac, 0xc7, 0x07, 0x1e, 0x27,

 -- inner packet, AES encrypted
 0xff,
 0xb5, 0x15, 0x64, 0x2e, 0x1a, 0x38, 0xaa, 0x33, 0xe2, 0xaf, 0x1d, 0x74, 0x46, 0xef, 0x89, 0xdc,
 0xa8, 0x15, 0x66, 0x7a, 0x5f, 0xa6, 0x45, 0x9f, 0xbb, 0xdb, 0x7a, 0x27, 0xb5, 0xa9, 0x48, 0xff,
 0xc3, 0xf6, 0xc3, 0x1e, 0xf6, 0x83, 0xf5, 0x1e, 0x06, 0xb4, 0xb3, 0x13, 0xfc, 0x57, 0xa1, 0x2a,
 0xdf, 0x96, 0xdf, 0x90, 0x2d, 0x14, 0x24, 0x11, 0xa6, 0x01, 0x4b, 0xed, 0xf1, 0xd1, 0x32, 0x88,
 0x15, 0xb4, 0x25, 0x0f, 0xa8, 0xda, 0x19, 0xc4, 0xb1, 0xf3, 0xe3, 0x4c, 0x31, 0x4d, 0xfe, 0x36,
 0xcf, 0x76, 0xc8, 0x46, 0x04, 0x30, 0xd2, 0x96, 0x46, 0xec, 0x45, 0xd3, 0x06, 0xb7, 0x92, 0x61,
 0xe8, 0xcf, 0x57, 0xd7, 0x20, 0xc7, 0xf4, 0xcb, 0xab, 0x66, 0x73, 0x39, 0xc5, 0xe4, 0xb4, 0x11,
 0x34, 0xd3, 0x45, 0x4f, 0x06, 0x4e, 0x75, 0xa1, 0xa6, 0x33, 0x91, 0x71, 0x49, 0xeb, 0x6c, 0xd9,
 0x6b, 0xf3, 0x8b, 0x3f, 0x96, 0xe1, 0x2e, 0xad, 0xbc, 0xf0, 0x81, 0x60, 0xae, 0x3d, 0x7d, 0x59,
 0xad, 0x1a, 0x0f, 0xdb, 0x1f, 0xa7, 0x6b, 0x36, 0x24, 0xfc, 0x6a, 0x0c, 0x15, 0xe9, 0x32, 0x64,
 0xe4, 0x55, 0x3f, 0x19, 0xd9, 0x20, 0x4d, 0x80, 0x27, 0x50, 0x68, 0x77, 0x32, 0x27, 0x34, 0x66,
 0xc2, 0x76, 0x02, 0x8f, 0x14, 0xda, 0xe8, 0xfb, 0x89, 0x28, 0x27, 0xfd, 0xbd, 0x8f, 0x41, 0x3f,
 0x71, 0xaa, 0x50, 0xca, 0x21, 0x98, 0x0e, 0x44, 0x69, 0x49, 0xc7, 0x74, 0xf0, 0xa0, 0xc9, 0x0b,
 0x30, 0x8f, 0x99, 0x60, 0x87, 0xec, 0x35, 0x25, 0x0d, 0xeb, 0xa5, 0x0a, 0x29, 0xec, 0x22, 0x13,
 0xae, 0xae, 0xdb, 0x32, 0xf9
 ]

{-
Second packet out
-}

p2 :: [Word8]
p2 =
 [
 0x00, 0x00,  -- head length 0

 0x16, 0x60, 0xef, 0x04, 0x2e, 0x32, 0x1e, 0xfb, 0x11, 0x0d, 0xb8, 0x9f, 0xe7, 0x05,
 0x72, 0xf6, 0x06, 0x48, 0xe2, 0x9c, 0x00, 0x00, 0x00, 0x00, 0x08, 0xf2, 0x87, 0x9e, 0xb5, 0xb2,
 0x4c, 0x3f, 0xf3, 0xca, 0x4c, 0xa3, 0x18, 0xdc, 0x16, 0xac, 0x33, 0x94, 0x9a, 0xaa, 0xcc, 0x01,
 0xdf, 0xb8, 0x16, 0x7f, 0x48, 0xe1, 0x4c, 0xe4, 0x45, 0xa8, 0x4b, 0x61, 0xfa, 0x1e, 0xdb, 0x99,
 0xee, 0x83, 0xdb, 0xb0, 0xbf, 0x83, 0x33, 0x72, 0xbc, 0xf0, 0xbc, 0xfd, 0xda, 0x4a, 0x5c, 0x40,
 0x9d, 0xb6, 0xe1, 0x33, 0x38, 0xc3, 0x9a, 0x54, 0x3e, 0x9e, 0xf6, 0xbe, 0x11, 0x39, 0x2c, 0x0f,
 0x57, 0xb0, 0xc9, 0x27, 0x97, 0x20, 0x8e, 0xf5, 0xf2, 0x38, 0x0a, 0xc1, 0xb9, 0x95, 0xf1, 0xe4,
 0x68, 0x34, 0xd0, 0xc8, 0x55, 0x9b, 0x8a, 0x87, 0xa5, 0xc5, 0xe3
 ]


testp1 = do
  let p1b = BL.pack p1
  let p@(Packet h (Body b)) = decode p1b :: Packet
  putStrLn $ show p
  putStrLn $ show (BC.length b)

testp2 = do
  let p1b = BL.pack p2
  let p@(Packet h (Body b)) = decode p1b :: Packet
  putStrLn $ show p
  putStrLn $ show (BC.length b)

-- ---------------------------------------------------------------------

-- Received open packet
-- RECV from IPP "10.0.0.42:42424":"00011adee339dc7ca4227333401b8d2dc460dfa78317b6c5dea168b4679c59fbc93a2267e1c2b7cf4bfe832f0fb07221f8654a758d6a63200979f9367e046379aa1f4d27f74be6ae9367f4ff655820f2e0dedea70c6a8e32084180a464993e625803fa9774ac99a50c2e63fa637a07a2ae52860a1961f630c51d4f6779c7409c80497f52c91c69ed812261f2dcb5c1675b24d978a94fb55d9d55ecb772b542aa21c32d9dc704374dcbf53b32579e68cc3a01da6f9fd44ee1a1753919c50a09790c168d2a22069e0bd1f7e7db5410ec540c90f893956ddbdf01fc9ae5a7c82fc832ae72f846a2b1dc3a911dc13aa641fcf83f68ed1d3e6f445f5b82814649b9a127c7ad6fd2e3a8d5b986852c8bca221931e7a09ea1a2e7aff7ea090fdc8eebdd8664bb926909c396c3f7dd01ac38819a6cf7b947a855f8bdc87593e20bda115913056d6935b188308fad9a7873fb95395216d487cb5173a20296b86103715005e1ccbe3bcaae8ee64e4806928dd654a08ed8a7818d4eff2052aaa62c300c7661e678febaf34378a32028e0a3eea83cc87bc9c18742d4daafa3029df15030d7fc2cf916eab082e2424e4f912cadd319aaa39d6a8dc32c4282" at

b16_rx_open = "00011adee339dc7ca4227333401b8d2dc460dfa78317b6c5dea168b4679c59fbc93a2267e1c2b7cf4bfe832f0fb07221f8654a758d6a63200979f9367e046379aa1f4d27f74be6ae9367f4ff655820f2e0dedea70c6a8e32084180a464993e625803fa9774ac99a50c2e63fa637a07a2ae52860a1961f630c51d4f6779c7409c80497f52c91c69ed812261f2dcb5c1675b24d978a94fb55d9d55ecb772b542aa21c32d9dc704374dcbf53b32579e68cc3a01da6f9fd44ee1a1753919c50a09790c168d2a22069e0bd1f7e7db5410ec540c90f893956ddbdf01fc9ae5a7c82fc832ae72f846a2b1dc3a911dc13aa641fcf83f68ed1d3e6f445f5b82814649b9a127c7ad6fd2e3a8d5b986852c8bca221931e7a09ea1a2e7aff7ea090fdc8eebdd8664bb926909c396c3f7dd01ac38819a6cf7b947a855f8bdc87593e20bda115913056d6935b188308fad9a7873fb95395216d487cb5173a20296b86103715005e1ccbe3bcaae8ee64e4806928dd654a08ed8a7818d4eff2052aaa62c300c7661e678febaf34378a32028e0a3eea83cc87bc9c18742d4daafa3029df15030d7fc2cf916eab082e2424e4f912cadd319aaa39d6a8dc32c4282"

rx_open = b16ToLbs b16_rx_open

lp_rx_open = LP rx_open

b16ToLbs str = cbsTolbs r
  where (r,_) = B16.decode $ BC.pack str

headok :: Head
headok = decode $ b16ToLbs "00011a"

bodyok :: Body
bodyok = decode $ b16ToLbs "dee339dc7ca4227333401b8d2dc460dfa78317b6c5dea168b4679c59fbc93a2267e1c2b7cf4bfe832f0fb07221f8654a758d6a63200979f9367e046379aa1f4d27f74be6ae9367f4ff655820f2e0dedea70c6a8e32084180a464993e625803fa9774ac99a50c2e63fa637a07a2ae52860a1961f630c51d4f6779c7409c80497f52c91c69ed812261f2dcb5c1675b24d978a94fb55d9d55ecb772b542aa21c32d9dc704374dcbf53b32579e68cc3a01da6f9fd44ee1a1753919c50a09790c168d2a22069e0bd1f7e7db5410ec540c90f893956ddbdf01fc9ae5a7c82fc832ae72f846a2b1dc3a911dc13aa641fcf83f68ed1d3e6f445f5b82814649b9a127c7ad6fd2e3a8d5b986852c8bca221931e7a09ea1a2e7aff7ea090fdc8eebdd8664bb926909c396c3f7dd01ac38819a6cf7b947a855f8bdc87593e20bda115913056d6935b188308fad9a7873fb95395216d487cb5173a20296b86103715005e1ccbe3bcaae8ee64e4806928dd654a08ed8a7818d4eff2052aaa62c300c7661e678febaf34378a32028e0a3eea83cc87bc9c18742d4daafa3029df15030d7fc2cf916eab082e2424e4f912cadd319aaa39d6a8dc32c4282"

decodeok = fromLinePacket lp_rx_open

decodefail = fromLinePacket (LP (b16ToLbs "08011adee339dc7ca422"))
