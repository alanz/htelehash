module Network.TeleHash.Packet
  (
    NetworkPacket(..)
  , Packet(..)
  , Head (..)
  , Body(..)
  , unBody
  , newPacket
  , headLen
  , bodyLen
  , packetLen
  , networkPacketLen
  , packetJson
  , LinePacket(..)
  , unLP
  , toNetworkPacket
  , fromNetworkPacket
  , toLinePacket
  , fromLinePacket

  , showPacketShort

  -- debug
  -- , p1, p2
  , myencode
  ) where

-- import Control.Applicative
-- import Control.Concurrent
import Control.Exception
-- import Control.Monad
-- import Control.Monad.Error

import Crypto.Number.Serialize
import Data.Binary
-- import Data.Bits
import Data.Binary.Get
-- import Data.Binary.Put
import Network.TeleHash.Convert

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL


-- ---------------------------------------------------------------------
-- | A network level packet.
-- See https://github.com/telehash/telehash.org/blob/master/network.md
-- This is either an `open` or a 'line' packet
-- The open is coded as 0x00 0x01 followed by the crypto set id, followed by the body
-- The line is coded as 0x00 0x00 followed by the encrypted line packet
data NetworkPacket = OpenPacket Word8 BC.ByteString
                   | LinePacket BC.ByteString
                   | PingPongPacket Packet
                   deriving (Eq,Show)


instance Binary NetworkPacket where
  put (OpenPacket cs bs) = do put (0::Word8)
                              put (1::Word8)
                              put (cs::Word8)
                              put bs
  put (LinePacket    bs) = do put (0::Word8)
                              put (0::Word8)
                              put bs
  put (PingPongPacket p) = do let LP b = toLinePacket p
                              put b

  get = do h  <- get
           case h::Word16 of
             0 -> do pb <- getRemainingLazyByteString
                     return (LinePacket $ lbsTocbs pb)
             1 -> do cs <- get
                     -- pb <- get
                     pb <- getRemainingLazyByteString
                     return (OpenPacket cs (lbsTocbs pb))
             x -> do rest <- getRemainingLazyByteString
                     let
                       Just bb = i2ospOf 2 (fromIntegral x)
                     return $ PingPongPacket ((decode (BL.append (cbsTolbs bb) rest)) :: Packet)

-- ---------------------------------------------------------------------

{-
HEAD

A length of 0 means there is no HEAD included and the packet is all
binary (only BODY).

A length of 1 means there is a single byte value that is not JSON.

A length of 2+ means the HEAD must be a UTF-8 encoded JSON object or
array (not any bare string/bool/number value). If the JSON parsing
fails, the parser must return an error.
-}

data Head = HeadEmpty | HeadJson BC.ByteString
          deriving (Eq)

instance Show Head where
  show (HeadEmpty) = "HeadEmpty"
  show (HeadJson j) = "HeadJson " ++ BC.unpack j

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
          deriving (Eq,Show)

unBody :: Body -> BC.ByteString
unBody (Body b) = b


-- | A packet is carried encrypted inside the NetworkPacket
data Packet = Packet { paHead :: Head
                     , paBody :: Body
                     }
            deriving (Eq,Show)

showPacketShort :: Packet -> String
showPacketShort p = "Packet " ++ show (paHead p) ++ " " ++ show (BC.length $ unBody $ paBody p) ++ " bytes"

newPacket :: Packet
newPacket = Packet { paHead = HeadEmpty
                   , paBody = Body BC.empty
                   }

headLen :: Packet -> Int
headLen (Packet HeadEmpty _)     = 2
-- headLen (Packet (HeadByte _) _)  = 3
headLen (Packet (HeadJson bs) _) = 2 + (fromIntegral $ BC.length bs)

bodyLen :: Packet -> Int
bodyLen (Packet _ (Body bs)) = fromIntegral $ BC.length bs

packetLen :: Packet -> Int
packetLen p = headLen p + bodyLen p

networkPacketLen :: NetworkPacket -> Int
networkPacketLen (OpenPacket _ pb)  = 3 + (BC.length pb)
networkPacketLen (LinePacket   pb)  = (BC.length pb)
networkPacketLen (PingPongPacket p) = packetLen p

packetJson :: Packet -> Maybe Aeson.Value
packetJson p =
  case paHead p of
    HeadEmpty -> Nothing
    HeadJson bs -> Aeson.decode (cbsTolbs bs)

-- ---------------------------------------------------------------------

instance Binary Packet where
  put p = do let LP b = toLinePacket p
             put b

  get = do h  <- get
           -- pb <- getRemainingLazyByteString

           -- return (newPacket { paHead = h, paBody = lbsTocbs pb})
           pb <- get
           return (newPacket { paHead = h, paBody = pb})

-- ---------------------------------------------------------------------

instance Binary Head where

  put HeadEmpty    = put (0 :: Word16)
  -- put (HeadByte b) = do put (1 :: Word16)
  --                       put b
  put (HeadJson x) = do put ((fromIntegral $ BC.length x) :: Word16)
                        mapM_ put $ BC.unpack x

  get = do hb <- get :: Get Word16
           h <- case hb of
                 0 -> return HeadEmpty
     --             1 -> do b <- get
     --                     return (HeadByte b)
                 x -> do b <- getLazyByteString (fromIntegral x)
                         return (HeadJson (lbsTocbs b))
           return h

instance Binary Body where
  put (Body bs) = mapM_ put $ BC.unpack bs

  get = do bs <- getRemainingLazyByteString
           return (Body $ lbsTocbs bs)


-- ---------------------------------------------------------------------

data LinePacket = LP BC.ByteString
                deriving (Show,Eq)
unLP :: LinePacket -> BC.ByteString
unLP (LP x) = x

-- ---------------------------------------------------------------------

toNetworkPacket :: NetworkPacket -> LinePacket
toNetworkPacket (OpenPacket cs bs) = LP $ BC.append (lbsTocbs $ BL.pack [0,1,cs]) bs
toNetworkPacket (LinePacket    bs) = LP $ BC.append (lbsTocbs $ BL.pack [0,0])    bs
toNetworkPacket (PingPongPacket p) = toLinePacket p

fromNetworkPacket :: LinePacket -> Maybe NetworkPacket
fromNetworkPacket (LP bs) =
  if bs == BC.pack ""
    then Nothing
    else Just $ decode (cbsTolbs bs)

-- ---------------------------------------------------------------------

toLinePacket :: Packet -> LinePacket
toLinePacket (Packet h (Body b)) = LP $ BC.append (myencode h) b

myencode :: Head -> BC.ByteString
myencode (HeadEmpty)  = lbsTocbs $ BL.pack [0,0]
-- myencode (HeadByte b) = BL.pack [0,1,b]
myencode (HeadJson x) = BC.append (bb) x
  where
    xlen :: Integer
    xlen = fromIntegral (BC.length x)

    Just bb = i2ospOf 2 xlen

--
-- ---------------------------------------------------------------------

-- |Note: this will throw an exception is the decode fails
fromLinePacket :: LinePacket -> IO (Maybe Packet)
fromLinePacket (LP bs) = handle handler fn
  where
    fn = return  (Just $ decode (cbsTolbs bs))
    handler :: SomeException -> IO (Maybe Packet)
    handler _ = return Nothing

-- ---------------------------------------------------------------------

{-

Examples

First packet out

  This encodes a startup packet of
   {"type":"seek","c":0,"seek":"89a4cbc6c27eb913c1bcaf06bac2d8b872c7cbef626b35b6d7eaf993590d37de"}
-}

_p1 :: [Word8]
_p1 =
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

_p2 :: [Word8]
_p2 =
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


_testp1 :: IO ()
_testp1 = do
  let p1b = BL.pack _p1
  let p@(Packet _h (Body b)) = decode p1b :: Packet
  putStrLn $ show p
  putStrLn $ show (BC.length b)

_testp2 :: IO ()
_testp2 = do
  let p1b = BL.pack _p2
  let p@(Packet _h (Body b)) = decode p1b :: Packet
  putStrLn $ show p
  putStrLn $ show (BC.length b)

-- ---------------------------------------------------------------------

-- Received open packet
-- RECV from IPP "10.0.0.42:42424":"00011adee339dc7ca4227333401b8d2dc460dfa78317b6c5dea168b4679c59fbc93a2267e1c2b7cf4bfe832f0fb07221f8654a758d6a63200979f9367e046379aa1f4d27f74be6ae9367f4ff655820f2e0dedea70c6a8e32084180a464993e625803fa9774ac99a50c2e63fa637a07a2ae52860a1961f630c51d4f6779c7409c80497f52c91c69ed812261f2dcb5c1675b24d978a94fb55d9d55ecb772b542aa21c32d9dc704374dcbf53b32579e68cc3a01da6f9fd44ee1a1753919c50a09790c168d2a22069e0bd1f7e7db5410ec540c90f893956ddbdf01fc9ae5a7c82fc832ae72f846a2b1dc3a911dc13aa641fcf83f68ed1d3e6f445f5b82814649b9a127c7ad6fd2e3a8d5b986852c8bca221931e7a09ea1a2e7aff7ea090fdc8eebdd8664bb926909c396c3f7dd01ac38819a6cf7b947a855f8bdc87593e20bda115913056d6935b188308fad9a7873fb95395216d487cb5173a20296b86103715005e1ccbe3bcaae8ee64e4806928dd654a08ed8a7818d4eff2052aaa62c300c7661e678febaf34378a32028e0a3eea83cc87bc9c18742d4daafa3029df15030d7fc2cf916eab082e2424e4f912cadd319aaa39d6a8dc32c4282" at

_b16_rx_open :: String
_b16_rx_open = "00011adee339dc7ca4227333401b8d2dc460dfa78317b6c5dea168b4679c59fbc93a2267e1c2b7cf4bfe832f0fb07221f8654a758d6a63200979f9367e046379aa1f4d27f74be6ae9367f4ff655820f2e0dedea70c6a8e32084180a464993e625803fa9774ac99a50c2e63fa637a07a2ae52860a1961f630c51d4f6779c7409c80497f52c91c69ed812261f2dcb5c1675b24d978a94fb55d9d55ecb772b542aa21c32d9dc704374dcbf53b32579e68cc3a01da6f9fd44ee1a1753919c50a09790c168d2a22069e0bd1f7e7db5410ec540c90f893956ddbdf01fc9ae5a7c82fc832ae72f846a2b1dc3a911dc13aa641fcf83f68ed1d3e6f445f5b82814649b9a127c7ad6fd2e3a8d5b986852c8bca221931e7a09ea1a2e7aff7ea090fdc8eebdd8664bb926909c396c3f7dd01ac38819a6cf7b947a855f8bdc87593e20bda115913056d6935b188308fad9a7873fb95395216d487cb5173a20296b86103715005e1ccbe3bcaae8ee64e4806928dd654a08ed8a7818d4eff2052aaa62c300c7661e678febaf34378a32028e0a3eea83cc87bc9c18742d4daafa3029df15030d7fc2cf916eab082e2424e4f912cadd319aaa39d6a8dc32c4282"

_rx_open :: BL.ByteString
_rx_open = b16ToLbs _b16_rx_open

_lp_rx_open :: LinePacket
_lp_rx_open = LP (lbsTocbs _rx_open)

b16ToLbs :: String -> BL.ByteString
b16ToLbs str = cbsTolbs r
  where (r,_) = B16.decode $ BC.pack str

_headok :: Head
_headok = decode $ b16ToLbs "00011a"

_bodyok :: Body
_bodyok = decode $ b16ToLbs "dee339dc7ca4227333401b8d2dc460dfa78317b6c5dea168b4679c59fbc93a2267e1c2b7cf4bfe832f0fb07221f8654a758d6a63200979f9367e046379aa1f4d27f74be6ae9367f4ff655820f2e0dedea70c6a8e32084180a464993e625803fa9774ac99a50c2e63fa637a07a2ae52860a1961f630c51d4f6779c7409c80497f52c91c69ed812261f2dcb5c1675b24d978a94fb55d9d55ecb772b542aa21c32d9dc704374dcbf53b32579e68cc3a01da6f9fd44ee1a1753919c50a09790c168d2a22069e0bd1f7e7db5410ec540c90f893956ddbdf01fc9ae5a7c82fc832ae72f846a2b1dc3a911dc13aa641fcf83f68ed1d3e6f445f5b82814649b9a127c7ad6fd2e3a8d5b986852c8bca221931e7a09ea1a2e7aff7ea090fdc8eebdd8664bb926909c396c3f7dd01ac38819a6cf7b947a855f8bdc87593e20bda115913056d6935b188308fad9a7873fb95395216d487cb5173a20296b86103715005e1ccbe3bcaae8ee64e4806928dd654a08ed8a7818d4eff2052aaa62c300c7661e678febaf34378a32028e0a3eea83cc87bc9c18742d4daafa3029df15030d7fc2cf916eab082e2424e4f912cadd319aaa39d6a8dc32c4282"

-- _decodeok :: Maybe Packet
_decodeok = fromLinePacket _lp_rx_open

-- _decodefail :: Maybe Packet
_decodefail = fromLinePacket (LP (lbsTocbs $ b16ToLbs "08011adee339dc7ca422"))

_buggy_line_packet :: String
_buggy_line_packet = BC.unpack $ lbsTocbs $ b16ToLbs "6c3258f8cc77b7f6e7eaf963b02b2121fb9a7eec724160b3a20450c90b4bd195742bc892b6793876583b0ee51a254f7955eebd14180f44290700ae5cdec67626931c8c947d252e627ce84a130adf753cabcb33f05a72e43b26930a960a47fcc9adea862a5af26bd2f15c81e4a3c33b910dd15b2435d6c14bfdfe42c72e93078b78a2386cd61414744a8a3264b4aacea222af54d698b5e5f7e74c2c0476882f30173103fcbd2817467ddbdc7f20ae1f40b91c949a19f036d6135f4f1b4ccaf73c54de5c329510df2034a1a8b02b904f0ba8672fd8ef2024b2e6083e37821075d16371fe02622405e382352e9167c81ffcbaba2586b83006dba9a8f8aec8c660ca194c1976613c6565248bac3b3c41df2e3ed2e2715faa4f901514bf92bf2d314bedd1abfb41ed46b5f4891dd0a7ee22892b55337ac6d5d6eee4883a4cbb258924399683cbb0e7e6a7bdf18bd70f50748994b6db0f52ba1b25328e2faa5c7db7c080124dd9dc4e0831c6574ede3bc9971557ff0b4bc1daa421b7d5d0c64579e2951c541934363c60e32ab0462e30de4d396ea6f01a62686e1c5307e7feb2"


_buggy_line_packet2 :: String
_buggy_line_packet2 = BC.unpack $ lbsTocbs $ b16ToLbs "0208b6d2ea2e7f81ddcbd196decd080f0903b727aec7edc7073e5c70b559fabf48fa2edec45f9c783276f4c80510de181748487fde0d8e4c23145147a9425d721b8d9522dfd71d1f33d9d558de8c72566b557b0e7869dcf36a7125260175dc5c9f7d48a17292630fe02843b460e2dbf42723eaa50dbbce03afcbf67f19f4e7af0e692778199d744eed7bf4b94220100374fb0ed270e450ac9815ed2b9ca160bb184d6914c28b0f5b3675482f27c13dea1591d0f8ba623264acad6dbda5207274da181be104182fa0cfd49525373bab4e57d7fc10dfa518a18d3ef7b2f308715107f5c78365a8103b7b45bc586eab67a4c14ef9ed1a4890f068a64f88b6081a6a77b1bd3a7f74c31c618273afaed958ffbe8c8f7ba309af245fc63ebd4260ae96730affcdc2184aaa26af63c391065c0249c51fd6e2a6b877008e8532d4fe7beff135619559ef2936213541f545ead91dbd822845904521c5f27892d785ef1444a60099d1743451ef3d277676a9a1c41173e2122c959a591e43f32c46abcb1167c8cb76f47803d62ce344d2d05151215024"

