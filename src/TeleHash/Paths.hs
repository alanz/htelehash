{-# LANGUAGE OverloadedStrings #-}

module TeleHash.Paths
  (
    Path(..)
  , PathIPv4(..)
  , PathIPv6(..)
  , PathHttp(..)
  , PathWebRtc(..)
  ) where

import Control.Applicative
import Data.Aeson
import Data.IP
import TeleHash.Convert

import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

-- ---------------------------------------------------------------------

type Port = Int
type Url  = String

-- ---------------------------------------------------------------------

data PathIPv4 = PathIPv4
                 { v4Ip :: IP
                 , v4Port :: Port
                 } deriving Show

data PathIPv6 = PathIPv6
                 { v6Ip :: IP
                 , v6Port :: Port
                 } deriving Show

data PathHttp = PathHttp
                 { url :: Url
                 } deriving Show

data PathWebRtc = PathWebRtc
                 { webrtcId :: String
                 } deriving Show


-- ---------------------------------------------------------------------

data Path = PtIPv4   PathIPv4
          | PtIPv6   PathIPv6
          | PtHttp   PathHttp
          | PtWebRtc PathWebRtc
          deriving Show

-- ---------------------------------------------------------------------

-- Aeson instances

 -- {"type":"ipv4","ip":"10.0.0.42","port":42424}
instance FromJSON PathIPv4 where
  parseJSON (Object v) =
    case (HM.lookup "type" v) of
      Just (String "ipv4") ->
         PathIPv4 <$>
         (v .: "ip")  <*>
         (v .: "port")
      _ -> fail "wrong or missing type:ipv4"
  parseJSON _ = fail "parseJSON PathIPv4 expecting Object"
 
instance FromJSON PathIPv6 where
  parseJSON (Object v) =
    case (HM.lookup "type" v) of
      Just (String "ipv6") ->
         PathIPv6 <$>
         (v .: "ip")  <*>
         (v .: "port")
      _ -> fail "wrong or missing type:ipv6"
  parseJSON _ = fail "parseJSON PathIPv6 expecting Object"

--  {"type":"http","http":"http://172.17.42.1:42424"}
instance FromJSON PathHttp where
  parseJSON (Object v) =
    case (HM.lookup "type" v) of
      Just (String "http") ->
         PathHttp <$>
         (v .: "http")
      _ -> fail "wrong or missing type:http"
  parseJSON _ = fail "parseJSON PathHttp expecting Object"

instance FromJSON PathWebRtc where
  parseJSON (Object v) =
    case (HM.lookup "type" v) of
      Just (String "id") ->
         PathWebRtc <$>
         (v .: "id")
      _ -> fail "wrong or missing type:id"
  parseJSON _ = fail "parseJSON PathWebRtc expecting Object"

instance FromJSON Path where
  parseJSON v = mIP4 <|> mIP6 <|> mHttp <|> mWebRtc
     where
       mIP4 = PtIPv4 <$> (parseJSON v)
       mIP6 = PtIPv6 <$> (parseJSON v)
       mHttp = PtHttp <$> (parseJSON v)
       mWebRtc = PtWebRtc <$> (parseJSON v)


instance FromJSON IP where
  parseJSON (String s) = pure $ (read $ T.unpack s)
  parseJSON _          = fail "Unknown IP type"

-- ---------------------------------------------------------------------
-- Testing
-- ---------------------------------------------------------------------

pathsJs = cbsTolbs $ BC.pack (
  "{\"path\":{\"type\":\"ipv4\",\"ip\":\"10.0.0.42\",\"port\":55794},"++
   "\"priority\":1,"++
   "\"paths\":"++
   "[{\"type\":\"http\",\"http\":\"http://172.17.42.1:42424\"},"++
    "{\"type\":\"ipv4\",\"ip\":\"10.0.0.42\",\"port\":42424},"++
    "{\"type\":\"ipv4\",\"ip\":\"172.17.42.1\",\"port\":42424},"++
    "{\"type\":\"ipv6\",\"ip\":\"fe80::b6b6:76ff:fe86:67cb\",\"port\":42424},"++
    "{\"type\":\"ipv6\",\"ip\":\"fe80::106b:48ff:fe51:3abe\",\"port\":42424},"++
    "{\"type\":\"ipv6\",\"ip\":\"fe80::d4d1:12ff:fec5:cb33\",\"port\":42424}"++
   "],"++
   "\"type\":\"path\","++
   "\"c\":2}"
   )

pathJs = cbsTolbs $ BC.pack "{\"type\":\"ipv4\",\"ip\":\"10.0.0.42\",\"port\":55794}"

tIpv4 = decode pathJs :: Maybe PathIPv4
tPath = decode pathJs :: Maybe Path

pathaJs = cbsTolbs $ BC.pack (
   -- "{\"path\":" ++
   "["++
     "{\"type\":\"http\",\"http\":\"http://172.17.42.1:42424\"}"++
    ",{\"type\":\"ipv4\",\"ip\":\"10.0.0.42\",\"port\":42424}"++
    ",{\"type\":\"ipv4\",\"ip\":\"172.17.42.1\",\"port\":42424}"++
    ",{\"type\":\"ipv6\",\"ip\":\"fe80::b6b6:76ff:fe86:67cb\",\"port\":42424}"++
    ",{\"type\":\"ipv6\",\"ip\":\"fe80::106b:48ff:fe51:3abe\",\"port\":42424}"++
    ",{\"type\":\"ipv6\",\"ip\":\"fe80::d4d1:12ff:fec5:cb33\",\"port\":42424}"++
   "]"
   -- "}"
   )

tPaths = decode pathaJs :: Maybe [Path]

pathhJs = cbsTolbs $ BC.pack (
     "{\"type\":\"http\",\"http\":\"http://172.17.42.1:42424\"}"
   )

tPathh = decode pathhJs :: Maybe PathHttp


{-
 λ> decode "[1]" :: Maybe [Int] Just [1]
 λ> :m + Data.Typeable Data.Data
 λ> :set -XDeriveDataTypeable
 λ> data Person = Person { personName :: String, personAge :: Int } deriving (Data,Typeable,Show)
 λ> encode Person { personName = "Chris", personAge = 123 } "{\"personAge\":123,\"personName\":\"Chris\"}"
 λ> decode "{\"personAge\":123,\"personName\":\"Chris\"}" :: Maybe Person
 Just (Person {
 personName = "Chris", personAge = 123
 })
-}
