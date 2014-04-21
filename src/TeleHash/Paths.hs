{-# LANGUAGE OverloadedStrings #-}

module TeleHash.Paths
  (
    PathJson(..)
  , PathIPv4(..)
  , PathIPv6(..)
  , PathHttp(..)
  , PathWebRtc(..)
  , PathType(..)
  , showPathJson
  , pjsonType
  , pjsonIp
  , pjsonPort
  , pjsonHttp
  , Port
  , Url
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
                 } deriving (Show,Eq,Ord)

data PathIPv6 = PathIPv6
                 { v6Ip :: IP
                 , v6Port :: Port
                 } deriving (Show,Eq,Ord)

data PathHttp = PathHttp
                 { url :: Url
                 } deriving (Show,Eq,Ord)

data PathWebRtc = PathWebRtc
                 { webrtcId :: String
                 } deriving (Show,Eq,Ord)


-- ---------------------------------------------------------------------

data PathJson = PIPv4   PathIPv4
              | PIPv6   PathIPv6
              | PHttp   PathHttp
              | PWebRtc PathWebRtc
              deriving (Show,Eq,Ord)

showPathJson :: PathJson -> String
showPathJson (PIPv4 (PathIPv4 ip port)) = "{ type: 'ipv4', ip: '" ++ show ip ++ "', port: " ++ show port ++ "}"
showPathJson (PIPv6 (PathIPv6 ip port)) = "{ type: 'ipv6', ip: '" ++ show ip ++ "', port: " ++ show port ++ "}"
showPathJson p = show p

instance ToJSON PathJson where
  toJSON (PIPv4 (PathIPv4 ip port))
       = Object $ HM.fromList [("type",String "ipv4")
                              ,("ip",String $ T.pack $ show ip)
                              ,("port",Number $ fromIntegral port)]
  toJSON (PIPv6 (PathIPv6 ip port))
       = Object $ HM.fromList [("type",String "ipv6")
                              ,("ip",String $ T.pack $ show ip)
                              ,("port",Number $ fromIntegral port)]

-- ---------------------------------------------------------------------

data PathType = PtIPv4
          | PtIPv6
          | PtHttp
          | PtWebRtc
          | PtRelay -- unmatched
          | PtLocal -- unmatched
          | PtBlueTooth -- unmatched
          deriving (Show,Eq,Ord)

-- ---------------------------------------------------------------------

pjsonType :: PathJson -> PathType
pjsonType (PIPv4 _) = PtIPv4
pjsonType (PIPv6 _) = PtIPv6
pjsonType (PHttp _) = PtHttp
pjsonType (PWebRtc _) = PtWebRtc

pjsonIp :: PathJson -> Maybe IP
pjsonIp (PIPv4 (PathIPv4 ip _port)) = Just ip
pjsonIp (PIPv6 (PathIPv6 ip _port)) = Just ip
pjsonIp _ = Nothing

pjsonPort :: PathJson -> Maybe Port
pjsonPort (PIPv4 (PathIPv4 _ip port)) = Just port
pjsonPort (PIPv6 (PathIPv6 _ip port)) = Just port
pjsonPort _ = Nothing

pjsonHttp :: PathJson -> Maybe Url
pjsonHttp (PHttp (PathHttp u)) = Just u
pjsonHttp _ = Nothing

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

instance FromJSON PathJson where
  parseJSON v = mIP4 <|> mIP6 <|> mHttp <|> mWebRtc
     where
       mIP4 = PIPv4 <$> (parseJSON v)
       mIP6 = PIPv6 <$> (parseJSON v)
       mHttp = PHttp <$> (parseJSON v)
       mWebRtc = PWebRtc <$> (parseJSON v)


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
tPath = decode pathJs :: Maybe PathJson

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

tPaths = decode pathaJs :: Maybe [PathJson]

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
