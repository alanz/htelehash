{-# LANGUAGE OverloadedStrings #-}

module TeleHash.Telex
       (
         IPP(..)
       , Hash(..)
       , unHash
       , mkHash
       , Tap(..)
       , Telex(..)
       ) where

import Control.Applicative ((<$>), (<*>), empty, pure)
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text, pack, unpack)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

data Tap = Tap { tapIs :: (String,String),
                 tapHas :: [String] }
           deriving (Eq,Show)

instance ToJSON Tap where
  toJSON (Tap is has)
                       = object
                         [
                           ("is",  String $ T.pack (show is)),
                           ("has", String $ T.pack (show has))
                         ]

instance FromJSON Tap where
  parseJSON (Object v) = Tap <$>
                         v .:* "is" <*>
                         v .: "has"
  parseJSON _          = empty

(.:*) :: Object -> Text -> Parser (String,String)
obj .:* key = case H.lookup key obj of
               Nothing -> fail $ "key " ++ show key ++ " not present"
               -- Just v  -> parseJSON v
               -- Just (Object v)  -> pure ((show (head $ map (\(k,String vv) -> (k,T.unpack vv)) $ H.toList v)),"xxx")
               Just (Object v)  -> pure (head $ map (\(k,String vv) -> (T.unpack k,T.unpack vv)) $ H.toList v)
{-# INLINE (.:*) #-}


newtype Hash = Hash String
             deriving (Eq,Show,Ord)
unHash :: Hash -> String
unHash (Hash str) = str

instance FromJSON Hash where
  parseJSON (String x) = pure (Hash $ T.unpack x)
  parseJSON x = pure (Hash $ show x)
  -- parseJSON _          = empty

instance ToJSON Hash where
  toJSON (Hash h) = String $ T.pack h

newtype IPP = IPP String
             deriving (Eq,Show,Ord)
unIPP :: IPP -> String
unIPP (IPP str) = str

instance FromJSON IPP where
  parseJSON (String x) = pure (IPP $ T.unpack x)
  parseJSON x = pure (IPP $ show x)
  -- parseJSON _          = empty

instance ToJSON IPP where
  toJSON (IPP ipp) = String $ T.pack ipp

data Telex = Telex
             {
               teleRing   :: Maybe Int
             , teleSee    :: Maybe [IPP]
             , teleBr     :: Int
             , teleTo     :: IPP
             , teleLine   :: Maybe Int
             , teleHop    :: Maybe Int
             , teleSigEnd :: Maybe Hash
             , teleSigPop :: Maybe String
             , teleTap    :: Maybe [Tap]
             -- , teleRest   :: Map.Map String String
             -- , teleMsgLength :: Maybe Int -- length of received Telex
             } deriving (Show)

-- A ToJSON instance allows us to encode a value as JSON.

instance ToJSON Telex where
  -- toJSON (Telex tRing tSee tBr tTo tLine tHop tSigEnd tSigPop tTap)
  toJSON (Telex tRing tSee tBr tTo tLine tHop tSigEnd tSigPop tTap)
                       = object $ stripNulls
                         [
                           "_ring" .= tRing
                         , ".see"  .= tSee
                         ,  "_br"  .= tBr
                         , "_to"   .= tTo
                         , "_line" .= tLine
                         , "_hop"  .= tHop
                         , "+end"  .= tSigEnd
                         , "+pop"  .= tSigPop
                         , ".tap"  .= tTap
                         ]

-- A FromJSON instance allows us to decode a value from JSON.  This
-- should match the format used by the ToJSON instance.

instance FromJSON Telex where
  parseJSON (Object v) = Telex <$>
                         v .:? "_ring" <*>
                         v .:? ".see"  <*>
                         v .:  "_br"   <*>
                         v .:  "_to"   <*>
                         v .:? "_line" <*>
                         v .:? "_hop"  <*>
                         v .:? "+end"  <*>
                         v .:? "+pop"  <*>
                         v .:? ".tap"

  parseJSON _          = empty

stripNulls :: [Pair] -> [Pair]
stripNulls xs = filter (\(_,v) -> v /= Null) xs

-- ---------------------------------------------------------------------
{-
/**
 * Hash objects represent a message digest of string content,
 * with methods useful to DHT calculations.
 */
-}

mkHash :: IPP -> Hash
mkHash (IPP str) =
  let
    digest = SHA.sha1 $ BL.fromChunks [BC.pack str]
  in
   -- B64.encode $ BL.unpack $ SHA.bytestringDigest digest
   Hash (show digest)



-- ---------------------------------------------------------------------
{-
main :: IO ()
main = do
  let req = decode "{\"x\":3.0,\"y\":-1.0}" :: Maybe Telex
  print req
  let reply = Telex (Just 1) 2 (Just 3) (Just 4) (Just "h")
  BL.putStrLn (encode reply)
-}
main :: IO ()
main = do
  -- let req = decode "{\"_to\":\"196.209.236.208:38636\",\"+end\":\"9e307c287a8a870919019d8315809d44b1ff8801\",\".see\":[\"76.14.65.244:34004\"],\".tap\":[{\"is\":{\"+end\":\"9e307c287a8a870919019d8315809d44b1ff8801\"},\"has\":[\"+pop\"]}],\"_ring\":15099,\"_br\":0}" :: Maybe Telex
  let req = decode "{\"_to\":\"196.209.236.208:38636\",\"+end\":\"9e307c287a8a870919019d8315809d44b1ff8801\",\".see\":[\"76.14.65.244:34004\"],\".tap\":[{\"is\":{\"+end\":\"9e307c287a8a870919019d8315809d44b1ff8801\"},\"has\":[\"+pop\"]}],\"_ring\":15099,\"_br\":0}" :: Maybe Telex
  -- let req = decode "{\"_br\":1,\"_to\":\"ipp1-hooray\"}" :: Maybe Telex
  print req
  let reply = Telex Nothing Nothing 1 (IPP "ipp1") Nothing Nothing Nothing Nothing Nothing
  BL.putStrLn (encode reply)


-- EOF
