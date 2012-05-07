{-# LANGUAGE DeriveDataTypeable #-}

module TeleHash.TelexOld
       (
         IPP(..)
       , unIPP
       , Hash(..)
       , unHash
       , mkHash
       , Tap(..)
       , Telex(..)
       , mkTelex
       , encodeTelex
       , parseTelex
       ) where

import Data.List
import Text.JSON
import Text.JSON.Generic
import Text.JSON.Types
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Map as Map

-- ---------------------------------------------------------------------

newtype Hash = Hash String
             deriving (Data,Eq,Show,Typeable,Ord)
unHash :: Hash -> String
unHash (Hash str) = str

newtype IPP = IPP String
             deriving (Data,Eq,Show,Typeable,Ord)
unIPP :: IPP -> String
unIPP (IPP str) = str

-- ---------------------------------------------------------------------

-- JSON stuff for a Telex

data Tap = Tap { tapIs :: (String,String),
                 tapHas :: [String] }
         deriving (Data, Typeable, -- For Text.JSON
                   Eq, Show)

data Telex = Telex
             { teleRing   :: Maybe Int
             , teleSee    :: Maybe [IPP]
             , teleBr     :: Int
             , teleTo     :: IPP
             , teleLine   :: Maybe Int
             , teleHop    :: Maybe Int
             , teleSigEnd :: Maybe Hash
             , teleSigPop :: Maybe String
             , teleTap    :: Maybe [Tap]
             , teleRest   :: Map.Map String String
             , teleMsgLength :: Maybe Int -- length of received Telex
             } deriving (Data, Typeable, -- For Text.JSON
                                 Eq, Show)

parseTelex :: String -> Maybe Telex
parseTelex s =
    let
      decoded = (decode s :: Result JSValue)
      Ok (JSObject cc) = decoded

      Just to   = getStringMaybe      cc "_to"
      maybeRing = getIntMaybe         cc "_ring"
      maybeSee  = getStringArrayMaybe cc ".see"
      Just br   = getIntMaybe         cc "_br"
      maybeLine = getIntMaybe         cc "_line"
      maybeHop  = getIntMaybe         cc "_hop"
      maybeEnd  = getHashMaybe        cc "+end"
      maybeTap  = getTapMaybe         cc ".tap"
      maybePop  = getStringMaybe      cc "+pop"
      msgLength = length s

      maybeSee' = case maybeSee of
        Nothing -> Nothing
        Just see -> Just $ map (\ss -> (IPP ss)) see
    in
     -- mkTelex to
     case decoded of
       Ok _ ->
         Just (mkTelex (IPP to)) {teleRing = maybeRing,
                   teleSee = maybeSee',
                   teleBr = br,
                   teleTo = (IPP to), teleLine = maybeLine, teleHop = maybeHop,
                   teleSigEnd = maybeEnd, teleTap = maybeTap,
                   teleSigPop = maybePop,
                   teleRest = Map.fromList $ map (\(name,val) -> (name, encode val)) (fromJSObject cc), -- horrible, but will do for now
                   teleMsgLength = Just msgLength }
       _ -> Nothing

-- ---------------------------------------------------------------------
-- Pretty sure these two exist in JSON somewhere, do not know how to find them

getStringMaybe :: JSObject JSValue -> String -> Maybe String
getStringMaybe cc field =
  case (get_field cc field) of
      Just (JSString jsStrVal)  -> Just (fromJSString jsStrVal)
      _                         -> Nothing

getHashMaybe :: JSObject JSValue -> String -> Maybe Hash
getHashMaybe cc field =
  case (get_field cc field) of
      Just (JSString jsStrVal)  -> Just (Hash $ fromJSString jsStrVal)
      _                         -> Nothing

getStringArrayMaybe :: JSObject JSValue -> String -> Maybe [String]
getStringArrayMaybe cc field =
  case (get_field cc field) of
      Just (JSArray jsStrArrVal)-> Just (map getStr jsStrArrVal)
      _                         -> Nothing
  where
    getStr (JSString jsStrVal) = fromJSString jsStrVal
    getStr x                   = "getStringArrayMaybe:oops:getstr :" ++ (show x)

getIntMaybe :: JSObject JSValue -> String -> Maybe Int
getIntMaybe cc field =
  case (get_field cc field) of
      Just (JSRational _ jsVal)  -> Just $ round (jsVal)
      Just (JSString jsStrVal)  -> Just $ read (fromJSString jsStrVal)
      _                         -> Nothing

{-
(".tap",JSArray
  [JSObject (JSONObject
     {fromJSObject = [
        ("is",JSObject (JSONObject
           {fromJSObject = [("+end",JSString (JSONString {fromJSString = "9fa23aa9f9ac828ced5a151fedd24af0d9fa8495"}))]})),
        ("has",JSArray [JSString (JSONString {fromJSString = "+pop"})])
        ]
     })])
-}
-- ".tap":[{"is":{"+end":"9fa23aa9f9ac828ced5a151fedd24af0d9fa8495"},"has":["+pop"]}]
getTapMaybe :: JSObject JSValue -> String -> Maybe [Tap]
getTapMaybe cc field =
  case (get_field cc field) of
      Just (JSArray jsArrVal)-> Just (map getTap jsArrVal)
      _                         -> Nothing
  where
    getTap (JSObject jsobj) = foldl' dTap (Tap ("","") []) $ fromJSObject jsobj
    getTap _                = error "Should never happen"

    dTap tap ("is",  JSObject o) = tap {tapIs = (k, fromJSString str)}
      where
        (k,JSString str) = head $ fromJSObject o

    --dTap tap ("has", o) = tap {tapHas= [show o]}
    dTap tap ("has", JSArray arr) = tap {tapHas= map (\(JSString s) -> fromJSString s) arr}
    dTap _   _                    = error "Should never happen"

-- ---------------------------------------------------------------------

_test_parseTelex :: Maybe Telex
_test_parseTelex = parseTelex _inp

_inp :: [Char]
_inp = ("{\"_ring\":17904," ++
       "\".see\":[ \"208.68.163.247:42424\", \"208.68.160.25:55137\"]," ++
       "\"_br\":52," ++
       "\"_to\":\"173.19.99.143:63681\" }")

_inp2 :: [Char]
_inp2 = "{\"_to\":\"208.68.163.247:42424\",\"+end\":\"9fa23aa9f9ac828ced5a151fedd24af0d9fa8495\",\".see\":[\"196.215.128.240:51602\"],\".tap\":[{\"is\":{\"+end\":\"9fa23aa9f9ac828ced5a151fedd24af0d9fa8495\"},\"has\":[\"+pop\"]}],\"_line\":35486388,\"_br\":174}"

_inp3 :: [Char]
_inp3 = "{\"+end\":\"9fa23aa9f9ac828ced5a151fedd24af0d9fa8495\",\"_to\":\"208.68.163.247:42424\",\".see\":[\"196.215.128.240:51602\"],\".tap\":[{\"is\":{\"+end\":\"9fa23aa9f9ac828ced5a151fedd24af0d9fa8495\"},\"has\":[\"+pop\"]}],\"_line\":35486388,\"_br\":174}"

_tx1 :: Maybe Telex
_tx1 = parseTelex _inp
_tx2 :: Maybe Telex
_tx2 = parseTelex _inp2
_tx3 :: Maybe Telex
_tx3 = parseTelex _inp3

-- ---------------------------------------------------------------------

encodeTelex :: Telex -> String
encodeTelex t =
  let
    to   = [("_to",   JSString $ toJSString $ unIPP $ teleTo t)]

    ring = case (teleRing t) of
      Just r -> [("_ring", JSString $ toJSString $ show r)]
      Nothing -> []

    see = case (teleSee t) of
      Just r -> [(".see", showJSON $ JSArray (map (\s -> JSString (toJSString (unIPP s))) r))]
      Nothing -> []

    -- br = [("_br", JSString $ toJSString $ show (teleBr t))]
    br = [("_br", JSRational False (fromIntegral (teleBr t)))]

    line = case (teleLine t) of
      Just r -> [("_line", JSString $ toJSString $ show r)]
      Nothing -> []

    hop = case (teleHop t) of
      -- Just r -> [("_hop", JSString $ toJSString $ show r)]
      Just r -> [("_hop", JSRational False (fromIntegral r))]
      Nothing -> []

    end = case (teleSigEnd t) of
      Just (Hash r) -> [("+end", JSString $ toJSString r)]
      Nothing -> []

    pop = case (teleSigPop t) of
      Just r -> [("+pop", JSString $ toJSString r)]
      Nothing -> []

    tap = case (teleTap t) of
      Just r -> [(".tap", JSArray $ map toJSTap r)]
      Nothing -> []

    toJSTap atap = JSObject $ toJSObject [
      ("is", JSObject $ toJSObject [
          (k, JSString $ toJSString v),
          ("has", showJSON $ JSArray (map (\s -> JSString (toJSString s)) (tapHas atap)))
          ])]
      where
      (k,v) = tapIs atap


  in
   -- encode $ toJSObject (to++ring++see++br++line++hop++end++tap++pop)
   encode $    toJSObject (to++end++ring++see++tap++line++br++hop++pop)

-- ---------------------------------------------------------------------

mkTelex :: IPP -> Telex
mkTelex seedIPP =
    -- set _to = seedIPP
  -- Telex Nothing Nothing 0 (T.pack seedIPP) Nothing Nothing Nothing Map.empty -- Nothing
  Telex Nothing Nothing 0 seedIPP Nothing Nothing Nothing Nothing Nothing Map.empty Nothing

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


-- EOF
