module TeleHash.TeleHash
       (
         initialize
       , seed
       , listen
       , connect
       , send
       , tap
       , dial
       , announce
       , ping
       , shutdown
       ) where

import Control.Monad.State
import Network.Socket
import System.IO
import System.Time
import Text.JSON
import TeleHash.Telex
import qualified Data.Map as Map
import qualified Data.Set as Set


--
-- | The 'TeleHash' monad, a wrapper over IO, carrying the switch's immutable state.
--
type TeleHash = StateT Switch IO

--
-- | The state variable for a given TeleHash Switch
data Switch = Switch { swH :: Maybe SocketHandle
                     , swSeeds :: [String] -- IPP?
                     , swSeedsIndex :: Set.Set IPP
                     , swConnected :: Bool
                     , swMaster :: Map.Map Hash Line
                     , swSelfIpp :: Maybe IPP
                     , swSelfHash :: Maybe Hash
                     , swTaps :: [Tap]
                     , swCountOnline :: Int
                     , swCountTx :: Int
                     , swCountRx :: Int
                     , swSender :: (String -> SockAddr -> TeleHash ())
                       } deriving (Eq,Show)


data SocketHandle =
    SocketHandle {slSocket :: Socket
                 --, slAddress :: SockAddr
                 } deriving (Eq,Show)

data Line = Line {
  lineIpp       :: IPP,    -- IP and port of the destination
  lineEnd       :: Hash,   -- Hash of the ipp (endpoint)
  lineHost      :: String, -- Split out host IP
  linePort      :: String, -- Split out port
  lineRingout   :: Int,    --  rand(32768),
  lineRingin    :: Maybe Int,
  lineLine      :: Maybe Int, -- When line is live, product of our ringout and their ringin
  lineInit      :: ClockTime,
  lineSeenat    :: Maybe ClockTime,
  lineSentat    :: Maybe ClockTime,
  lineLineat    :: Maybe ClockTime,
  lineTapLast   :: Maybe ClockTime,
  lineBr        :: Int,
  lineBrout     :: Int,
  lineBrin      :: Int,
  lineBsent     :: Int,
  lineNeighbors :: Set.Set Hash, -- lineNeighbors,
  lineVisible   :: Bool,
  lineVisibled  :: Bool,
  lineRules     :: [Tap]
  } deriving (Eq,Show)

{-
newtype Hash = Hash String
--             deriving (Data,Eq,Show,Typeable,Ord)
             deriving (Eq,Show,Ord)
unHash :: Hash -> String
unHash (Hash str) = str


newtype IPP = IPP String
             -- deriving (Data,Eq,Show,Typeable,Ord)
             deriving (Eq,Show,Ord)
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
-}


initialize = undefined

seed callback = undefined

listen = undefined

connect = undefined

send = undefined

tap = undefined

dial = undefined

announce = undefined

ping = undefined

shutdown = undefined

-- ---------------------------------------------------------------------

pingSeeds :: TeleHash ()
pingSeeds = do
  seeds <- gets swSeeds
  connected <- gets swConnected

  -- logT $ "pingSeeds:" ++ (show connected) ++ " " ++ (show seeds)

  -- TODO: rotate the seeds, so the we use a fresh one each time through
  case (not connected) && (seeds /= []) of
    True -> do
      nextSeed <- rotateToNextSeed
      pingSeed nextSeed
    False -> return ()

-- ---------------------------------------------------------------------

purgeSeeds :: TeleHash ()
purgeSeeds = do
  {-
    self.seeds.forEach(function (ipp) {
        slib.getSwitch(ipp).drop();
    });
  -}

-- ---------------------------------------------------------------------

rotateToNextSeed :: TeleHash String
rotateToNextSeed = do
  seeds <- gets swSeeds
  s <- get
  case seeds of
    [] -> return ""
    _  -> do
      put (s { swSeeds = ((tail seeds) ++ [head seeds]) })
      return (head seeds)

-- ---------------------------------------------------------------------

pingSeed :: String -> TeleHash ()
pingSeed seed =
  do
    -- logT ( "pingSeed " ++ (show seed))

    (_serveraddr,ip,port) <- io (resolveToSeedIPP seed)

    --logT ( "pingSeed serveraddr=" ++ (show serveraddr))

    let seedIPP = IPP (ip ++ ":" ++ port)
    -- console.log(["SEEDING[", seedIPP, "]"].join(""));
    logT ( "SEEDING[" ++ (show seedIPP))

    switch <- get
    put switch {swSeedsIndex = Set.insert seedIPP (swSeedsIndex switch) }

    timeNow <- io getClockTime

    line <- getOrCreateLine seedIPP timeNow
    let bootTelex = mkTelex seedIPP
    -- // any end will do, might as well ask for their neighborhood
    let bootTelex' = bootTelex { teleSigEnd = Just $ (lineEnd line) }

    sendTelex bootTelex'

    return ()

-- ---------------------------------------------------------------------


-- EOF

