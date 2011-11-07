{-# LANGUAGE TemplateHaskell #-}
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.HUnit
import Test.QuickCheck

import Control.Concurrent
import Control.Monad.State
import Data.Knob
import Data.List
import Data.Maybe
import Network.Socket
import System.IO
import System.Log.Handler.Simple
import System.Log.Logger
import System.Time
import TeleHash.Controller
--import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System.Random as R

-- Testing, see http://hackage.haskell.org/package/test-framework-th
main = $(defaultMainGenerator)

-- ---------------------------------------------------------------------

case_seeVisible1 :: Assertion
case_seeVisible1 = do
  let line2 = mkLine ipp2 (TOD 1000 999) 4567
  (res,state) <- runStateT (seeVisible False line2 ipp1 ipp2 ) st1

  case (state == st1) of
    True  -> return ()
    False -> assertFailure ("State has been modified:" ++ (show state))

-- ---------------------------------------------------------------------

case_seeVisible2 :: Assertion
case_seeVisible2 = do
  let line = mkLine ipp2 (TOD 1000 999) 4567
  (res,state) <- (runStateT (seeVisible True line ipp1 ipp2 ) st2)

  let
    Just lineNew = getLineMaybe (swMaster state) hash2

  case (lineVisible line == lineVisible lineNew) of
    True  -> assertFailure ("Line is the same:" ++ (show line))
    False -> return ()

-- ---------------------------------------------------------------------

-- Note: near_to :: IPP -> IPP -> TeleHash [Hash]
--       near_to end ipp
case_near_to1 :: Assertion
case_near_to1 = do
  -- Check that a line exists for the given ipp. If not, should get an undefined result
  let
    st = (Switch {swH = Nothing
                 , swSeeds = []
                 , swSeedsIndex = (Set.fromList [])
                 , swConnected = False
                 , swMaster = Map.empty
                 , swSelfIpp = Nothing
                 , swSelfHash = Nothing
                 , swTaps = []
                 , swCountOnline = 0
                 , swCountTx = 0
                 , swCountRx = 0
                 , swSender = doNullSendDgram
                 })

  (res,state) <- runStateT (near_to hash1 ipp2 ) st

  case res of
    Left errStr -> return ()
    Right _     -> assertFailure "found non-existent line"


-- ---------------------------------------------------------------------

case_near_to2 :: Assertion
case_near_to2 = do
  -- Check that a line exists for the given ipp. If not, should get an undefined result
  let
    line2 = mkLine ipp2 (TOD 1000 999) 2345
    st = (Switch {swH = Nothing
                 , swSeeds = []
                 , swSeedsIndex = (Set.fromList [])
                 , swConnected = False
                 , swMaster = Map.fromList [(mkHash ipp2, line2)]
                 , swSelfIpp = Nothing
                 , swSelfHash = Nothing
                 , swTaps = []
                 , swCountOnline = 0
                 , swCountTx = 0
                 , swCountRx = 0
                 , swSender = doNullSendDgram
                 })

  (res,state) <- runStateT (near_to hash1 ipp2) st

  case res of
    Left "empty see list" -> return ()
    Right _     -> assertFailure "oops"

-- ---------------------------------------------------------------------

st1 = (Switch {swH = Nothing
              , swSeeds = []
              , swSeedsIndex = (Set.fromList [])
              , swConnected = False
              , swMaster = Map.empty
              , swSelfIpp = Nothing
              , swSelfHash = Nothing
              , swTaps = []
              , swCountOnline = 0
              , swCountTx = 0
              , swCountRx = 0
              , swSender = doNullSendDgram
              })

st2 =
  let
    line1 = (mkLine ipp1 (TOD 1000 999)) 1234
    line2 = (mkLine ipp2 (TOD 1002 999)) 2345
    line3 = (mkLine ipp3 (TOD 1003 999)) 3456
  in
   (Switch {swH = Nothing
           , swSeeds = []
           , swSeedsIndex = (Set.fromList [])
           , swConnected = False
           , swMaster = Map.fromList
                        [
                          (hash1,line1 {lineNeighbors = Set.fromList [hash1,hash2]}) -- ipp under test
                        , (hash2,line2 {lineVisible = True })-- first neighbour
                        , (hash3,line3 {lineVisible = True })-- second neighbour
                        ]
           , swSelfIpp = Nothing
           , swSelfHash = Nothing
           , swTaps = []
           , swCountOnline = 0
           , swCountTx = 0
           , swCountRx = 0
           , swSender = doNullSendDgram
           })

ipp1 = IPP "1.2.3.4:1234"
ipp2 = IPP "2.3.4.5:2345"
ipp3 = IPP "3.4.5.6:3456"
ipp4 = IPP "4.5.6.7:4567"
hash1 = mkHash ipp1
hash2 = mkHash ipp2
hash3 = mkHash ipp3
hash4 = mkHash ipp4

case_near_to3 :: Assertion
case_near_to3 = do
  -- Check that the ipp has some visible neighbours
  (res,state) <- runStateT (near_to hash1 ipp2 ) st2

  case res of
    Left msgStr -> assertFailure msgStr
    Right [Hash "0ec331643f4a7a74068ea47dda062b48b419c832"] -> return ()

-- ---------------------------------------------------------------------

case_parseMsg1Js =
  parseTelex msg1Js @?= (
    Just Telex {teleRing = Nothing, teleSee = Just [IPP "208.68.163.247:42424"],
           teleBr = 74, teleTo = IPP "196.209.236.12:34963",
           teleLine = Just 412367436,
           teleHop = Nothing,
           teleSigEnd = Just $ Hash "38666817e1b38470644e004b9356c1622368fa57",
           teleSigPop = Nothing,
           teleTap = Just [Tap {tapIs = ("+end","38666817e1b38470644e004b9356c1622368fa57"),
                                tapHas = ["+pop"]}],
           teleRest = Map.fromList [(".tap","[{\"has\":[\"+pop\"],\"is\":{\"+end\":\"38666817e1b38470644e004b9356c1622368fa57\"}}]"),("_line","412367436"),(".see","[\"208.68.163.247:42424\"]"),("_br","74"),("+end","\"38666817e1b38470644e004b9356c1622368fa57\""),("_to","\"196.209.236.12:34963\"")],
           teleMsgLength = Just (length msg1Js)
          })

msg1Js = "{\".tap\":[{\"has\":[\"+pop\"],\"is\":{\"+end\":\"38666817e1b38470644e004b9356c1622368fa57\"}}],"++
       "\"_line\":412367436,"++
       "\".see\":[\"208.68.163.247:42424\"],"++
       "\"_br\":74,"++
       "\"+end\":\"38666817e1b38470644e004b9356c1622368fa57\","++
       "\"_to\":\"196.209.236.12:34963\"}"

{-
_case_encodeMsg1Js =
  encodeMsg msg1Js @?= "\"{\\\".tap\\\":[{\\\"has\\\":[\\\"+pop\\\"],\\\"is\\\":{\\\"+end\\\":\\\"38666817e1b38470644e004b9356c1622368fa57\\\"}}],\\\"_line\\\":412367436,\\\".see\\\":[\\\"208.68.163.247:42424\\\"],\\\"_br\\\":74,\\\"+end\\\":\\\"38666817e1b38470644e004b9356c1622368fa57\\\",\\\"_to\\\":\\\"196.209.236.12:34963\\\"}\""
  -}

-- --------------------------------------------

-- lineOk tests
-- 1. Timeout - relates to line only
-- 2a. msg.line is set, but no line.ringout
--     * line.ringout is set when setting up a line, impossible situation
-- 2b. line.line == msg.line
-- 2c. msg.line is a multiple of our ring
-- 3a. msg.ring is > 0 and <= 32768
-- 3b. msg.ring == line.ringin, if there is a line.ringin

case_lineOk_msgRingMatchesLine =
  True @=? isRingOk (line1 { lineRingin = Just 15}) (msg1 { teleRing = Just 15})

case_lineOk_msgRingMisMatchesLine =
  False @=? isRingOk (line1 { lineRingin = Just 14}) (msg1 { teleRing = Just 15})

-- ---------------------------------------------------------------------

case_lineOk_msgRingValid1 =
  False @=? isRingOk line1 msg1 { teleRing = Just 0}

case_lineOk_msgRingValid2 =
  True @=? isRingOk line1 msg1 { teleRing = Just 1}

case_lineOk_msgRingValid3 =
  True @=? isRingOk line1 msg1 { teleRing = Just 32768}

case_lineOk_msgRingValid4 =
  False @=? isRingOk line1 msg1 { teleRing = Just 32769}

-- ---------------------------------------------------------------------

case_lineOk_ringMatch =
  Right True @=? isLineOk (line1 {lineLine = Just 12345, lineRingout = 5 }) 1008 (msg1 { teleLine = Just 12345 })

case_lineOk_ringMisMatch =
  Left "msgLineOk=False,ringTimedOut=False" @=? isLineOk (line1 {lineLine = Just 12345, lineRingout = 7 }) 1008 (msg1 { teleLine = Just 12345 })

case_lineOk_lineMatch1 =
  Right True @=? isLineOk (line1 {lineLine = Just 12345 }) 1008 (msg1 { teleLine = Just 12345 })

case_lineOk_lineMatch2 =
  Right True @=?isLineOk (line1 {lineLineat = Nothing, lineRingout=15}) 1003 (msg1 {teleLine = Just 12345})

case_lineOk_lineMisMatch =
  Left "msgLineOk=False,ringTimedOut=False" @=? isLineOk (line1 {lineLine = Just 12345 }) 1008 (msg1 { teleLine = Just 1 })

-- ---------------------------------------------------------------------

case_lineOk_timeoutOk =
  Right True @=? isLineOk line1 1010 msg1

case_lineOk_timeoutFail =
  -- Left "False" @=? isLineOk line1 1011 msg1
  -- Left "msgLineOk=True,timedOut=True" @=? isLineOk line1 1041 msg1
  Left "msgLineOk=True,ringTimedOut=True" @=? isLineOk (line1 {lineLineat = Just (TOD 1000 999), lineLine=Just 30}) 1011 msg1


--line1 = (mkLine (IPP "telehash.org:42424") (TOD 1000 999)) { lineLineat = Just (TOD 1000 999), lineRingout = 5, lineBr = 10 }
--line1 = (mkLine (IPP "telehash.org:42424") (TOD 1000 999) 1234) { lineRingout = 5, lineBr = 10 }
line1 = (mkLine ipp1 (TOD 1000 999) 1234) { lineRingout = 5, lineBr = 10 }
-- msg1 = (mkTelex (IPP "1.2.3.4:567")) { teleMsgLength = Just 100 }
msg1 = (mkTelex ipp1) { teleMsgLength = Just 100, teleBr = 97 }

line2 = (mkLine ipp2 (TOD 1002 999) 2345) { lineRingout = 2345, lineBr = 102 }

-- ---------------------------------------------------------------------
-- checkLine tests, in terms of modifying line state
-- If lineat is not set
--   set lineat, ringin, and line
-- set line br, brin
-- drop line if brin/brout delta > 12k

case_checkLineBrDrop = do
  (Left "lineOk=Right True,ringOk=True,brOk=False")
  @=? (checkLine (line1 {lineLineat = Nothing, lineRingout=15, lineBr = 11904, lineBrout = 3})
                  (msg1 {teleLine = Just 12345, teleRing = Nothing})
                  (TOD 1000 999))

case_checkLineBrNoDrop =
  (Right $ line1 {lineSeenat = Just (TOD 1000 999), lineLineat = Just (TOD 1000 999),
              lineBr = 12003, lineBrin = 97, lineBrout = 3,
              lineRingin = Just 823, lineLine = Just 12345, lineRingout = 15})
  @=? (checkLine (line1 {lineLineat = Nothing, lineRingout=15, lineBr = 11903, lineBrout = 3})
            (msg1 {teleLine = Just 12345, teleRing = Nothing})
            (TOD 1000 999))


case_checkLineRing1 =
  (Right $ line1 {lineSeenat = Just (TOD 1000 999), lineLineat = Just (TOD 1000 999),
              lineBr = 110, lineBrin = 97, lineRingin = Just 823,
              lineLine = Just 12345, lineRingout = 15})
  @=? (checkLine (line1 {lineLineat = Nothing, lineRingout=15})
                (msg1 {teleLine = Just 12345, teleRing = Nothing})
                (TOD 1000 999))

case_checkLineRing2 =
  (Right $ line1 {lineSeenat = Just (TOD 1000 999), lineLineat = Just (TOD 1000 999),
              lineBr = 110, lineBrin = 97, lineRingin = Just 823,
              lineLine = Just 12345, lineRingout = 15})
  @=? (checkLine (line1 {lineLineat = Nothing, lineRingout=15})
            (msg1 {teleLine = Nothing, teleRing = Just 823})
            (TOD 1000 999))

case_checkLineRingInvalid =
  (Left "ringOk=False")
  @=? (checkLine (line1 {lineLineat = Nothing, lineRingout=15})
            (msg1 {teleLine = Nothing, teleRing = Just 32769})
            (TOD 1000 999))

-- -----------------------------------------------

case_checkLine1 =
  (Right $ line1 {lineSeenat = Just (TOD 1000 999), lineBr = 110, lineBrin = 97})
  @=? checkLine line1 msg1 (TOD 1000 999)

case_checkLine2 =
  Left "msgLineOk=True,ringTimedOut=True"
  -- @=? checkLine line1 msg1 (TOD 1020 999)
  -- @=? checkLine line1 msg1 (TOD 1050 999)
  @=? checkLine (line1 {lineLineat = Just (TOD 1000 999), lineLine=Just 30}) msg1 (TOD 1011 999)

-- ---------------------------------------------------------------------

case_getCommands1 =
  [".see",".tap"]
  @=? (getCommands $ fromJust $ parseTelex "{\"_to\":\"208.68.163.247:42424\",\"+end\":\"f507a91f7277fb46e34eebf17a76f0e0351f6269\",\".see\":[\"196.215.40.28:59056\"],\".tap\":[{\"is\":{\"+end\":\"f507a91f7277fb46e34eebf17a76f0e0351f6269\"},\"has\":[\"+pop\"]}],\"_line\":252817576,\"_br\":89}")

case_getSignals1 =
  [("+end","\"f507a91f7277fb46e34eebf17a76f0e0351f6269\"")]
  @=? (getSignals $ fromJust $ parseTelex "{\"_to\":\"208.68.163.247:42424\",\"+end\":\"f507a91f7277fb46e34eebf17a76f0e0351f6269\",\".see\":[\"196.215.40.28:59056\"],\".tap\":[{\"is\":{\"+end\":\"f507a91f7277fb46e34eebf17a76f0e0351f6269\"},\"has\":[\"+pop\"]}],\"_line\":252817576,\"_br\":89}")


-- ----------------------------------------------------------------------

case_DistanceTo1 =
  -1 @=? distanceTo (mkHash (IPP "208.68.163.247:42424")) (mkHash (IPP "208.68.163.247:42424"))

case_DistanceTo2 =
  158 @=? distanceTo (mkHash (IPP "208.68.163.247:42424")) (mkHash (IPP "208.68.160.25:32771"))

-- ---------------------------------------------------------------------

case_EncodeTelex1 =
  "{\"_to\":\"1.2.3.4:1234\",\"_br\":97}" @=? encodeTelex msg1

case_EncodeTelex2 =
  "{\"_to\":\"1.2.3.4:1234\"," ++
  "\"+end\":\"255b5a502b0a348883ffa757e0c1ea812a128088\"," ++
  "\"_ring\":\"13\"," ++
  "\".see\":[\"2.3.4.5:2345\",\"3.4.5.6:3456\"]," ++
  "\".tap\":[{\"is\":{\".end\":\"foo\",\"has\":[\"+pop\"]}}]," ++
  "\"_line\":\"4567\"," ++
  "\"_br\":234," ++
  "\"_hop\":3," ++
  "\"+pop\":\"pop_val\"}"
  @=?
  (encodeTelex $ ((mkTelex ipp1) { teleRing = Just 13
                               , teleSee = Just [ipp2,ipp3]
                               , teleBr = 234
                               , teleLine = Just 4567
                               , teleHop = Just 3
                               , teleSigEnd = Just hash1
                               , teleSigPop = Just "pop_val"
                               , teleTap = Just [Tap {tapIs = (".end","foo"), tapHas = ["+pop"]}]
                               }))
-- ---------------------------------------------------------------------

--addr :: AddrInfo
--addr = AddrInfo [] AF_INET Datagram 6 (SockAddrInet (PortNum 1234) 1234) Nothing

case_resolveToSeedIpp :: Assertion
case_resolveToSeedIpp = do
  -- (a,b,c) <- (resolveToSeedIPP "telehash.org:42424")
  (a,b,c) <- (resolveToSeedIPP "localhost:42424")

  case (a,b,c) of
    -- (_,"208.68.163.247","42424") -> return ()
    (_,"127.0.0.1","42424") -> return ()
    (x,y,z)        -> assertFailure ("case_resolveToSeedIpp:" ++ (show (x,y,z)))

case_addrFromHostPort :: Assertion
case_addrFromHostPort = do
  addr <- addrFromHostPort "1.2.3.4" "42424"
  case addr of
    (SockAddrInet 42424 0x04030201) -> return ()
    (SockAddrInet p h) -> assertFailure ("case_addrFromHostPort:" ++ (show (p,h)))

-- ---------------------------------------------------------------------

-- If the line is not already known, insert it, making it a neighbour to itself. Else return it.
case_getOrCreateLine_firstTime = do
  (line,state) <- runStateT (getOrCreateLine ipp1 (TOD 1000 9999)) st1

  let t1 = TOD 1000 9999

  if (lineIpp line /= ipp1)
    then (assertFailure $ "ipp wrong, got " ++ (show (lineIpp line)))
    else return ()

  if (lineEnd line /= hash1)
    then (assertFailure $ "end wrong, got " ++ (show (lineEnd line)))
    else return ()

  let (TOD s1 ps1) = (lineInit line)
  if (lineInit line /= (TOD 1000 9999))
    then (assertFailure $ "lineInit wrong, got " ++ (show (s1,ps1)))
    else return ()

  if ((Set.toList (lineNeighbors line)) /= [hash1])
    then (assertFailure $ "neighbours wrong, got " ++ (show (lineNeighbors line)))
    else return ()

  if (Map.size (swMaster state) /= 1)
    then (assertFailure $ "state swMaster size wrong, got " ++ (show (Map.size (swMaster state))))
    else return ()

  if (Map.notMember hash1 (swMaster state))
    then (assertFailure $ "line notMember state swMaster")
    else return ()

  if ((swMaster state) Map.! hash1 /= line)
    then (assertFailure $ "returned line /= swMaster line")
    else return ()

-- ---------------------------------------------------------------------

case_getOrCreateLine_alreadyThere = do
  let st = st1 {swMaster = Map.fromList
                           [
                             (hash1,
                              (mkLine ipp1 (TOD 1000 1234) 1234) {lineNeighbors = Set.fromList [hash1]})
                             ]}

  let t1 = TOD 1000 9999

  (line,state) <- runStateT (getOrCreateLine ipp1 t1) st

  if (lineIpp line /= ipp1)
    then (assertFailure $ "ipp wrong, got " ++ (show (lineIpp line)))
    else return ()

  let (TOD s1 ps1) = (lineInit line)
  if (lineInit line /= (TOD 1000 1234))
    then (assertFailure $ "lineInit wrong, got " ++ (show (s1,ps1)))
    else return ()

  if ((Set.toList (lineNeighbors line)) /= [hash1])
    then (assertFailure $ "neighbours wrong, got " ++ (show (lineNeighbors line)))
    else return ()

  if (lineEnd line /= hash1)
    then (assertFailure $ "end wrong, got " ++ (show (lineEnd line)))
    else return ()

  if (Map.size (swMaster state) /= 1)
    then (assertFailure $ "state swMaster size wrong, got " ++ (show (Map.size (swMaster state))))
    else return ()

  if (Map.notMember hash1 (swMaster state))
    then (assertFailure $ "line notMember state swMaster")
    else return ()

-- ---------------------------------------------------------------------

case_online = do
  (line,state) <- runStateT (online msg1 (TOD 1000 9999)) st1

  if (swConnected state == True)
    then return ()
    else (assertFailure $ "online:swConnected fail" ++ (show $ (line,state)))

  if (swSelfIpp state == Just (IPP "1.2.3.4:1234"))
    then return ()
    else (assertFailure $ "online:swSelfIpp fail" ++ (show $ (line,state)))

  if (swSelfHash state == Just (mkHash (IPP "1.2.3.4:1234")))
    then return ()
    else (assertFailure $ "online:swSelfHash fail" ++ (show $ (line,state)))

  if (Map.size (swMaster state) == 1)
    then return ()
    else (assertFailure $ "online:swMaster size fail" ++ (show $ (line,state)))

-- ---------------------------------------------------------------------

case_prepareTelex_bSentOk = do
  let
    st = st1 { swMaster =
                  Map.fromList [
                    (hash1,
                     (mkLine ipp1 (TOD 1000 999) 1234) {lineBsent = 10000, lineBrin = 100 })
                    ]}

  (res,state) <- runStateT (prepareTelex msg1 (TOD 1000 9999)) st

  if (res /= Nothing)
    then return ()
    else (assertFailure $ "prepareTelex:" ++ (show $ res))

-- ---------------------------------------------------------------------

case_prepareTelex_bSentFail = do
  let
    st = st1 { swMaster =
                  Map.fromList [
                    (hash1,
                     (mkLine ipp1 (TOD 1000 999) 1234) {lineBsent = 10101, lineBrin = 100 })
                    ]}

  (res,state) <- runStateT (prepareTelex msg1 (TOD 1000 9999)) st

  if (res == Nothing)
    then return ()
    else (assertFailure $ "prepareTelex:" ++ (show $ (res,state)))

-- ---------------------------------------------------------------------

case_prepareTelex_ringout = do
  (res,state) <- runStateT (prepareTelex msg1 (TOD 1000 9999)) st1

  let
    Just (line,msgJson) = res
    Just msg = parseTelex msgJson

  if (teleLine msg == Nothing)
    then return ()
    else (assertFailure $ "prepareTelex:line set " ++ (show $ msg))

  if (teleRing msg /= Nothing)
    then return ()
    else (assertFailure $ "prepareTelex:ring not set " ++ (show $ (msgJson,msg)))

-- ---------------------------------------------------------------------

case_prepareTelex_line = do
  let
    st = st1 { swMaster =
                  Map.fromList [
                    (hash1,
                     (mkLine ipp1 (TOD 1000 999) 1234 ) {lineLine = Just 123456})
                    ]}

  (res,state) <- runStateT (prepareTelex msg1 (TOD 1000 9999)) st

  let
    Just (line,msgJson) = res
    Just msg = parseTelex msgJson

  if (teleLine msg /= Nothing)
    then return ()
    else (assertFailure $ "prepareTelex:line set " ++ (show $ msg))

  if (teleRing msg == Nothing)
    then return ()
    else (assertFailure $ "prepareTelex:ring not set " ++ (show $ (msgJson,msg)))

-- ---------------------------------------------------------------------

case_prepareTelex_counters = do
  let
    st = st1 { swMaster =
                  Map.fromList [
                    (hash1,
                     (mkLine ipp1 (TOD 1000 999) 1234) {lineBr = 100, lineBrout = 10, lineBsent = 120})
                    ]}

  (res,state) <- runStateT (prepareTelex msg1 (TOD 10003 9999)) st

  let
    Just (line,msgJson) = res
    msg = parseTelex msgJson

  if (lineBr line == 100)
    then return ()
    else (assertFailure $ "prepareTelex:lineBr " ++ (show $ line))

  if (lineBrout line == 100)
    then return ()
    else (assertFailure $ "prepareTelex:lineBrout " ++ (show $ line))

  if (lineBsent line == 120 + (length msgJson))
    then return ()
    else (assertFailure $ "prepareTelex:lineBsent " ++ (show $ line))

  if (lineSentat line == Just (TOD 10003 9999))
    then return ()
    else (assertFailure $ "prepareTelex:lineSentat " ++ (show $ line))

-- ---------------------------------------------------------------------

prop_idempotent :: [Int] -> Bool
prop_idempotent xs = sort (sort xs) == sort xs

-- ---------------------------------------------------------------------

prop_parseTelexJson =
  forAll telehashJson $ \msgJson ->
  Nothing /= parseTelex msgJson

-- ---------------------------------------------------------------------

-- Trying to write an arbitrary generator of valid Telehash javascript
-- strings.
-- See http://book.realworldhaskell.org/read/testing-and-quality-assurance.html
-- and http://jasani.org/2008/01/03/testing-haskell-with-quickcheck/


telehashJson :: Gen String
telehashJson = do
  name <- identifier
  i <- ipp
  br <- choose (0,1000000) :: Gen Int
  let toClause = ("\"_to\":\"" ++ (unIPP i) ++ "\"")
  let brClause = ("\"_br\":" ++ (show br))
  return ("{" ++ toClause ++ "," ++ brClause ++ "}")

ipp :: Gen IPP
ipp = do
  port <- choose (1024,65535) :: Gen Int
  quads <- vectorOf 4 (choose (1,255) :: Gen Int)
  let ip = intercalate "." $ map show quads
  return (IPP (ip ++ ":" ++ (show port)))

-- Library functions
iden0 :: Gen Char
iden0 = oneof [ elements ['a'..'z'], elements ['A'..'Z'], elements ['0'..'9'] ]

idenN :: Gen String
idenN = listOf iden0

opt :: Gen String -> Gen String
opt g = oneof [ g, return "" ]

identifier :: Gen String
--identifier = iden0 >>= \i0 -> idenN >>= return . (i0:)
identifier = do
  -- note: could use listOf1, instead.
  firstChar <- iden0
  rest <- idenN
  return (firstChar:rest)

s = sample identifier

-- Need a newtype, else get complaints about arbitrary instance already existing
newtype GenJsonTelex = GJT String deriving Show
unGJT (GJT str) = str

instance Arbitrary GenJsonTelex where
  arbitrary = do
    str <- elements (['A'..'Z'] ++ ['a' .. 'z'] ++ " ~!@#$%^&*()")
    return (GJT [str])

samples :: IO [GenJsonTelex]
samples = sample' arbitrary :: IO [GenJsonTelex]

samples' = do
  s <- samples
  return (mapM unGJT s)

-- ---------------------------------------------------------------------

case_removeLineM = do
  if (Map.size (swMaster st2) == 3)
    then return ()
    else (assertFailure $ "removeLineM:starting with 3 lines " ++ (show $ (swMaster st2)))

  (line,state) <- runStateT (removeLineM hash2) st2

  if (Map.size (swMaster state) == 2)
    then return ()
    else (assertFailure $ "removeLineM:expected 2 lines left" ++ (show $ (swMaster state)))

  if (Map.keys (swMaster state) == sort [hash1,hash3])
    then return ()
    else (assertFailure $ "removeLineM:wrong lines left" ++ (show $ (swMaster state)))

-- ---------------------------------------------------------------------
{-
See process.

Receive a telex from a remote switch with a list of ipps in it, that
can be seen from the remote ipp

1. Do not process an IPP of ourselves

2. Only process a see once, tracked in the lineVisible flag

3. For the self-see, ie. line making iteslf visible
   TODO:*** still to test*** set the line neighbours to be the near_to list of the see list.
   [Why near_to, why not just the list?]

4. If we already have a record of the line, do not process it

5. If we bucket_want the ipp, send it a direct message to it and a
   +pop to the originator to open it up in both directions

-}

case_processSee_Self = do
  let
    remoteipp = ipp2
    seeipp    = ipp1
    line1 = (mkLine ipp1 (TOD 1000 999) 1234) {lineNeighbors = Set.fromList [hash1], lineVisible = True}
    line2 = (mkLine ipp2 (TOD 1000 999) 2345) {lineNeighbors = Set.fromList [hash2], lineVisible = False}

    st = st1 { swSelfIpp  = Just ipp1,
               swSelfHash = Just hash1,
               swMaster   = Map.fromList [(hash1,line1),(hash2,line2)] }

    telex = (mkTelex ipp1) { teleSee = Just [ipp1] }

  (line,state) <- runStateT (processCommand ".see" remoteipp telex line2) st

  if (state == st)
     then return ()
     else (assertFailure $ "processSee:" ++ (show $ (swMaster state)))

-- ---------------------------------------------------------------------

case_processSee_AlreadyVisible = do
  let
    remoteipp = ipp1
    seeipp    = ipp1
    line1 = (mkLine ipp1 (TOD 1000 999) 1234) {lineNeighbors = Set.fromList [hash1], lineVisible = True}
    line2 = (mkLine ipp2 (TOD 1000 999) 2345) {lineNeighbors = Set.fromList [hash2], lineVisible = True}

    st = st1 { swSelfIpp  = Just ipp1,
               swSelfHash = Just hash1,
               swMaster   = Map.fromList [(hash1,line1),(hash2,line2)] }

    telex = (mkTelex ipp1) { teleSee = Just [ipp1] }

  (line,state) <- runStateT (processCommand ".see" remoteipp telex line2) st

  if (state == st)
     then return ()
     else (assertFailure $ "processSee:" ++ (show $ (swMaster state)))

-- ---------------------------------------------------------------------

case_processSee_NotVisible = do
  let
    remoteipp = ipp2
    seeipp    = ipp1
    line1 = (mkLine ipp1 (TOD 1000 999) 1234) {lineNeighbors = Set.fromList [hash1], lineVisible = True}
    line2 = (mkLine ipp2 (TOD 1000 999) 2345) {lineNeighbors = Set.fromList [hash2], lineVisible = False}

    st = st1 { swSelfIpp  = Just ipp1,
               swSelfHash = Just hash1,
               swMaster   = Map.fromList [(hash1,line1),(hash2,line2)] }

    telex = (mkTelex ipp1) { teleSee = Just [ipp2] }

  (line,state) <- runStateT (processCommand ".see" remoteipp telex line2) st

  let line2' = (swMaster state) Map.! hash2
  if (lineVisible line2')
     then return ()
     else (assertFailure $ "processSee:" ++ (show $ (swMaster state)))

-- ---------------------------------------------------------------------

case_processSee_LineKnown = do
  let
    remoteipp = ipp2
    seeipp    = ipp1
    line1 = (mkLine ipp1 (TOD 1000 999) 1234) {lineNeighbors = Set.fromList [hash1], lineVisible = True}
    line2 = (mkLine ipp2 (TOD 1000 999) 2345) {lineNeighbors = Set.fromList [hash2], lineVisible = False}
    line3 = (mkLine ipp3 (TOD 1000 999) 3456) {lineNeighbors = Set.fromList [hash3], lineVisible = False}

    st = st1 { swSelfIpp  = Just ipp1,
               swSelfHash = Just hash1,
               swMaster   = Map.fromList [(hash1,line1),(hash2,line2),(hash3,line3)] }

    telex = (mkTelex ipp1) { teleSee = Just [ipp3] }

  (line,state) <- runStateT (processCommand ".see" remoteipp telex line2) st

  if (state == st)
     then return ()
     else (assertFailure $ "processSee:" ++ (show $ (swMaster state)))

-- ---------------------------------------------------------------------

case_processSee_LineUnKnown = do
  let
    remoteipp = ipp2
    seeipp    = ipp1
    line1 = (mkLine ipp1 (TOD 1000 999) 1234) {lineNeighbors = Set.fromList [hash1], lineVisible = True}
    line2 = (mkLine ipp2 (TOD 1000 999) 2345) {lineNeighbors = Set.fromList [hash2], lineVisible = False}

    st = st1 { swSelfIpp  = Just ipp1,
               swSelfHash = Just hash1,
               swMaster   = Map.fromList [(hash1,line1),(hash2,line2)] }

    telex = (mkTelex ipp1) { teleSee = Just [ipp3] }

  (line,state) <- runStateT (processCommand ".see" remoteipp telex line2) st

  if (Map.member hash3 (swMaster state))
     then return ()
     else (assertFailure $ "processSee:" ++ (show $ (swMaster state)))

-- ---------------------------------------------------------------------

case_processSee_sendTelexes = do
  let
    remoteipp = ipp2
    line1 = (mkLine ipp1 (TOD 1000 999) 1234) {lineNeighbors = Set.fromList [hash1], lineVisible = True}
    line2 = (mkLine ipp2 (TOD 1000 999) 2345) {lineNeighbors = Set.fromList [hash2], lineVisible = False}

    st = st1 { swSelfIpp  = Just ipp1,
               swSelfHash = Just hash1,
               swMaster   = Map.fromList [(hash1,line1),(hash2,line2)] }

    telex = (mkTelex ipp1) { teleSee = Just [ipp3] }

  knob <- setupLogger

  R.setStdGen (R.mkStdGen 1)
  (line,state) <- runStateT (processCommand ".see" remoteipp telex line2) st

  l <- retrieveLog knob
  let
    [m1,m2] = filter (\s -> "SEND" == take 4 s) $ lines l

    em1 = ("SEND[:IPP \"3.4.5.6:3456\"]\t{\"_to\":\"3.4.5.6:3456\"," ++
          "\"+end\":\"255b5a502b0a348883ffa757e0c1ea812a128088\"," ++
          "\"_ring\":\"6482\",\"_br\":0}")

    em2 = ("SEND[:IPP \"2.3.4.5:2345\"]\t{\"_to\":\"2.3.4.5:2345\"," ++
          "\"+end\":\"0097bc897d6c2d5b5a06c94d40528c66859941c8\"," ++
          "\"_ring\":\"2345\",\"_br\":0," ++
          "\"_hop\":1,\"+pop\":\"th:1.2.3.4:1234\"}")

  if (m1 == em1 && m2 == em2)
  --if (False)
     then return ()
     else (assertFailure $ "processSee:" ++ (show $ [m1,m2]))

-- ---------------------------------------------------------------------
{-
Tests

0. Only scan if we are online
1. Do not process the line if it is us
2. Purge if more than 70 secs have passed since ring, and no line yet
3. Purge if line init, but no activity for more than 70 secs

The next three are all in a single message
4. If 1,2, & 3 pass, send our "+end" to the line
5. If 1,2, & 3 pass, send a ".see" to the line, but only the first
   time (isVisibled is controlling var)
6. If 1,2, & 3 pass, send a .tap request to +pop our end

7. If no line passes 1,2, & 3, and we are not in the sed list, go offline

-}

case_ScanLines_LineGoodWithSee = do
  let
    line2 = (mkLine ipp2 (TOD 1000 999) 4567) {lineSeenat = Just (TOD 2000 999),
                                               lineVisibled = False}

    st = st1 {swConnected = True, swSelfIpp = Just ipp1, swSelfHash = Just hash1,
              swMaster = Map.fromList [(hash1,line1),(hash2,line2)]
              }

  knob <- setupLogger

  (_,state) <- runStateT (scanlines (TOD 2070 999)) st

  l <- retrieveLog knob

  if ((take 2 $ (lines l)) ==
      ["SCAN\t2",
       "SEND[:IPP \"2.3.4.5:2345\"]\t{\"_to\":\"2.3.4.5:2345\"," ++
       "\"+end\":\"255b5a502b0a348883ffa757e0c1ea812a128088\",\"_ring\":\"4567\"," ++
       "\".see\":[\"1.2.3.4:1234\"]," ++
       "\".tap\":[{\"is\":{\"+end\":\"255b5a502b0a348883ffa757e0c1ea812a128088\",\"has\":[\"+pop\"]}}]," ++
       "\"_br\":0}"])
     then return ()
     else (assertFailure $ "scanlines:" ++ (show (lines l,state)))

-- ---------------------------------------------------------------------

case_ScanLines_LineGoodNoSee = do
  let
    line2 = (mkLine ipp2 (TOD 1000 999) 4567) {lineSeenat = Just (TOD 2000 999),
                                               lineVisibled = True}

    st = st1 {swConnected = True, swSelfIpp = Just ipp1, swSelfHash = Just hash1,
              swMaster = Map.fromList [(hash1,line1),(hash2,line2)]
              }

  knob <- setupLogger

  (_,state) <- runStateT (scanlines (TOD 2070 999)) st

  l <- retrieveLog knob

  if ((take 2 $ (lines l)) ==
      ["SCAN\t2",
       "SEND[:IPP \"2.3.4.5:2345\"]\t{\"_to\":\"2.3.4.5:2345\"," ++
       "\"+end\":\"255b5a502b0a348883ffa757e0c1ea812a128088\",\"_ring\":\"4567\"," ++
       "\".tap\":[{\"is\":{\"+end\":\"255b5a502b0a348883ffa757e0c1ea812a128088\",\"has\":[\"+pop\"]}}]," ++
       "\"_br\":0}"])
     then return ()
     else (assertFailure $ "scanlines:" ++ (show (lines l,state)))

-- ---------------------------------------------------------------------

case_ScanLines_PurgeStaleRing = do
  let
    line2 = (mkLine ipp2 (TOD 1000 999) 4567)

    st = st1 {swConnected = True, swSelfIpp = Just ipp1, swSelfHash = Just hash1,
              swMaster = Map.fromList [(hash1,line1),(hash2,line2)]
              }

  knob <- setupLogger

  (_,state) <- runStateT (scanlines (TOD 1071 999)) st

  l <- retrieveLog knob

  if ((lines l) == ["SCAN\t2",
                    "\tPURGE[Hash \"0ec331643f4a7a74068ea47dda062b48b419c832\" IPP \"2.3.4.5:2345\"] last seen Nothing",
                    "OFFLINE at Thu Jan  1 02:17:51 SAST 1970"])
     then return ()
     else (assertFailure $ "scanlines:" ++ (show (lines l,state)))

case_ScanLines_PurgeStaleLine = do
  let
    line2 = (mkLine ipp2 (TOD 1000 999) 4567) {lineSeenat = Just (TOD 2000 999)}

    st = st1 {swConnected = True, swSelfIpp = Just ipp1, swSelfHash = Just hash1,
              swMaster = Map.fromList [(hash1,line1),(hash2,line2)]
              }

  knob <- setupLogger

  (_,state) <- runStateT (scanlines (TOD 2071 999)) st

  l <- retrieveLog knob

  if ((lines l) == ["SCAN\t2",
                    "\tPURGE[Hash \"0ec331643f4a7a74068ea47dda062b48b419c832\" IPP \"2.3.4.5:2345\"]" ++
                    " last seen Just Thu Jan  1 02:33:20 SAST 1970",
                    "OFFLINE at Thu Jan  1 02:34:31 SAST 1970"])
     then return ()
     else (assertFailure $ "scanlines:" ++ (show (lines l,state)))

-- ---------------------------------------------------------------------

case_ScanLines_Us = do
  let

    st = st1 {swConnected = True, swSelfIpp = Just ipp1, swSelfHash = Just hash1,
              swMaster = Map.fromList [(hash1,line1)]
              }

  knob <- setupLogger

  (_,state) <- runStateT (scanlines (TOD 1000 999)) st

  l <- retrieveLog knob

  if (lines l == ["SCAN\t1","OFFLINE at Thu Jan  1 02:16:40 SAST 1970"])
     then return ()
     else (assertFailure $ "scanlines:" ++ (show (lines l,state)))

-- ---------------------------------------------------------------------

case_ScanLines_Offline = do
  let
    st = st1 {swConnected = False}

  knob <- setupLogger

  (_,state) <- runStateT (scanlines (TOD 1000 999)) st

  l <- retrieveLog knob

  if (l == "")
     then return ()
     else (assertFailure $ "scanlines:" ++ (show (l,state)))

case_ScanLines_Online = do
  let
    st = st1 {swConnected = True, swSelfIpp = Just ipp1, swSelfHash = Just hash1}

  knob <- setupLogger

  (_,state) <- runStateT (scanlines (TOD 1000 999)) st

  l <- retrieveLog knob

  if ((head $ lines l) == "SCAN\t0")
     then return ()
     else (assertFailure $ "scanlines:" ++ (show (lines l,state)))

-- ---------------------------------------------------------------------

setupLogger :: IO Knob
setupLogger = do
  knob <- newKnob (BC.pack [])
  h <- newFileHandle knob "testlog.txt" WriteMode
  s <- streamHandler h DEBUG
  updateGlobalLogger rootLoggerName (setHandlers [s])
  return knob

retrieveLog :: MonadIO m => Knob -> m [Char]
retrieveLog knob = do
  bytes <- Data.Knob.getContents knob
  return (BC.unpack bytes)

-- ---------------------------------------------------------------------

case_rotateToNextSeed = do
  let
    st = st1 { swSeeds = ["a","b","c"] }

  (s1,st1) <- runStateT (rotateToNextSeed) st
  if (s1 == "a")
     then return ()
     else (assertFailure $ "rotateToNextSeed:s1" ++ (show (s1,st1)))

  (s2,st2) <- runStateT (rotateToNextSeed) st1
  if (s2 == "b")
     then return ()
     else (assertFailure $ "rotateToNextSeed:s2" ++ (show (s2,st2)))

  (s3,st3) <- runStateT (rotateToNextSeed) st2
  if (s3 == "c")
     then return ()
     else (assertFailure $ "rotateToNextSeed:s3" ++ (show (s3,st3)))

  (s4,st4) <- runStateT (rotateToNextSeed) st3
  if (s4 == "a")
     then return ()
     else (assertFailure $ "rotateToNextSeed:s4" ++ (show (s4,st4)))

-- ---------------------------------------------------------------------
{-
Testing tapsignals on received messages

1. Only process them if the message hop < 4
2. Only process for lines with rules set
3. A rule matchs if both
   a. All the "is" keys are in this telex, and match exactly
   b. All the "has" keys are present in the telex
4. If at least one rule matches
   a. If the line holding the rule is us, do nothing
   b. Else send a telex to the ipp of the line, having all the signals
      (start with +) from the original telex
      and hop' = hop+1
-}

case_TapSignals_RulesMatches_b_us = do
  let
    --msg = msg1 { teleHop = Just 3, teleSigEnd = Just (Hash "38666817e1b38470644e004b9356c1622368fa57") }
    Just msg = parseTelex ("{" ++
                           "\"_line\":412367436,"++
                           "\".see\":[\"208.68.163.247:42424\"],"++
                           "\"_br\":74,"++
                           "\"+end\":\"38666817e1b38470644e004b9356c1622368fa57\","++
                           "\"+pop\":\"th:196.215.128.240:51602\"," ++ -- New addition
                           "\"_to\":\"196.209.236.12:34963\""++
                           "}")

    st = st1 { swSelfIpp = Just ipp1,
               swMaster =
                  Map.fromList
                  [(hash2,line2),
                   (hash1,line1 {lineRules =
                                    [Tap {tapIs = ("+end","38666817e1b38470644e004b9356c1622368fa57"),
                                          tapHas = ["+pop"]}]})
                  ],
               swSender = doNullSendDgram
               }

  knob <- setupLogger

  (_,state) <- runStateT (tapSignals True msg) st

  l <- retrieveLog knob

  let e = ["\tTAP CHECK IS IPP \"1.2.3.4:1234\"\tTap {"++
             "tapIs = (\"+end\",\"38666817e1b38470644e004b9356c1622368fa57\"), "++
             "tapHas = [\"+pop\"]}",
           "\t\tIS match: Just (\"+end\",\"\\\"38666817e1b38470644e004b9356c1622368fa57\\\"\")"++
                                           "=\"38666817e1b38470644e004b9356c1622368fa57\"?",

           "\t\tHAS match: +pop -> True",
           "\tTAP CHECK:( (True,True)",

           "STDOUT[Telex {teleRing = Nothing, teleSee = Just [IPP \"208.68.163.247:42424\"], "++
             "teleBr = 74, teleTo = IPP \"196.209.236.12:34963\", "++
             "teleLine = Just 412367436, teleHop = Nothing, "++
             "teleSigEnd = Just (Hash \"38666817e1b38470644e004b9356c1622368fa57\"), "++
             "teleSigPop = Just \"th:196.215.128.240:51602\", teleTap = Nothing, "++
             "teleRest = fromList [(\"+end\",\"\\\"38666817e1b38470644e004b9356c1622368fa57\\\"\"),"++
             "(\"+pop\",\"\\\"th:196.215.128.240:51602\\\"\"),"++
             "(\".see\",\"[\\\"208.68.163.247:42424\\\"]\"),(\"_br\",\"74\"),"++
             "(\"_line\",\"412367436\"),(\"_to\",\"\\\"196.209.236.12:34963\\\"\")], "++
             "teleMsgLength = Just 173}]"]

  if (lines l == e)
     then return ()
     --then (assertFailure $ ":" ++ (show (lines l,msg,state)))
     else (assertFailure $ ":" ++ (show (lines l,msg,state)))

-- ---------------------------------------------------------------------

case_TapSignals_RulesMatches_b = do
  let
    --msg = msg1 { teleHop = Just 3, teleSigEnd = Just (Hash "38666817e1b38470644e004b9356c1622368fa57") }
    Just msg = parseTelex ("{" ++
                           "\"_line\":412367436,"++
                           "\".see\":[\"208.68.163.247:42424\"],"++
                           "\"_br\":74,"++
                           "\"+end\":\"38666817e1b38470644e004b9356c1622368fa57\","++
                           "\"+pop\":\"th:196.215.128.240:51602\"," ++ -- New addition
                           "\"_to\":\"196.209.236.12:34963\""++
                           "}")

    st = st1 { swSelfIpp = Just ipp1,
               swMaster =
                  Map.fromList
                  [(hash1,line1),
                   (hash2,line2 {lineRules =
                                    [Tap {tapIs = ("+end","38666817e1b38470644e004b9356c1622368fa57"),
                                          tapHas = ["+pop"]}]})
                  ],
               swSender = doNullSendDgram
               }

  knob <- setupLogger

  (_,state) <- runStateT (tapSignals True msg) st

  l <- retrieveLog knob

  let e = ["\tTAP CHECK IS IPP \"2.3.4.5:2345\"\tTap {"++
             "tapIs = (\"+end\",\"38666817e1b38470644e004b9356c1622368fa57\"), "++
             "tapHas = [\"+pop\"]}",
           "\t\tIS match: Just (\"+end\",\"\\\"38666817e1b38470644e004b9356c1622368fa57\\\"\")"++
                                           "=\"38666817e1b38470644e004b9356c1622368fa57\"?",

           "\t\tHAS match: +pop -> True",
           "\tTAP CHECK:( (True,True)",
           "SEND[:IPP \"2.3.4.5:2345\"]\t{\"_to\":\"2.3.4.5:2345\","++
             "\"+end\":\"38666817e1b38470644e004b9356c1622368fa57\","++
             "\"_ring\":\"2345\",\"_br\":102,\"_hop\":1,"++
             "\"+pop\":\"th:196.215.128.240:51602\"}",
           "doNullSendDgram"]



  if (lines l == e)
     then return ()
     --then (assertFailure $ ":" ++ (show (lines l,msg,state)))
     else (assertFailure $ ":" ++ (show (lines l,msg,state)))

-- ---------------------------------------------------------------------

case_TapSignals_RulesMatches_a = do
  let
    --msg = msg1 { teleHop = Just 3, teleSigEnd = Just (Hash "38666817e1b38470644e004b9356c1622368fa57") }
    Just msg = parseTelex ("{" ++
                           "\"_line\":412367436,"++
                           "\".see\":[\"208.68.163.247:42424\"],"++
                           "\"_br\":74,"++
                           "\"+end\":\"38666817e1b38470644e004b9356c1622368fa57\","++
                           "\"_to\":\"196.209.236.12:34963\""++
                           "}")

    st = st1 { swMaster =
                  Map.fromList
                  [(hash1,line1),
                   (hash2,line2 {lineRules =
                                    [Tap {tapIs = ("+end","38666817e1b38470644e004b9356c1622368fa57"),
                                          tapHas = ["+pop"]}]})
                  ] }

  knob <- setupLogger

  (_,state) <- runStateT (tapSignals True msg) st

  l <- retrieveLog knob

  let e = ["\tTAP CHECK IS IPP \"2.3.4.5:2345\"\tTap {"++
             "tapIs = (\"+end\",\"38666817e1b38470644e004b9356c1622368fa57\"), "++
             "tapHas = [\"+pop\"]}",
           "\t\tIS match: Just (\"+end\",\"\\\"38666817e1b38470644e004b9356c1622368fa57\\\"\")"++
                                           "=\"38666817e1b38470644e004b9356c1622368fa57\"?",
           "\t\tHAS match: +pop -> False",
           "\tTAP CHECK:( (True,False)",
           "\tCHECK MISS"]

  if (lines l == e)
     then return ()
     --then (assertFailure $ ":" ++ (show (lines l,msg,state)))
     else (assertFailure $ ":" ++ (show (lines l,msg,state)))

-- ---------------------------------------------------------------------

case_TapSignals_LineWithRulesOnly = do
  let
    msg = msg1 { teleHop = Just 3 }

    st = st1 { swMaster =
                  Map.fromList
                  [(hash1,line1),
                   (hash2,line2 {lineRules =
                                    [Tap {tapIs = ("+end","38666817e1b38470644e004b9356c1622368fa57"),
                                          tapHas = ["+pop"]}]})
                  ] }

  knob <- setupLogger

  (_,state) <- runStateT (tapSignals True msg) st

  l <- retrieveLog knob

  let e = ["\tTAP CHECK IS IPP \"2.3.4.5:2345\"\tTap {"++
           "tapIs = (\"+end\",\"38666817e1b38470644e004b9356c1622368fa57\"), "++
           "tapHas = [\"+pop\"]}",
           "\t\tIS match: Nothing=\"38666817e1b38470644e004b9356c1622368fa57\"?",
           "\t\tHAS match: +pop -> False",
           "\tTAP CHECK:( (False,False)",
           "\tCHECK MISS"]

  if (lines l == e)
     then return ()
     else (assertFailure $ "scanlines:" ++ (show (lines l,state)))

-- ---------------------------------------------------------------------

case_TapSignals_BigHop = do
  let
    msg = msg1 { teleHop = Just 4 }
  knob <- setupLogger

  (_,state) <- runStateT (tapSignals True msg) st1

  l <- retrieveLog knob

  if (lines l == [])
     then return ()
     else (assertFailure $ "scanlines:" ++ (show (lines l,state)))

-- ---------------------------------------------------------------------

-- EOF
