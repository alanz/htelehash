import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Control.Monad.State
import Data.List
import System.Time
import TeleHash.Controller
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = defaultMain tests

tests = [ testGroup "group 1" 
          [
            testCase "lineOk_timeoutOk"     test_lineOk_timeoutOk
          , testCase "lineOk_timeoutFail"   test_lineOk_timeoutFail
          , testCase "lineOk_lineMatch1"    test_lineOk_lineMatch1
          , testCase "lineOk_lineMatch2"    test_lineOk_lineMatch2
          , testCase "lineOk_lineMisMatch"  test_lineOk_lineMisMatch
          , testCase "lineOk_ringMatch"     test_lineOk_ringMatch
          , testCase "lineOk_ringMisMatch"  test_lineOk_ringMisMatch
            
          , testCase "lineOk_msgRingValid1" test_lineOk_msgRingValid1
          , testCase "lineOk_msgRingValid2" test_lineOk_msgRingValid2
          , testCase "lineOk_msgRingValid3" test_lineOk_msgRingValid3
          , testCase "lineOk_msgRingValid4" test_lineOk_msgRingValid4
              
          , testCase "lineOk_msgRingMatchesLine"    test_lineOk_msgRingMatchesLine
          , testCase "lineOk_msgRingMisMatchesLine" test_lineOk_msgRingMisMatchesLine
            
          , testCase "test_checkLine1" test_checkLine1
          , testCase "test_checkLine2" test_checkLine2
            
          , testCase "test_checkLineRing1" test_checkLineRing1  
          , testCase "test_checkLineRing2" test_checkLineRing2  
            
          , testCase "test_checkLineBrDrop" test_checkLineBrDrop  
          , testCase "test_checkLineBrDrop" test_checkLineBrNoDrop  
            
          , testCase "test_getCommands1" test_getCommands1  
          , testCase "test_getSignals1"  test_getSignals1  
          
          , testCase "test_DistanceTo1" test_DistanceTo1
          , testCase "test_DistanceTo2" test_DistanceTo2
          ],
          testGroup "recvTelex" [
            -- testCase "basic_rx" test_recvTelex1
            testCase "parseMsg1Js" test_parseMsg1Js
            -- testCase "parseMsg2" test_parseMsg2
            , testCase "encodeMsg1Js1" test_encodeMsg1Js
            ],
          testGroup "near_to" [
            testCase "test_near_to1" test_near_to1
            , testCase "test_near_to2" test_near_to2
            , testCase "test_near_to3" test_near_to3
          ],
          testGroup "seeVisible" [
            testCase "test_seeVisible1" test_seeVisible1
            , testCase "test_seeVisible2" test_seeVisible2
            ]
        ]
  
-- ---------------------------------------------------------------------

test_seeVisible1 :: Assertion
test_seeVisible1 = do
  (res,state) <- runStateT (seeVisible False (mkLine ipp2 (TOD 1000 999)) ipp1 ipp2 ) st1

  case (state == st1) of
    True  -> return ()
    False -> assertFailure ("State has been modified:" ++ (show state))

-- ---------------------------------------------------------------------

test_seeVisible2 :: Assertion
test_seeVisible2 = do
  let
    line = mkLine ipp2 (TOD 1000 999)
  (res,state) <- runStateT (seeVisible True line ipp1 ipp2 ) st2

  let Just lineNew = getLineMaybe (swMaster state) hash2
  
  case (line == lineNew) of
    True  -> assertFailure ("Line is the same:" ++ (show line))
    False -> assertFailure ("Line has been modified:" ++ (show lineNew))

-- ---------------------------------------------------------------------
          
-- Note: near_to :: IPP -> IPP -> TeleHash [Hash]
--       near_to end ipp
test_near_to1 :: Assertion
test_near_to1 = do
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
                 })

  (res,state) <- runStateT (near_to ipp1 ipp2 ) st

  case res of
    Left errStr -> return ()
    Right _     -> assertFailure "found non-existent line"

  
-- ---------------------------------------------------------------------          

test_near_to2 :: Assertion
test_near_to2 = do
  -- Check that a line exists for the given ipp. If not, should get an undefined result
  let
    st = (Switch {swH = Nothing
                 , swSeeds = [] 
                 , swSeedsIndex = (Set.fromList [])
                 , swConnected = False
                 , swMaster = Map.fromList [(mkHash ipp2, mkLine ipp2 (TOD 1000 999) )]
                 , swSelfIpp = Nothing 
                 , swSelfHash = Nothing 
                 , swTaps = []
                 })

  (res,state) <- runStateT (near_to ipp1 ipp2) st

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
              })

st2 = (Switch {swH = Nothing
              , swSeeds = [] 
              , swSeedsIndex = (Set.fromList [])
              , swConnected = False
              , swMaster = Map.fromList 
                           [
                             (hash1,
                              (mkLine ipp1 (TOD 1000 999) ) {lineNeighbors = Set.fromList [hash1,hash2]}) -- ipp under test
                           , (hash2,(mkLine ipp2 (TOD 1002 999) ) {lineVisible = True })-- first neighbour
                           , (hash3,(mkLine ipp3 (TOD 1003 999) ) {lineVisible = True })-- second neighbour
                           ]
              , swSelfIpp = Nothing 
              , swSelfHash = Nothing 
              , swTaps = []
              })
      
ipp1 = IPP "1.2.3.4:1234"
ipp2 = IPP "2.3.4.5:2345"
ipp3 = IPP "3.4.5.6:3456"
ipp4 = IPP "4.5.6.7:4567"
hash1 = mkHash ipp1          
hash2 = mkHash ipp2          
hash3 = mkHash ipp3
hash4 = mkHash ipp4

test_near_to3 :: Assertion
test_near_to3 = do
  -- Check that the ipp has some visible neighbours
  (res,state) <- runStateT (near_to ipp1 ipp2 ) st2

  case res of
    Left msgStr -> assertFailure msgStr
    Right [Hash "0ec331643f4a7a74068ea47dda062b48b419c832"] -> return ()
  
-- ---------------------------------------------------------------------          

{-
test_recvTelex1 = 
  recvTelex msg rinfo @?= "foo"
  where
    msg = undefined
    rinfo = undefined      
-}

test_parseMsg1Js = 
  parseTelex msg1Js @?= (
    Telex {teleRing = Nothing, teleSee = Just ["208.68.163.247:42424"],
           teleBr = 74, teleTo = IPP "196.209.236.12:34963",
           teleLine = Just 412367436, 
           teleHop = Nothing,
           teleSigEnd = Just $ Hash "38666817e1b38470644e004b9356c1622368fa57",
           teleSigPop = Nothing,
           teleTap = Just [Tap {tapIs = ("+end","38666817e1b38470644e004b9356c1622368fa57"), 
                                tapHas = ["+pop"]}],
           teleRest = [(".tap","[{\"has\":[\"+pop\"],\"is\":{\"+end\":\"38666817e1b38470644e004b9356c1622368fa57\"}}]"),("_line","412367436"),(".see","[\"208.68.163.247:42424\"]"),("_br","74"),("+end","\"38666817e1b38470644e004b9356c1622368fa57\""),("_to","\"196.209.236.12:34963\"")],
           teleMsgLength = Just (length msg1Js)
          })
  
msg1Js = "{\".tap\":[{\"has\":[\"+pop\"],\"is\":{\"+end\":\"38666817e1b38470644e004b9356c1622368fa57\"}}],"++
       "\"_line\":412367436,"++
       "\".see\":[\"208.68.163.247:42424\"],"++
       "\"_br\":74,"++
       "\"+end\":\"38666817e1b38470644e004b9356c1622368fa57\","++
       "\"_to\":\"196.209.236.12:34963\"}"

test_encodeMsg1Js =
  encodeMsg msg1Js @?= "\"{\\\".tap\\\":[{\\\"has\\\":[\\\"+pop\\\"],\\\"is\\\":{\\\"+end\\\":\\\"38666817e1b38470644e004b9356c1622368fa57\\\"}}],\\\"_line\\\":412367436,\\\".see\\\":[\\\"208.68.163.247:42424\\\"],\\\"_br\\\":74,\\\"+end\\\":\\\"38666817e1b38470644e004b9356c1622368fa57\\\",\\\"_to\\\":\\\"196.209.236.12:34963\\\"}\""
  
-- --------------------------------------------

-- lineOk tests
-- 1. Timeout - relates to line only
-- 2a. msg.line is set, but no line.ringout
--     * line.ringout is set when setting up a line, impossible situation
-- 2b. line.line == msg.line
-- 2c. msg.line is a multiple of our ring
-- 3a. msg.ring is > 0 and <= 32768
-- 3b. msg.ring == line.ringin, if there is a line.ringin 

test_lineOk_msgRingMatchesLine =
  True @=? isRingOk (line1{ lineRingin = Just 15}) (msg1 { teleRing = Just 15})

test_lineOk_msgRingMisMatchesLine =
  False @=? isRingOk (line1{ lineRingin = Just 14}) (msg1 { teleRing = Just 15})

-- ---------------------------------------------------------------------

test_lineOk_msgRingValid1 =
  False @=? isRingOk line1 msg1 { teleRing = Just 0}

test_lineOk_msgRingValid2 =
  True @=? isRingOk line1 msg1 { teleRing = Just 1}

test_lineOk_msgRingValid3 =
  True @=? isRingOk line1 msg1 { teleRing = Just 32768}

test_lineOk_msgRingValid4 =
  False @=? isRingOk line1 msg1 { teleRing = Just 32769}

-- ---------------------------------------------------------------------

test_lineOk_ringMatch =
  True @=? isLineOk (line1 {lineLine = Just 12345, lineRingout = 5 }) 1008 (msg1 { teleLine = Just 12345 })

test_lineOk_ringMisMatch =
  False @=? isLineOk (line1 {lineLine = Just 12345, lineRingout = 7 }) 1008 (msg1 { teleLine = Just 12345 })

test_lineOk_lineMatch1 =
  True @=? isLineOk (line1 {lineLine = Just 12345 }) 1008 (msg1 { teleLine = Just 12345 })

test_lineOk_lineMatch2 =
  True @=?isLineOk (line1 {lineLineat = Nothing, lineRingout=15}) 1003 (msg1 {teleLine = Just 12345})

test_lineOk_lineMisMatch =
  False @=? isLineOk (line1 {lineLine = Just 12345 }) 1008 (msg1 { teleLine = Just 1 })

-- ---------------------------------------------------------------------

test_lineOk_timeoutOk =
  True @=? isLineOk line1 1010 msg1
  
test_lineOk_timeoutFail =
  False @=? isLineOk line1 1011 msg1

line1 = (mkLine (IPP "telehash.org:42424") (TOD 1000 999)) { lineLineat = Just (TOD 1000 999), lineRingout = 5, lineBr = 10 }
msg1 = (mkTelex (IPP "1.2.3.4:567")) { teleMsgLength = Just 100 }

-- ---------------------------------------------------------------------
-- checkLine tests, in terms of modifying line state
-- If lineat is not set
--   set lineat, ringin, and line
-- set line br, brin
-- drop line if brin/brout delta > 12k

test_checkLineBrDrop =
  (False, line1 {lineSeenat = Just (TOD 1000 999), lineBr = 12004, lineBrin = 100, lineBrout = 3,
                lineRingin = Just 823, lineLine = Just 12345, lineRingout = 15}) 
  @=? checkLine (line1 {lineLineat = Nothing, lineRingout=15, lineBr = 11904, lineBrout = 3}) 
                (msg1 {teleLine = Just 12345, teleRing = Nothing}) 
                (TOD 1000 999)

test_checkLineBrNoDrop =
  (True, line1 {lineSeenat = Just (TOD 1000 999), lineBr = 12003, lineBrin = 100, lineBrout = 3,
                lineRingin = Just 823, lineLine = Just 12345, lineRingout = 15}) 
  @=? checkLine (line1 {lineLineat = Nothing, lineRingout=15, lineBr = 11903, lineBrout = 3}) 
                (msg1 {teleLine = Just 12345, teleRing = Nothing}) 
                (TOD 1000 999)


test_checkLineRing1 =
  (True, line1 {lineSeenat = Just (TOD 1000 999), lineBr = 110, lineBrin = 100,
                lineRingin = Just 823, lineLine = Just 12345, lineRingout = 15}) 
  @=? checkLine (line1 {lineLineat = Nothing, lineRingout=15}) 
                (msg1 {teleLine = Just 12345, teleRing = Nothing}) 
                (TOD 1000 999)

test_checkLineRing2 =
  (True, line1 {lineSeenat = Just (TOD 1000 999), lineBr = 110, lineBrin = 100,
                lineRingin = Just 823, lineLine = Just 12345, lineRingout = 15}) 
  @=? checkLine (line1 {lineLineat = Nothing, lineRingout=15}) 
                (msg1 {teleLine = Nothing, teleRing = Just 823}) 
                (TOD 1000 999)

-- -----------------------------------------------

test_checkLine1 =
  (True, line1 {lineSeenat = Just (TOD 1000 999), lineBr = 110, lineBrin = 100}) 
  @=? checkLine line1 msg1 (TOD 1000 999)

test_checkLine2 =
  (False, line1 {lineSeenat = Just (TOD 1020 999), lineBr = 10, lineBrin = 0}) 
  @=? checkLine line1 msg1 (TOD 1020 999)

-- ---------------------------------------------------------------------
          
test_getCommands1 =
  [
    (".see","[\"196.215.40.28:59056\"]"),
    (".tap","[{\"is\":{\"+end\":\"f507a91f7277fb46e34eebf17a76f0e0351f6269\"},\"has\":[\"+pop\"]}]")
  ]
  @=? (getCommands $ parseTelex "{\"_to\":\"208.68.163.247:42424\",\"+end\":\"f507a91f7277fb46e34eebf17a76f0e0351f6269\",\".see\":[\"196.215.40.28:59056\"],\".tap\":[{\"is\":{\"+end\":\"f507a91f7277fb46e34eebf17a76f0e0351f6269\"},\"has\":[\"+pop\"]}],\"_line\":252817576,\"_br\":89}")
  
test_getSignals1 =
  [("+end","\"f507a91f7277fb46e34eebf17a76f0e0351f6269\"")]
  @=? (getSignals $ parseTelex "{\"_to\":\"208.68.163.247:42424\",\"+end\":\"f507a91f7277fb46e34eebf17a76f0e0351f6269\",\".see\":[\"196.215.40.28:59056\"],\".tap\":[{\"is\":{\"+end\":\"f507a91f7277fb46e34eebf17a76f0e0351f6269\"},\"has\":[\"+pop\"]}],\"_line\":252817576,\"_br\":89}")


-- ----------------------------------------------------------------------

test_DistanceTo1 =
  -1 @=? distanceTo (mkHash (IPP "208.68.163.247:42424")) (mkHash (IPP "208.68.163.247:42424"))

test_DistanceTo2 =
  158 @=? distanceTo (mkHash (IPP "208.68.163.247:42424")) (mkHash (IPP "208.68.160.25:32771"))

-- EOF
