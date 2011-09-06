import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import System.Time

import Data.List
import TeleHash.Controller
import qualified Data.ByteString.Char8 as BC

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
            ]
          ]
  
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
           teleBr = 74, teleTo = "196.209.236.12:34963",
           teleLine = Just 412367436, 
           teleHop = Nothing,
           teleSigEnd = Just $ Hash "38666817e1b38470644e004b9356c1622368fa57",
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

line1 = (mkLine "telehash.org:42424" (TOD 1000 999)) { lineLineat = Just (TOD 1000 999), lineRingout = 5, lineBr = 10 }
msg1 = (mkTelex "1.2.3.4:567") { teleMsgLength = Just 100 }

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
  -1 @=? distanceTo (mkHash "208.68.163.247:42424") (mkHash "208.68.163.247:42424")

test_DistanceTo2 =
  158 @=? distanceTo (mkHash "208.68.163.247:42424") (mkHash "208.68.160.25:32771")

-- EOF
