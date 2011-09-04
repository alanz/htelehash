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
          , testCase "lineOk_lineMatch"     test_lineOk_lineMatch
          , testCase "lineOk_lineMisMatch"  test_lineOk_lineMisMatch
          , testCase "lineOk_ringMatch"     test_lineOk_ringMatch
          , testCase "lineOk_ringMisMatch"  test_lineOk_ringMisMatch
            
          , testCase "lineOk_msgRingValid1" test_lineOk_msgRingValid1
          , testCase "lineOk_msgRingValid2" test_lineOk_msgRingValid2
          , testCase "lineOk_msgRingValid3" test_lineOk_msgRingValid3
          , testCase "lineOk_msgRingValid4" test_lineOk_msgRingValid4
              
          , testCase "lineOk_msgRingMatchesLine"    test_lineOk_msgRingMatchesLine
          , testCase "lineOk_msgRingMisMatchesLine" test_lineOk_msgRingMisMatchesLine
          
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
                                tapHas = ["+pop"]}]
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

test_lineOk_lineMatch =
  True @=? isLineOk (line1 {lineLine = Just 12345 }) 1008 (msg1 { teleLine = Just 12345 })

test_lineOk_lineMisMatch =
  False @=? isLineOk (line1 {lineLine = Just 12345 }) 1008 (msg1 { teleLine = Just 1 })

-- ---------------------------------------------------------------------

test_lineOk_timeoutOk =
  True @=? isLineOk line1 1010 msg1
  
test_lineOk_timeoutFail =
  False @=? isLineOk line1 1011 msg1


line1 = (mkLine "telehash.org:42424" (TOD 1000 999)) { lineLineat = Just (TOD 1000 999), lineRingout = 5 }
msg1 = mkTelex "1.2.3.4:567"

-- EOF
