import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Data.List
import TeleHash.Controller
import qualified Data.ByteString.Char8 as BC

main :: IO ()
main = defaultMain tests

tests = [ testGroup "group 1" [
             testCase "sort7" test_sort7
             ],
          testGroup "recvTelex" [
            -- testCase "basic_rx" test_recvTelex1
            testCase "parseMsg1" test_parseMsg1
            -- testCase "parseMsg2" test_parseMsg2
            ]
          ]
  
{-
test_recvTelex1 = 
  recvTelex msg rinfo @?= "foo"
  where
    msg = undefined
    rinfo = undefined      
-}

test_parseMsg1 = 
  parseTeleHashEntry msg @?= (TeleHashEntry {teleRing = Nothing, teleSee = Just ["208.68.163.247:42424"],
                                             teleBr = 74, teleTo = "196.209.236.12:34963",
                                             teleLine = Just 412367436, 
                                             teleHop = Nothing,
                                             teleSigEnd = Just "38666817e1b38470644e004b9356c1622368fa57"
                                             })
  where
    msg= "{\".tap\":[{\"has\":[\"+pop\"],\"is\":{\"+end\":\"38666817e1b38470644e004b9356c1622368fa57\"}}],"++
         "\"_line\":412367436,"++
         "\".see\":[\"208.68.163.247:42424\"],"++
         "\"_br\":74,"++
         "\"+end\":\"38666817e1b38470644e004b9356c1622368fa57\","++
         "\"_to\":\"196.209.236.12:34963\"}"


test_sort7 = sort [8, 7, 2, 5, 4, 9, 6, 1, 0, 3] @?= [0..9]
  
