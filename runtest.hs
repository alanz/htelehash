import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Data.List
import TeleHash.Controller
import qualified Data.ByteString.Char8 as BC

main = defaultMain tests

tests = [ testGroup "group 1" [
             testCase "sort7" test_sort7
             ],
          testGroup "recvTelex" [
            -- testCase "basic_rx" test_recvTelex1
            testCase "parseMsg" test_parseMsg
            ]
          ]
  
{-
test_recvTelex1 = 
  recvTelex msg rinfo @?= "foo"
  where
    msg = undefined
    rinfo = undefined      
-}

test_parseMsg = 
  Nothing @?= parseTeleHashEntry msg
  where
    -- msg= BC.pack "{\".tap\":[{\"has\":[\"+pop\"],\"is\":{\"+end\":\"38666817e1b38470644e004b9356c1622368fa57\"}}],\"_line\":412367436,\".see\":[\"208.68.163.247:42424\"],\"_br\":74,\"+end\":\"38666817e1b38470644e004b9356c1622368fa57\",\"_to\":\"196.209.236.12:34963\"}"
    msg= BC.pack "{\"+end\":\"1234\",\".see\":null,\"_br\":0,\"_ring\":null,\"_to\":\"208.68.163.247:42424\"}"
    {-
    msg = BC.pack ("{\"_ring\":17904," ++
       "\".see\":[ \"208.68.163.247:42424\", \"208.68.160.25:55137\"]," ++ 
       "\"_br\":52," ++ 
       "\"_to\":\"173.19.99.143:63681\" }")
    -}

test_sort7 = sort [8, 7, 2, 5, 4, 9, 6, 1, 0, 3] @?= [0..9]
  
