{-# LANGUAGE TemplateHaskell #-}
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.HUnit

import Control.Monad.State
import Data.List
import Network.Socket
import System.Time
import TeleHash.Controller
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Testing, see http://hackage.haskell.org/package/test-framework-th
main = $(defaultMainGenerator)

-- ---------------------------------------------------------------------

case_seeVisible1 :: Assertion
case_seeVisible1 = do
  (res,state) <- runStateT (seeVisible False (mkLine ipp2 (TOD 1000 999)) ipp1 ipp2 ) st1

  case (state == st1) of
    True  -> return ()
    False -> assertFailure ("State has been modified:" ++ (show state))

-- ---------------------------------------------------------------------

case_seeVisible2 :: Assertion
case_seeVisible2 = do
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
    st = (Switch {swH = Nothing
                 , swSeeds = [] 
                 , swSeedsIndex = (Set.fromList [])
                 , swConnected = False
                 , swMaster = Map.fromList [(mkHash ipp2, mkLine ipp2 (TOD 1000 999) )]
                 , swSelfIpp = Nothing 
                 , swSelfHash = Nothing 
                 , swTaps = []
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

case_near_to3 :: Assertion
case_near_to3 = do
  -- Check that the ipp has some visible neighbours
  (res,state) <- runStateT (near_to hash1 ipp2 ) st2

  case res of
    Left msgStr -> assertFailure msgStr
    Right [Hash "0ec331643f4a7a74068ea47dda062b48b419c832"] -> return ()
  
-- ---------------------------------------------------------------------          

{-
_case_recvTelex1 = 
  recvTelex msg rinfo @?= "foo"
  where
    msg = undefined
    rinfo = undefined      
-}

case_parseMsg1Js = 
  parseTelex msg1Js @?= (
    Telex {teleRing = Nothing, teleSee = Just [IPP "208.68.163.247:42424"],
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

case_encodeMsg1Js =
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

case_lineOk_msgRingMatchesLine =
  True @=? isRingOk (line1{ lineRingin = Just 15}) (msg1 { teleRing = Just 15})

case_lineOk_msgRingMisMatchesLine =
  False @=? isRingOk (line1{ lineRingin = Just 14}) (msg1 { teleRing = Just 15})

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
  True @=? isLineOk (line1 {lineLine = Just 12345, lineRingout = 5 }) 1008 (msg1 { teleLine = Just 12345 })

case_lineOk_ringMisMatch =
  False @=? isLineOk (line1 {lineLine = Just 12345, lineRingout = 7 }) 1008 (msg1 { teleLine = Just 12345 })

case_lineOk_lineMatch1 =
  True @=? isLineOk (line1 {lineLine = Just 12345 }) 1008 (msg1 { teleLine = Just 12345 })

case_lineOk_lineMatch2 =
  True @=?isLineOk (line1 {lineLineat = Nothing, lineRingout=15}) 1003 (msg1 {teleLine = Just 12345})

case_lineOk_lineMisMatch =
  False @=? isLineOk (line1 {lineLine = Just 12345 }) 1008 (msg1 { teleLine = Just 1 })

-- ---------------------------------------------------------------------

case_lineOk_timeoutOk =
  True @=? isLineOk line1 1010 msg1
  
case_lineOk_timeoutFail =
  False @=? isLineOk line1 1011 msg1

line1 = (mkLine (IPP "telehash.org:42424") (TOD 1000 999)) { lineLineat = Just (TOD 1000 999), lineRingout = 5, lineBr = 10 }
msg1 = (mkTelex (IPP "1.2.3.4:567")) { teleMsgLength = Just 100 }

-- ---------------------------------------------------------------------
-- checkLine tests, in terms of modifying line state
-- If lineat is not set
--   set lineat, ringin, and line
-- set line br, brin
-- drop line if brin/brout delta > 12k

case_checkLineBrDrop =
  (False, line1 {lineSeenat = Just (TOD 1000 999), lineBr = 12004, lineBrin = 100, lineBrout = 3,
                lineRingin = Just 823, lineLine = Just 12345, lineRingout = 15}) 
  @=? checkLine (line1 {lineLineat = Nothing, lineRingout=15, lineBr = 11904, lineBrout = 3}) 
                (msg1 {teleLine = Just 12345, teleRing = Nothing}) 
                (TOD 1000 999)

case_checkLineBrNoDrop =
  (True, line1 {lineSeenat = Just (TOD 1000 999), lineBr = 12003, lineBrin = 100, lineBrout = 3,
                lineRingin = Just 823, lineLine = Just 12345, lineRingout = 15}) 
  @=? checkLine (line1 {lineLineat = Nothing, lineRingout=15, lineBr = 11903, lineBrout = 3}) 
                (msg1 {teleLine = Just 12345, teleRing = Nothing}) 
                (TOD 1000 999)


case_checkLineRing1 =
  (True, line1 {lineSeenat = Just (TOD 1000 999), lineBr = 110, lineBrin = 100,
                lineRingin = Just 823, lineLine = Just 12345, lineRingout = 15}) 
  @=? checkLine (line1 {lineLineat = Nothing, lineRingout=15}) 
                (msg1 {teleLine = Just 12345, teleRing = Nothing}) 
                (TOD 1000 999)

case_checkLineRing2 =
  (True, line1 {lineSeenat = Just (TOD 1000 999), lineBr = 110, lineBrin = 100,
                lineRingin = Just 823, lineLine = Just 12345, lineRingout = 15}) 
  @=? checkLine (line1 {lineLineat = Nothing, lineRingout=15}) 
                (msg1 {teleLine = Nothing, teleRing = Just 823}) 
                (TOD 1000 999)

-- -----------------------------------------------

case_checkLine1 =
  (True, line1 {lineSeenat = Just (TOD 1000 999), lineBr = 110, lineBrin = 100}) 
  @=? checkLine line1 msg1 (TOD 1000 999)

case_checkLine2 =
  (False, line1 {lineSeenat = Just (TOD 1020 999), lineBr = 10, lineBrin = 0}) 
  @=? checkLine line1 msg1 (TOD 1020 999)

-- ---------------------------------------------------------------------
          
case_getCommands1 =
  [".see",".tap"]
  @=? (getCommands $ parseTelex "{\"_to\":\"208.68.163.247:42424\",\"+end\":\"f507a91f7277fb46e34eebf17a76f0e0351f6269\",\".see\":[\"196.215.40.28:59056\"],\".tap\":[{\"is\":{\"+end\":\"f507a91f7277fb46e34eebf17a76f0e0351f6269\"},\"has\":[\"+pop\"]}],\"_line\":252817576,\"_br\":89}")
  
case_getSignals1 =
  [("+end","\"f507a91f7277fb46e34eebf17a76f0e0351f6269\"")]
  @=? (getSignals $ parseTelex "{\"_to\":\"208.68.163.247:42424\",\"+end\":\"f507a91f7277fb46e34eebf17a76f0e0351f6269\",\".see\":[\"196.215.40.28:59056\"],\".tap\":[{\"is\":{\"+end\":\"f507a91f7277fb46e34eebf17a76f0e0351f6269\"},\"has\":[\"+pop\"]}],\"_line\":252817576,\"_br\":89}")


-- ----------------------------------------------------------------------

case_DistanceTo1 =
  -1 @=? distanceTo (mkHash (IPP "208.68.163.247:42424")) (mkHash (IPP "208.68.163.247:42424"))

case_DistanceTo2 =
  158 @=? distanceTo (mkHash (IPP "208.68.163.247:42424")) (mkHash (IPP "208.68.160.25:32771"))

-- ---------------------------------------------------------------------

case_EncodeTelex1 =
  "{\"_to\":\"1.2.3.4:567\",\"_br\":\"0\"}" @=? encodeTelex msg1
  
case_EncodeTelex2 =
  "{\"_to\":\"1.2.3.4:1234\"," ++ 
  "\"_ring\":\"13\"," ++ 
  "\".see\":[\"2.3.4.5:2345\",\"3.4.5.6:3456\"]," ++ 
  "\"_br\":\"234\"," ++
  "\"_line\":\"4567\"," ++ 
  "\"_hop\":\"3\"," ++ 
  "\"+end\":\"255b5a502b0a348883ffa757e0c1ea812a128088\"," ++
  "\".tap\":[{\"is\":{\".end\":\"foo\",\"has\":[\"+pop\"]}}]," ++
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
  (a,b,c) <- (resolveToSeedIPP "telehash.org:42424")

  case (a,b,c) of
    (_,"208.68.163.247","42424") -> return ()
    (x,y,z)        -> assertFailure ("case_resolveToSeedIpp:" ++ (show (x,y,z)))

case_addrFromHostPort = do
  addr <- addrFromHostPort "1.2.3.4" "42424"
  case addr of
    (SockAddrInet 42424 0x04030201) -> return ()
    (SockAddrInet p h) -> assertFailure ("case_addrFromHostPort:" ++ (show (p,h)))
    
-- EOF
