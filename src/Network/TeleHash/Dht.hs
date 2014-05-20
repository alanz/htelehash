module Network.TeleHash.Dht
  (
    dhtMaint
  , insertIntoDht
  , deleteFromDht
  , getBucketContentsForHn
  , dhtBucket
  , distanceTo
  ) where


import Control.Exception
import Control.Monad.State
import Data.Bits
import Data.Char
import Data.List
import Data.Maybe
import System.Time

import Network.TeleHash.SwitchApi
import Network.TeleHash.Types
import Network.TeleHash.Utils

import qualified Data.Map as Map
import qualified Data.Set as Set


-- | Manage the DHT
-- See https://github.com/telehash/telehash.org/blob/master/dht.md

-- ---------------------------------------------------------------------

-- |Called periodically to maintain the DHT.
-- Spec calls for maintenance every 29 secs
dhtMaint :: TeleHash ()
dhtMaint = do
  now <- io $ getClockTime
  logR $ "dhtMaint entered at " ++ show now
  dht <- gets swDht
  k <- gets swDhtK
  link_max <- gets swDhtLinkMax
  forM_ (Map.assocs dht) $ \(d,_b) -> do
    -- sort by age and send maintenance to only k links
    bucket <- getBucketContents d
    let sorted = sortBy sf bucket
        sf a b = compare (hLinkAge a) (hLinkAge b)
    if length sorted > 0
      then logR $ "dhtMaint:processing bucket " ++ show d
      else return ()
    forM_ (take k sorted) $ \hc -> do
      if isNothing (hLinkAge hc) || hChanOut hc == nullChannelId
        then return ()
        else do
          hstr <- showHashName now hc
          logR $ "dhtMaint:considering " ++ hstr
          if isTimeOut now (hLinkAge hc) param_link_timeout_secs
            then do
              case hLinkChan hc of
                Nothing -> do
                  logR $ "dhtMaint:expected a valid hLinkChan for " ++ show hc
                  assert False undefined
                Just cid -> do
                  mc <- getChanMaybe cid
                  case mc of
                    Nothing -> do
                      logR $ "dhtMaint: bad hLinkChan, cannot to refresh link for " ++ show hc
                    Just _ -> return ()
                  void $ link_hn (hHashName hc) (Just cid)
            else return ()


{-
js equivalent

// every link that needs to be maintained, ping them
function linkMaint(self)
{
  // process every bucket
  Object.keys(self.buckets).forEach(function(bucket){
    // sort by age and send maintenance to only k links
    var sorted = self.buckets[bucket].sort(function(a,b){ return a.age - b.age });
    if(sorted.length) debug("link maintenance on bucket",bucket,sorted.length);
    sorted.slice(0,defaults.link_k).forEach(function(hn){
      if(!hn.linked || !hn.alive) return;
      if((Date.now() - hn.linked.sentAt) < Math.ceil(defaults.link_timer/2)) return; // we sent to them recently
      hn.linked.send({js:{seed:self.seed}});
    });
  });
}

-}

-- ---------------------------------------------------------------------

-- |Get the relevant bucket, and dereference all the HashNames
getBucketContents :: HashDistance -> TeleHash [HashContainer]
getBucketContents hd = do
  sw <- get
  let bucket = (Map.findWithDefault Set.empty hd (swDht sw))
  hcs <- mapM getHN $ Set.toList bucket
  return hcs

-- ---------------------------------------------------------------------

insertIntoDht :: HashName -> TeleHash ()
insertIntoDht hn = do
  (distance,bucket) <- getBucketContentsForHn hn
  sw <- get
  put $ sw { swDht = Map.insert distance (Set.insert hn bucket) (swDht sw) }

-- ---------------------------------------------------------------------

deleteFromDht :: HashName -> TeleHash ()
deleteFromDht hn = do
  (distance,bucket) <- getBucketContentsForHn hn
  sw <- get
  put $ sw { swDht = Map.insert distance (Set.delete hn bucket) (swDht sw) }

-- ---------------------------------------------------------------------

getBucketContentsForHn :: HashName -> TeleHash (HashDistance,Bucket)
getBucketContentsForHn hn = do
  distance <- dhtBucket hn
  sw <- get
  case Map.lookup distance (swDht sw) of
    Nothing -> return (distance,Set.empty)
    Just b  -> return (distance,b)

-- ---------------------------------------------------------------------

-- |Calculate the bucket as the hash distance between our hashname and
-- the passed in one
dhtBucket :: HashName -> TeleHash HashDistance
dhtBucket hn = do
  sw <- get
  return $ distanceTo (swId sw) hn

-- ---------------------------------------------------------------------

-- TODO: consider memoising this result, will be used a LOT
distanceTo :: HashName -> HashName -> HashDistance
distanceTo (HN this) (HN h) = go 252 diffs
  where
    go acc [] = acc
    go _acc (-1:[]) = -1
    go acc (-1:xs) = go (acc - 4) xs
    go acc (x:_xs) = acc + x

    diffs = map (\(a,b) -> sbtab !! (xor (digitToInt a) (digitToInt b))) $ zip this h
    sbtab = [-1,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3]

{-
-- javascript version

// XOR distance between two hex strings, high is furthest bit, 0 is closest bit, -1 is error
function dhash(h1, h2) {
  // convert to nibbles, easier to understand
  var n1 = hex2nib(h1);
  var n2 = hex2nib(h2);
  if(!n1.length || !n2.length) return -1;
  // compare nibbles
  var sbtab = [-1,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3];
  var ret = 252;
  for (var i = 0; i < n1.length; i++) {
    if(!n2[i]) return ret;
    var diff = n1[i] ^ n2[i];
    if (diff) return ret + sbtab[diff];
    ret -= 4;
  }
  return ret;
}

// convert hex string to nibble array
function hex2nib(hex)
{
  var ret = [];
  for (var i = 0; i < hex.length / 2; i ++) {
      var bite = parseInt(hex.substr(i * 2, 2), 16);
      if (isNaN(bite)) return [];
      ret[ret.length] = bite >> 4;
      ret[ret.length] = bite & 0xf;
  }
  return ret;
}

-}

-- ---------------------------------------------------------------------
