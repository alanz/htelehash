module Network.TeleHash.Dht
  (
    dhtMaint
  , distanceTo

  ) where

-- | Manage the DHT
-- See https://github.com/telehash/telehash.org/blob/master/dht.md

import Control.Exception
import Data.Bits
import Data.Char

import Network.TeleHash.Types
import Network.TeleHash.Utils

-- ---------------------------------------------------------------------

-- |Called periodically to maintain the DHT.
-- Spec calls for maintenance every 55 secs
dhtMaint :: TeleHash ()
dhtMaint = do
  assert False undefined

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
