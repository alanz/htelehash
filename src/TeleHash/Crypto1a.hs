module TeleHash.Crypto1a
  (
    cset_1a
  --  init
  , crypt_keygen_1a
  , crypt_loadkey_1a
  ) where

-- | Implement Cypher Suite 1a for TeleHash, as per
-- https://github.com/telehash/telehash.org/blob/master/cs/1a.md

{-

    SHA1 - well trusted and performs well on small devices
    ECC secp160r1 - small key sizes, balance of strong crypto (~1024bit) and still supportable with low cpu
    HMAC-SHA1 - common implementations available for embedded environments
    AES-128-CTR - low impact cipher, many implementations including hardware ones

-}


import Crypto.PubKey.ECC.ECDSA
import Crypto.PubKey.ECC.Generate
import Crypto.Random
import Crypto.Types.PubKey.ECC
import Data.ByteString.Base64
import Data.List
import Data.Maybe
import Data.Word
import Crypto.Number.Serialize
import TeleHash.Utils
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Digest.Pure.SHA as SHA

curve = getCurveByName SEC_p160r1

-- ---------------------------------------------------------------------

cset_1a = CS
  {
  csLoadkey = crypt_loadkey_1a
  }
-- ---------------------------------------------------------------------

-- | Generate base64 encoded public and matching private keys
crypt_keygen_1a :: CPRG t => t -> ((B.ByteString, B.ByteString), t)
crypt_keygen_1a g = ((pub_b64,priv_b64),g')
  where
    ((pub,priv) ,g') = generate g curve
    (PublicKey _ pubk) = pub
    (PrivateKey _ privk) = priv

    pub_b64 = encode $ pointTow8s pubk

    (Just p20) = i2ospOf 20 privk
    priv_b64 = encode p20

{-
int crypt_keygen_1a(packet_t p)
{
  char b64[ECC_BYTES*4];
  uint8_t id_private[ECC_BYTES], id_public[ECC_BYTES*2];

  // create line ephemeral key
  ecc_make_key(id_public, id_private);

  base64enc(b64,id_public,ECC_BYTES*2);
  packet_set_str(p,"1a",b64);

  base64enc(b64,id_private,ECC_BYTES);
  packet_set_str(p,"1a_secret",b64);

  return 0;
}

-}

-- ---------------------------------------------------------------------

crypt_loadkey_1a :: Maybe HashContainer -> String -> Maybe String
   -> TeleHash (Maybe HashContainer)
crypt_loadkey_1a mhc pub mpriv = do
  -- base64 decode it
  let mbs = decode $ B8.pack pub
  case mbs of
    Left _err -> do
      logT $ "invalid public key b64decode failed:" ++ pub
      return Nothing
    Right bs -> do
      if B8.length bs /= 40
        then do logT $ "invalid public key wrong len:" ++ pub
                return Nothing
        else do
          -- convert the ByteString into a pair of Integer
          let (b1,b2) = B.splitAt 20 bs
              i1 = os2ip b1
              i2 = os2ip b2
          -- create the public key
              pubkey = PublicKey curve (Point i1 i2)
              hc' = case mpriv of
                Nothing -> mhc
                Just priv -> hcp
                  where
                    mbsp = decode $ B8.pack priv
                    hcp = case mbsp of
                      Left _err -> mhc
                      Right bsp -> Just $ hcn {hcPrivate = Just $ Private1a privkey}
                        where
                          i = os2ip bsp
                          privkey = PrivateKey curve i
                          hexName = mkHashFromBS bs
                          newParts = [("1a",unHash hexName)]
                          hcn = case mhc of
                            Just h  -> h  { hcHashName = parts2hn (hcParts h ++ newParts)
                                          , hcHexName = hexName
                                          , hcParts = (hcParts h ++ newParts)
                                          , hcCsid = "1a"
                                          , hcKey = pub
                                          , hcPublic = Public1a pubkey
                                          , hcPrivate = Just $ Private1a privkey
                                          }
                            Nothing -> HC { hcHashName = parts2hn newParts
                                          , hcHexName = hexName
                                          , hcParts = newParts
                                          , hcCsid = "1a"
                                          , hcKey = pub
                                          , hcPublic = Public1a pubkey
                                          , hcPrivate = Just $ Private1a privkey
                                          }

          logT $ "crypt_loadkey_1a:parts=" ++ show (hcParts $ fromJust hc')
          return hc'

mkHashFromBS :: B8.ByteString -> Hash
mkHashFromBS bs =
  let
    digest = SHA.sha1 $ BL.fromChunks [bs]
  in
   Hash (show digest)

mkHashFromB64 :: String -> Hash
mkHashFromB64 str =
  let
    digest = SHA.sha1 $ BL.fromChunks [B8.pack str]
  in
   -- B64.encode $ BL.unpack $ SHA.bytestringDigest digest
   Hash (show digest)

testhash :: IO ()
testhash = do
  let b64 = "o0UL/D6qQ+dcSX7hCoyMjLDYeA6dNScZ+YY/fcX4fyCtsSO2u9L5Lg=="
  let Right bs = decode $ B8.pack b64
  let digest = SHA.sha1 $ BL.fromChunks [bs]
  putStrLn (show digest)

  let mh = parts2hn [("1a",show digest)]
  putStrLn (show [("1a",show digest)])
  putStrLn (show mh)

  putStrLn (show $ parts2hn [("1a","a5c8b5c8a630c84dc01f92d2e5af6aa41801457a")
                            ,("2a","bf6e23c6db99ed2d24b160e89a37c9cd183fb61afeca40c4bc378cf6e488bebe")
                            ])

  putStrLn "foo"
  let
    digest' = SHA256.finalize ctx
    ctx    = foldl' SHA256.update iCtx (map BU.fromString
                     [ "1a", "a5c8b5c8a630c84dc01f92d2e5af6aa41801457a",
                       "2a", "bf6e23c6db99ed2d24b160e89a37c9cd183fb61afeca40c4bc378cf6e488bebe"
                     ])
    iCtx   = SHA256.init
  putStrLn $ show $ BU.toString $ B16.encode digest'


  let c0 = SHA256.init
      c1 = SHA256.update c0 $ BU.fromString "1a"
      c2 = SHA256.update c1 $ BU.fromString "a5c8b5c8a630c84dc01f92d2e5af6aa41801457a"
      c3 = SHA256.update c2 $ BU.fromString "2a"
      c4 = SHA256.update c3 $ BU.fromString "bf6e23c6db99ed2d24b160e89a37c9cd183fb61afeca40c4bc378cf6e488bebe"
  putStrLn $ show $ BU.toString $ B16.encode $ SHA256.finalize c4

  let h1 = SHA256.hash $ B8.pack "1a"
      h2 = SHA256.hash $ B8.append h1 $ B8.pack "a5c8b5c8a630c84dc01f92d2e5af6aa41801457a"
      h3 = SHA256.hash $ B8.append h2 $ B8.pack "2a"
      h4 = SHA256.hash $ B8.append h3 $ B8.pack "bf6e23c6db99ed2d24b160e89a37c9cd183fb61afeca40c4bc378cf6e488bebe"
  putStrLn $ show $ B8.unpack $ B16.encode $ h4

  return ()

{-
exports.loadkey = function(id, pub, priv)
{
  if(typeof pub == "string") pub = new Buffer(pub,"base64");
  if(!Buffer.isBuffer(pub) || pub.length != 40) return "invalid public key";
  id.key = pub;
  id.public = new crypto.ecc.ECKey(crypto.ecc.ECCurves.secp160r1, Buffer.concat([new Buffer("04","hex"),id.key]), true);
  if(!id.public) return "public key load failed";

  if(priv)
  {
    if(typeof priv == "string") priv = new Buffer(priv,"base64");
    if(!Buffer.isBuffer(priv) || priv.length != 20) return "invalid private key";
    id.private = new crypto.ecc.ECKey(crypto.ecc.ECCurves.secp160r1, priv);
    if(!id.private) return "private key load failed";
  }
  return false;
}


-}

-- ---------------------------------------------------------------------

main = do
  g <- initRNG
  let ((pub,priv) ,g') = generate g (getCurveByName SEC_p160r1)
  let (PublicKey _ pubk) = pub
  let (PrivateKey _ privk) = priv
  putStrLn $ "pub=" ++ show pubk
  putStrLn $ "priv=" ++ show privk

  -- let pubw64 = (fromIntegral pubk) :: Word64
  let privw64 = (fromIntegral privk) :: Word64

  putStrLn $ "encode pubw64=" ++ show (encode $ pointTow8s pubk)

  let (Just p20) = i2ospOf 20 privk

  putStrLn $ "encode ppriv=" ++ show (encode p20)

  let (Right bs) = decode $ B8.pack "o0UL/D6qQ+dcSX7hCoyMjLDYeA6dNScZ+YY/fcX4fyCtsSO2u9L5Lg=="
  putStrLn $ "decode pub len=" ++ show (length $ B.unpack bs)

  -- putStrLn $ "decode pub=" ++ show (decode $ B8.pack "o0UL/D6qQ+dcSX7hCoyMjLDYeA6dNScZ+YY/fcX4fyCtsSO2u9L5Lg==")
  -- putStrLn $ "decode priv=" ++ show (decode $ B8.pack "iollyIcHaGeD/JpUNn/7ef1QAzE=")

{-
example id.json

{"1a":"o0UL/D6qQ+dcSX7hCoyMjLDYeA6dNScZ+YY/fcX4fyCtsSO2u9L5Lg==",
 "1a_secret":"iollyIcHaGeD/JpUNn/7ef1QAzE="}
-}




-- ----------------------------------------------------------------------

initRNG :: IO SystemRNG
initRNG = do
  pool <- createEntropyPool
  return $ cprgCreate pool

-- ---------------------------------------------------------------------

pointTow8s :: Point -> B.ByteString
pointTow8s PointO = B.empty
pointTow8s (Point i1 i2)= B.append i1_20 i2_20
  where
    (Just i1_20) = i2ospOf 20 i1
    (Just i2_20) = i2ospOf 20 i2


