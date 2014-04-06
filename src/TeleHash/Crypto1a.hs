module TeleHash.Crypto1a
  (
    cset_1a
  --  init
  , crypt_keygen_1a
  , crypt_loadkey_1a
  , crypt_openize_1a
  , crypt_openline_1a
  , crypt_lineize_1a
  ) where

-- | Implement Cypher Suite 1a for TeleHash, as per
-- https://github.com/telehash/telehash.org/blob/master/cs/1a.md

{-

    SHA1 - well trusted and performs well on small devices
    ECC secp160r1 - small key sizes, balance of strong crypto (~1024bit) and still supportable with low cpu
    HMAC-SHA1 - common implementations available for embedded environments
    AES-128-CTR - low impact cipher, many implementations including hardware ones

-}


import Control.Exception
import Crypto.MAC.HMAC
import Crypto.PubKey.DH
import Crypto.PubKey.ECC.ECDSA
import Crypto.PubKey.ECC.Generate
import Crypto.Random
import Crypto.Types.PubKey.ECC
import Data.ByteString.Base64
import Data.List
import Data.Maybe
import Data.Word
import Crypto.Number.Serialize
import Crypto.Cipher.AES
import TeleHash.Convert
import TeleHash.Packet
import TeleHash.Utils
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
-- import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Binary as Binary


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

test_keygen :: IO (BC.ByteString,BC.ByteString)
test_keygen = do
  rng <- initRng
  let ((pub,priv),_) = crypt_keygen_1a rng
  return (pub,priv)

initRng :: IO SystemRNG
initRng = do
  pool <- createEntropyPool
  return $ cprgCreate pool

-- ---------------------------------------------------------------------

crypt_loadkey_1a :: String -> Maybe String -> TeleHash (Maybe HashCrypto)
crypt_loadkey_1a pub mpriv = do
  -- base64 decode the public key
  let mbs = decode $ BC.pack pub
  case mbs of
    Left _err -> do
      logT $ "invalid public key b64decode failed:" ++ pub
      return Nothing
    Right bs -> do
      if BC.length bs /= 40
        then do logT $ "invalid public key wrong len:" ++ pub
                return Nothing
        else do
          -- convert the ByteString into a pair of Integer
          let (b1,b2) = B.splitAt 20 bs
              i1 = os2ip b1
              i2 = os2ip b2
          -- create the public key
              pubkey = PublicKey curve (Point i1 i2)
              privkey = case mpriv of
                Nothing -> Nothing
                Just priv -> pk
                  where
                    mbsp = decode $ BC.pack priv
                    pk = case mbsp of
                      Left _err -> Nothing
                      Right bsp -> Just $ Private1a privkey
                        where
                          i = os2ip bsp
                          privkey = PrivateKey curve i

              hexName = mkHashFromBS bs
              newParts = [("1a",unHash hexName)]
              hc = HC { hcHashName = parts2hn newParts
                      , hcHexName = hexName
                      , hcParts = newParts
                      , hcCsid = "1a"
                      , hcKey = pub
                      , hcPublic = Public1a pubkey
                      , hcPrivate = privkey
                      }

          logT $ "crypt_loadkey_1a:parts=" ++ show (hcParts $ hc)
          return $ Just hc

mkHashFromBS :: BC.ByteString -> Hash
mkHashFromBS bs =
  let
    -- digest = SHA.sha1 $ BL.fromChunks [bs]
    digest = SHA1.hash bs
  in
   Hash (show digest)

mkHashFromB64 :: String -> Hash
mkHashFromB64 str =
  let
    -- digest = SHA.sha1 $ BL.fromChunks [BC.pack str]
    digest = SHA1.hash $ BC.pack str
  in
   -- B64.encode $ BL.unpack $ SHA.bytestringDigest digest
   Hash (show digest)

-- ---------------------------------------------------------------------

crypt_openize_1a :: HashContainer -> Telex -> TeleHash (Maybe Telex)
crypt_openize_1a = assert False undefined

{-
  use

getShared :: Params -> PrivateNumber -> PublicNumber -> SharedKey
    generate a shared key using our private number and the other party public number 
-}

{-
exports.openize = function(id, to, inner)
{
if(!to.ecc) to.ecc = new crypto.ecc.ECKey(crypto.ecc.ECCurves.secp160r1);
  var eccpub = to.ecc.PublicKey.slice(1);

  // get the shared secret to create the iv+key for the open aes
  var secret = to.ecc.deriveSharedSecret(to.public);
  var key = secret.slice(0,16);
  var iv = new Buffer("00000000000000000000000000000001","hex");

  // encrypt the inner
  var body = self.pencode(inner,id.cs["1a"].key);
  var cbody = crypto.aes(true, key, iv, body);

  // prepend the line public key and hmac it
  var secret = id.cs["1a"].private.deriveSharedSecret(to.public);
  var macd = Buffer.concat([eccpub,cbody]);
  var hmac = crypto.createHmac('sha1', secret).update(macd).digest();

  // create final body
  var body = Buffer.concat([hmac,macd]);
  return self.pencode(0x1a, body);
},
-}
-- ---------------------------------------------------------------------

{-
 Line

Line secrets are generated by using ECDH with both line-keys,
performing a SHA1 with the secret and line ids from the inner packet
and using the first 16 bytes as the key:

    line encryption key: SHA1(secret, my-line-id, their-line-id)
    line decryption key: SHA1(secret, their-line-id, my-line-id)

Line packets are designed to be very lightweight with minimum overhead
for use on networks such as 802.15.4 where there is a very low MTU.
the BODY is binary and defined as:

    HMAC - 4 bytes, the first part of the HMAC
    IV - 4 bytes
    CHANNEL CIPHERTEXT - the AES-128-CTR encrypted channel packet

Using the correct line key the HMAC can be verified/created, and then
using that key with the IV the channel packet can be
encrypted/decrypted.

-}
crypt_lineize_1a :: HashContainer -> Telex -> (HashContainer,Telex)
crypt_lineize_1a to packet = r
  where
    -- TODO: look at using ByteString.Builder for performance, and
    --       everything needs to be Data.ByteString.Char8

    -- iv is a 16 byte value, made up of 12 zero bytes followed by a
    -- uint32 counter value
    ivPrefix = BL.pack $ take 12 (repeat (0::Word8))
    iv = lbsTocbs $ BL.append ivPrefix (Binary.encode (hLineIV to))

    body = lbsTocbs $ Binary.encode (tPacket packet)
    ctx = initAES (gfromJust "crypt_lineize_1a" (hEncKey to))
    cbody = encryptCTR ctx iv body

    -- hmac :: (ByteString -> ByteString) -> Int -> ByteString -> ByteString -> ByteString
    -- hmac f blockSize secret msg
    h = BC.take 4 $ hmac SHA1.hash 16 (gfromJust "crypt_lineize_1a.1" $ hEncKey to) (BC.append iv cbody)

    final = foldl' BC.append BC.empty [(gfromJust "crypt_lineize_1a.2" $ hLineIn to),h,iv,cbody]
    fc = packet { tPacket = Just (Packet HeadEmpty final) }

    r = (to,fc)

{-
exports.lineize = function(to, packet)
{
// now encrypt the packet
  var iv = new Buffer(4);
  iv.writeUInt32LE(to.lineIV++,0);
  var ivz = new Buffer(12);
  ivz.fill(0);
  var cbody = crypto.aes(true, to.encKey, Buffer.concat([ivz,iv]), self.pencode(packet.js,packet.body));

  // prepend the IV and hmac it
  var mac = crypto.createHmac('sha1', to.encKey).update(Buffer.concat([iv,cbody])).digest()

  // create final body
  var body = Buffer.concat([to.lineInB,mac.slice(0,4),iv,cbody]);

  return self.pencode(null, body);
},


-}

{-
packet_t crypt_lineize_1a(crypt_t c, packet_t p)
{
  packet_t line;
  aes_context ctx;
  unsigned char iv[16], block[16], hmac[HMAC_SHA1_BYTES];
  size_t off = 0;
  crypt_1a_t cs = (crypt_1a_t)c->cs;

  line = packet_chain(p);
  packet_body(line,NULL,16+4+4+packet_len(p));
  memcpy(line->body,c->lineIn,16);
  memcpy(line->body+16+4,&(cs->seq),4);
  memset(iv,0,16);
  memcpy(iv+12,&(cs->seq),4);
  cs->seq++;

  aes_setkey_enc(&ctx,cs->keyOut,16);
  aes_crypt_ctr(&ctx,packet_len(p),&off,iv,block,packet_raw(p),line->body+16+4+4);

  hmac_sha1(hmac,cs->keyOut,16,line->body+16+4,4+packet_len(p));
  memcpy(line->body+16,hmac,4);

  return line;
}

-}

-- ---------------------------------------------------------------------

-- |set up the line enc/dec keys
crypt_openline_1a :: HashContainer -> HashContainer -> HashContainer
crypt_openline_1a from to = from'
  where
    -- sha1 
    -- ecdhe -- shared secret
    from' = assert False undefined

{-
// set up the line enc/dec keys
exports.openline = function(from, open)
{
  from.lineIV = 0;
  from.lineInB = new Buffer(from.lineIn, "hex");
  var ecdhe = from.ecc.deriveSharedSecret(open.linepub);
  from.encKey = crypto.createHash("sha1")
    .update(ecdhe)
    .update(new Buffer(from.lineOut, "hex"))
    .update(from.lineInB)
    .digest().slice(0,16);
  from.decKey = crypto.createHash("sha1")
    .update(ecdhe)
    .update(from.lineInB)
    .update(new Buffer(from.lineOut, "hex"))
    .digest().slice(0,16);
  return true;
},
-}
{-
// C version
// makes sure all the crypto line state is set up, and creates line keys if exist
int crypt_line_1a(crypt_t c, packet_t inner)
{
  unsigned char line_public[ECC_BYTES*2], secret[ECC_BYTES], input[ECC_BYTES+16+16], hash[20];
  char *hecc;
  crypt_1a_t cs;
  
  cs = (crypt_1a_t)c->cs;
  hecc = packet_get_str(inner,"ecc"); // it's where we stashed it
  if(!hecc || strlen(hecc) != ECC_BYTES*4) return 1;

  // do the diffie hellman
  util_unhex((unsigned char*)hecc,ECC_BYTES*4,line_public);
  if(!ecdh_shared_secret(line_public, cs->line_private, secret)) return 1;

  // make line keys!
  memcpy(input,secret,ECC_BYTES);
  memcpy(input+ECC_BYTES,c->lineOut,16);
  memcpy(input+ECC_BYTES+16,c->lineIn,16);
  sha1(input,ECC_BYTES+16+16,hash);
  memcpy(cs->keyOut,hash,16);

  memcpy(input+ECC_BYTES,c->lineIn,16);
  memcpy(input+ECC_BYTES+16,c->lineOut,16);
  sha1(input,ECC_BYTES+16+16,hash);
  memcpy(cs->keyIn,hash,16);

  return 0;
}
-}



-- ---------------------------------------------------------------------

testhash :: IO ()
testhash = do
  let b64 = "o0UL/D6qQ+dcSX7hCoyMjLDYeA6dNScZ+YY/fcX4fyCtsSO2u9L5Lg=="
  let Right bs = decode $ BC.pack b64
  -- let digest = SHA.sha1 $ BL.fromChunks [bs]
  let digest = SHA1.hash bs
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
    ctx    = foldl' SHA256.update iCtx (map BC.pack
                     [ "1a", "a5c8b5c8a630c84dc01f92d2e5af6aa41801457a",
                       "2a", "bf6e23c6db99ed2d24b160e89a37c9cd183fb61afeca40c4bc378cf6e488bebe"
                     ])
    iCtx   = SHA256.init
  putStrLn $ show $ BC.unpack $ B16.encode digest'


  let c0 = SHA256.init
      c1 = SHA256.update c0 $ BC.pack "1a"
      c2 = SHA256.update c1 $ BC.pack "a5c8b5c8a630c84dc01f92d2e5af6aa41801457a"
      c3 = SHA256.update c2 $ BC.pack "2a"
      c4 = SHA256.update c3 $ BC.pack "bf6e23c6db99ed2d24b160e89a37c9cd183fb61afeca40c4bc378cf6e488bebe"
  putStrLn $ show $ BC.unpack $ B16.encode $ SHA256.finalize c4

  let h1 = SHA256.hash $ BC.pack "1a"
      h2 = SHA256.hash $ BC.append h1 $ BC.pack "a5c8b5c8a630c84dc01f92d2e5af6aa41801457a"
      h3 = SHA256.hash $ BC.append h2 $ BC.pack "2a"
      h4 = SHA256.hash $ BC.append h3 $ BC.pack "bf6e23c6db99ed2d24b160e89a37c9cd183fb61afeca40c4bc378cf6e488bebe"
  putStrLn $ show $ BC.unpack $ B16.encode $ h4

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

  let (Right bs) = decode $ BC.pack "o0UL/D6qQ+dcSX7hCoyMjLDYeA6dNScZ+YY/fcX4fyCtsSO2u9L5Lg=="
  putStrLn $ "decode pub len=" ++ show (length $ B.unpack bs)

  -- putStrLn $ "decode pub=" ++ show (decode $ BC.pack "o0UL/D6qQ+dcSX7hCoyMjLDYeA6dNScZ+YY/fcX4fyCtsSO2u9L5Lg==")
  -- putStrLn $ "decode priv=" ++ show (decode $ BC.pack "iollyIcHaGeD/JpUNn/7ef1QAzE=")

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


