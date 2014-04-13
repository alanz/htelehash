{-# LANGUAGE OverloadedStrings #-}
module TeleHash.Crypto1a
  (
    cset_1a
  --  init
  , crypt_keygen_1a
  , crypt_loadkey_1a
  , crypt_openize_1a
  , crypt_deopenize_1a
  , crypt_openline_1a
  , crypt_lineize_1a

  , mkHashFromB64
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
import Control.Monad.State
import Crypto.Cipher.AES
import Crypto.MAC.HMAC
import Crypto.Number.Serialize
import Crypto.PubKey.ECC.ECDSA
import Crypto.PubKey.ECC.Generate
import Crypto.Random
import Data.ByteString.Base64
import Data.List
import Data.Maybe
import Data.Word
import System.Time
import TeleHash.Convert
import TeleHash.Packet
import TeleHash.Utils

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.PubKey.DH as DH
import qualified Crypto.PubKey.ECC.Prim as ECC
import qualified Crypto.Types.PubKey.DH as DH
import qualified Crypto.Types.PubKey.ECC as ECC
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text

curve = ECC.getCurveByName ECC.SEC_p160r1

-- ---------------------------------------------------------------------

cset_1a = CS
  { csLoadkey = crypt_loadkey_1a
  , csOpenize = crypt_openize_1a
  , csDeopenize = crypt_deopenize_1a
  , csOpenLine = crypt_openline_1a
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
  logT $ "crypt_loadkey_1a for:" ++ show (pub,mpriv)
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
          let
            -- create the public key
              pubkey = PublicKey curve (bsToPoint bs)
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
mkHashFromB64 str = r
  where
    Right bs = decode $ BC.pack str
    r = Hash (BC.unpack $ B16.encode $ SHA1.hash $ bs)

-- ---------------------------------------------------------------------

crypt_openize_1a :: HashContainer -> Telex -> TeleHash LinePacket
crypt_openize_1a to inner = do

{-
From https://en.wikipedia.org/wiki/Elliptic_curve_Diffie%E2%80%93Hellman

Key establishment protocol

The following example will illustrate how a key establishment is made.
Suppose Alice wants to establish a shared key with Bob, but the only
channel available for them may be eavesdropped by a third party.
Initially, the domain parameters (that is, (p,a,b,G,n,h) in the prime
case or (m,f(x),a,b,G,n,h) in the binary case) must be agreed upon.
Also, each party must have a key pair suitable for elliptic curve
cryptography, consisting of a private key d (a randomly selected
integer in the interval [1, n-1]) and a public key Q (where Q = d G,
that is, the result of adding G together d times).

Let Alice's key pair be (d_A, Q_A)
  and Bob's key pair be (d_B, Q_B).

Each party must have the other party's public key (an exchange must
occur).

Alice computes (x_k, y_k) = d_A Q_B.

  Bob computes (x_k, y_k) = d_B Q_A.

The shared secret is x_k (the x coordinate of the point).

Most standardized protocols based on ECDH derived a symmetric key
from x_k using some hash-based key derivation function.

Note: 1. the domain parameters are the agreed curve, i.e. SEC_p160r1
      2. d is a scalar, Q a point
      3. multiplication is as defined for ECC
-}


  -- get the shared secret to create the iv+key for the open aes

    -- We user the line public id, and our ephemeral line private id
    -- our credentials are in (swIdCrypto sw)
    -- our line ephemeral credentials are in (hEcc to)
    -- the line public id is in (hSelf to)
  sw <- get
  (linePub,linePriv) <- case (hEcc to) of
    Nothing -> do
      let ((pub,priv) ,g') = generate (swRNG sw) curve
      put $ sw { swRNG = g' } -- must come before next line, else update lost
      logT $ "crypt_openize_1a:putHN for " ++ show (hHashName to)
      putHN $ to { hEcc = Just (Public1a pub,Private1a priv) }
      let (PublicKey _ lpub) = pub
      let (PrivateKey _ lp) = priv
      return (lpub,lp)
    Just (Public1a (PublicKey _ lpub), Private1a (PrivateKey _ lp)) -> return (lpub,lp)

  let toCrypto = gfromJust "crypt_openize_1a" (hSelf to)
      (Public1a (PublicKey _ toPublic)) = hcPublic toCrypto

  let (ECC.Point sharedX _Y) = ECC.pointMul curve linePriv toPublic

  logT $ "crypt_openize_1a:(sharedX)=" ++ show (sharedX)

  --  encrypt the inner
  let ourCrypto = gfromJust "crypt_openize_1a" (swIdCrypto sw)
      Public1a (PublicKey _ ourPub) = hcPublic ourCrypto
      Private1a (PrivateKey _ ourPriv) = gfromJust "crypt_openize_1a.2" $ hcPrivate ourCrypto

      (TOD atSeconds _ ) = (gfromJust "crypt_openize_1a.at" (tAt inner))

      from = (gfromJust "crypt_openize_1a.from" (tFrom inner))
      fromJS = "{" ++ (intercalate "," $ map (\(k,v) -> show k ++ ":" ++ show v) from) ++ "}"
{-

crypt_openize_1a:js=
{"at":"1397156337"
,"to":"89a4cbc6c27eb913c1bcaf06bac2d8b872c7cbef626b35b6d7eaf993590d37de"
,"from":{"1a":"o0UL/D6qQ+dcSX7hCoyMjLDYeA6dNScZ+YY/fcX4fyCtsSO2u9L5Lg=="}"
,"line":"7d544053d7ee09ea9ba42ac96c508edc"}

-}

      -- Construct the JSON for the inner packet
      js =("{\"at\":" ++ (show atSeconds) ++ "," ++
           "\"to\":"    ++ (show $ unHN (gfromJust "crypt_openize_1a.to" (tToHash inner))) ++ "," ++
           "\"from\":"  ++ fromJS ++ "," ++
           "\"line\":"  ++ (show (gfromJust "crypt_openize_1a.line" (tLine inner))) ++
           "}")

  logT $ "crypt_openize_1a:js=" ++ js

  logT $ "crypt_openize_1a:ourPub hex=" ++ (show $ B16.encode $ pointTow8s ourPub)

  let innerPacket = Packet (HeadJson (cbsTolbs $ BC.pack js)) (Body $ pointTow8s ourPub)
      (LP innerPacketBody) = toLinePacket innerPacket
      body = lbsTocbs innerPacketBody

      (Just longkey) = i2ospOf 20 sharedX
      key = BC.take 16 longkey
      Just iv = i2ospOf 16 1
      cbody = encryptCTR (initAES key) iv body

  let (Packet h b) = innerPacket
  logT $ "crypt_openize_1a:body=" ++ show innerPacket
  logT $ "crypt_openize_1a:body.h=" ++ (show $ B16.encode $ lbsTocbs $ myencode h)
  -- logT $ "crypt_openize_1a:body.b=" ++ (show $ B16.encode $ lbsTocbs $ Binary.encode b)
  let (LP xx) = toLinePacket innerPacket
  logT $ "crypt_openize_1a:innerPacket=" ++ (show $ B16.encode $ lbsTocbs xx)

  -- prepend the line public key and hmac it

  -- var secret = id.cs["1a"].private.deriveSharedSecret(to.public);
  let (ECC.Point secretX _Y) = ECC.pointMul curve ourPriv toPublic
      Just secretMac = i2ospOf 20 secretX
      -- secretMac = BC.take 16 secretMacLong
      macd = BC.append (pointTow8s linePub) cbody
      hmacVal = hmac SHA1.hash 64 secretMac macd

  logT $ "crypt_openize_1a:hmac=" ++ (show $ B16.encode hmacVal)

  -- create final body
  let bodyFinal = BC.append hmacVal macd

  return $ toLinePacket $ Packet (HeadByte 0x1a) (Body bodyFinal)



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


{-
// create a new open packet
packet_t crypt_openize_1a(crypt_t self, crypt_t c, packet_t inner)
{
  unsigned char secret[ECC_BYTES], iv[16], block[16];
  packet_t open;
  aes_context ctx;
  int inner_len;
  size_t off = 0;
  crypt_1a_t cs = (crypt_1a_t)c->cs, scs = (crypt_1a_t)self->cs;

  open = packet_chain(inner);
  packet_json(open,&(self->csid),1);
  inner_len = packet_len(inner);
  packet_body(open,NULL,20+40+inner_len);

  // copy in the line public key
  memcpy(open->body+20, cs->line_public, 40);

  // get the shared secret to create the iv+key for the open aes
  if(!ecdh_shared_secret(cs->id_public, cs->line_private, secret)) return packet_free(open);
  memset(iv,0,16);
  iv[15] = 1;

  // encrypt the inner
  aes_setkey_enc(&ctx,secret,128);
  aes_crypt_ctr(&ctx,inner_len,&off,iv,block,packet_raw(inner),open->body+20+40);

  // generate secret for hmac
  if(!ecdh_shared_secret(cs->id_public, scs->id_private, secret)) return packet_free(open);
  sha1_hmac(secret,ECC_BYTES,open->body+20,40+inner_len,open->body);

  return open;
}

-}

-- ---------------------------------------------------------------------

crypt_deopenize_1a :: Packet -> TeleHash DeOpenizeResult
crypt_deopenize_1a open = do
  if BC.length (unBody $ paBody open) <= 60
    then do
      logT $ "crypt_deopenize_1a:body length too short:" ++ show (BC.length $ unBody $ paBody open)
      return DeOpenizeVerifyFail
    else do
      sw <- get
      let
        mac1  = B16.encode $ BC.take 20 (unBody $ paBody open)
        pubBs = BC.take 40 $ BC.drop 20 (unBody $ paBody open)
        cbody = BC.drop 60 (unBody $ paBody open)


        linePubPoint = (bsToPoint pubBs)
        linePub = PublicKey curve linePubPoint

      logT $ "crypt_deopenize_1a:mac1=" ++ show (mac1)
      logT $ "crypt_deopenize_1a:pubBs=" ++ show (B16.encode pubBs)
      logT $ "crypt_deopenize_1a:cbody=" ++ show (B16.encode cbody)

      logT $ "crypt_deopenize_1a:pubBs b64=" ++ show (encode pubBs)


      -- derive shared secret
      -- var secret = id.cs["1a"].private.deriveSharedSecret(ret.linepub);
      let ourCrypto = gfromJust "crypt_deopenize_1a" (swIdCrypto sw)
          Public1a (PublicKey _ pub) = hcPublic ourCrypto
          Private1a (PrivateKey _ ourPriv) = gfromJust "crypt_deopenize_1a.2" $ hcPrivate ourCrypto

      let (ECC.Point sharedX _Y) = ECC.pointMul curve ourPriv linePubPoint
          (Just longkey) = i2ospOf 20 sharedX
          key = BC.take 16 longkey
          Just iv = i2ospOf 16 1

      logT $ "crypt_deopenize_1a:iv=" ++ show (B16.encode iv)

      logT $ "crypt_deopenize_1a:(sharedX,key)=" ++ show (sharedX,key)

      -- aes-128 decipher the inner
      let body = decryptCTR (initAES key) iv cbody
          Just inner = fromLinePacket (LP $ cbsTolbs body)

      logT $ "crypt_deopenize_1a:inner=" ++ show inner
      let HeadJson js = paHead inner
      logT $ "crypt_deopenize_1a:inner json=" ++ show (B16.encode $ lbsTocbs js)

      -- verify+load inner key info
      let Body ekey = paBody inner
          epub = PublicKey curve (bsToPoint $ ekey)

          Just json@(Aeson.Object jsHashMap) = Aeson.decode js :: Maybe Aeson.Value

      case HM.lookup "from" jsHashMap of
        Nothing -> do
          logT $ "crypt_deopenize_1a:missing inner.js.from"
          return DeOpenizeVerifyFail
        Just (Aeson.Object fromHm) -> do
          case HM.lookup "1a" fromHm of
            Nothing -> do
              logT $ "crypt_deopenize_1a:missing inner.js.from.1a"
              return DeOpenizeVerifyFail
            Just (Aeson.String from1aVal) -> do
              -- if(crypto.createHash("sha1").update(inner.body).digest("hex") != inner.js.from["1a"]) return ret;
              let calcDigest = B16.encode $ SHA1.hash ekey
                  from1aValStr = BC.pack $ Text.unpack from1aVal
              if calcDigest /= from1aValStr
                then do
                  logT $ "crypt_deopenize_1a:pub key digest does not match key:" ++ show (calcDigest,from1aVal)
                  return DeOpenizeVerifyFail
                else do
                  -- verify the hmac
                  -- var secret = id.cs["1a"].private.deriveSharedSecret(epub);
                  let (PublicKey _ epubPoint) = epub
                      (ECC.Point secretX _Y) = ECC.pointMul curve ourPriv epubPoint
                      Just secretMac = i2ospOf 20 secretX
                      hmacVal = hmac SHA1.hash 64 secretMac (BC.drop 20 (unBody $ paBody open))
                      mac2 = B16.encode hmacVal
                     -- var mac2 = crypto.createHmac('sha1', secret).update(open.body.slice(20)).digest("hex");
                     -- if(mac2 != mac1) return ret;
                  if mac1 /= mac2
                    then do
                      logT $ "crypt_deopenize_1a:mac1 /= mac2:" ++ show (mac1,mac2)
                      return DeOpenizeVerifyFail
                    else do
                      --  all good, cache+return
                      -- ret.verify = true;
                      -- ret.js = inner.js;
                      -- return ret;
                      let ret = DeOpenize
                           { doLinePub = Public1a linePub
                           , doKey = unBody $ paBody inner
                           , doJs = json
                           , doCsid = "1a"
                           }
                      return ret

      -- logT $ "crypt_deopenize_1a:inner.js decoded=" ++ show json

      -- return $ assert False undefined


{-

exports.deopenize = function(id, open)
{
  var ret = {verify:false};
  if(!open.body) return ret;

  var mac1 = open.body.slice(0,20).toString("hex");
  var pub = open.body.slice(20,60);
  var cbody = open.body.slice(60);

  try{
    ret.linepub = new crypto.ecc.ECKey(crypto.ecc.ECCurves.secp160r1, Buffer.concat([new Buffer("04","hex"),pub]), true);
  }catch(E){
    console.log("ecc err",E);
  }
  if(!ret.linepub) return ret;

  var secret = id.cs["1a"].private.deriveSharedSecret(ret.linepub);
  var key = secret.slice(0,16);
  var iv = new Buffer("00000000000000000000000000000001","hex");

  // aes-128 decipher the inner
  var body = crypto.aes(false, key, iv, cbody);
  var inner = self.pdecode(body);
  if(!inner) return ret;

  // verify+load inner key info
  var epub = new crypto.ecc.ECKey(crypto.ecc.ECCurves.secp160r1, Buffer.concat([new Buffer("04","hex"),inner.body]), true);
  if(!epub) return ret;
  ret.key = inner.body;
  if(typeof inner.js.from != "object" || !inner.js.from["1a"]) return ret;
  if(crypto.createHash("sha1").update(inner.body).digest("hex") != inner.js.from["1a"]) return ret;

  // verify the hmac
  var secret = id.cs["1a"].private.deriveSharedSecret(epub);
  var mac2 = crypto.createHmac('sha1', secret).update(open.body.slice(20)).digest("hex");
  if(mac2 != mac1) return ret;

  // all good, cache+return
  ret.verify = true;
  ret.js = inner.js;
  return ret;
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
crypt_lineize_1a :: HashContainer -> Telex -> (HashContainer,LinePacket)
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
    fc = packet { tPacket = Just (Packet HeadEmpty (Body final)) }

    r = (to,assert False undefined)

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
crypt_openline_1a :: HashContainer -> DeOpenizeResult -> TeleHash ()
crypt_openline_1a from to = do
  case hEcc from of
    Nothing -> error $ "crypt_openline_1a, expecting hEcc to be populated:" ++ show (hHashName from)
    Just (Public1a eccPub,Private1a (PrivateKey _ eccPriv)) -> do
      let Public1a (PublicKey _ linePub) = doLinePub to
          (ECC.Point secretX _Y) = ECC.pointMul curve eccPriv linePub
          Just ecdhe = i2ospOf 20 secretX
          (lineOutB,_) = B16.decode (BC.pack $ hLineOut from)
          lineInB = gfromJust "crypt_openline_1a" (hLineIn from)

          encKeyCtx = SHA1.updates SHA1.init [ecdhe,lineOutB,lineInB]
          encKey = B16.encode $ BC.take 16 (SHA1.finalize encKeyCtx)

          decKeyCtx = SHA1.updates SHA1.init [ecdhe,lineInB,lineOutB]
          decKey = B16.encode $ BC.take 16 (SHA1.finalize decKeyCtx)


      logT $ "crypt_openline_1a:(encKey,decKey)=" ++ show (encKey,decKey)
      putHN $ from { hLineIV = 0
                   , hEncKey = Just encKey
                   , hDecKey = Just decKey
                   }

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
  let ((pub,priv) ,g') = generate g (ECC.getCurveByName ECC.SEC_p160r1)
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

pointTow8s :: ECC.Point -> B.ByteString
pointTow8s ECC.PointO = B.empty
pointTow8s (ECC.Point i1 i2)= B.append i1_20 i2_20
  where
    (Just i1_20) = i2ospOf 20 i1
    (Just i2_20) = i2ospOf 20 i2

-- ---------------------------------------------------------------------

bsToPoint :: BC.ByteString -> ECC.Point
bsToPoint bs = (ECC.Point i1 i2)
  where
    (b1,b2) = B.splitAt 20 bs
    i1 = os2ip b1
    i2 = os2ip b2


-- ---------------------------------------------------------------------
{-
From https://en.wikipedia.org/wiki/Hash-based_message_authentication_code

HMAC_MD5("", "") = 0x74e6f7298a9c2d168935f58c001bad88
HMAC_SHA1("", "") = 0xfbdb1d1b18aa6c08324b7d64b71fb76370690e1d
HMAC_SHA256("", "") = 0xb613679a0814d9ec772f95d778c35fc5ff1697c493715653c6c712144292c5ad

Here are some non-empty HMAC values, assuming 8-bit ASCII or UTF-8 encoding:

HMAC_MD5("key", "The quick brown fox jumps over the lazy dog") = 0x80070713463e7749b90c2dc24911e275
HMAC_SHA1("key", "The quick brown fox jumps over the lazy dog") = 0xde7c9b85b8b78aa6bc8a7a36f70a90701c9db4d9
HMAC_SHA256("key", "The quick brown fox jumps over the lazy dog") = 0xf7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8
-}

t1 = B16.encode $ hmac SHA1.hash 64 BC.empty BC.empty
t2 = B16.encode $ hmac SHA1.hash 64 "key" "The quick brown fox jumps over the lazy dog"





-- ---------------------------------------------------------------------
{-

Captured from first packet going on a ping


alanz@alanz-laptop:~/mysrc/github/alanz/telehash-c$ ./bin/ping 
loaded hashname 7ecf6a5884d483fde2f6a027e33e6e1756efdb70925557c3e3f776b35329aef5
cs->id_publichex dump
 0x89, 0xd4, 0xf4, 0x56, 0x69, 0x84, 0x99, 0x27, 0x43, 0x17, 0x57, 0x8c, 0xad, 0xa8, 0xc8, 0x54,
 0x73, 0xf4, 0x5d, 0x9f, 0xfc, 0x51, 0xd8, 0x1e, 0x6a, 0xcd, 0x59, 0xc7, 0x2d, 0x2f, 0x6b, 0x9b,
 0x43, 0xfc, 0xa3, 0xf7, 0xf7, 0x74, 0x62, 0xdf,
hex dump done
-}

t_cs_id_public :: [Word8]
t_cs_id_public =
 [
 0x89, 0xd4, 0xf4, 0x56, 0x69, 0x84, 0x99, 0x27, 0x43, 0x17, 0x57, 0x8c, 0xad, 0xa8, 0xc8, 0x54,
 0x73, 0xf4, 0x5d, 0x9f, 0xfc, 0x51, 0xd8, 0x1e, 0x6a, 0xcd, 0x59, 0xc7, 0x2d, 0x2f, 0x6b, 0x9b,
 0x43, 0xfc, 0xa3, 0xf7, 0xf7, 0x74, 0x62, 0xdf
 ]

{-
cs->line_privatehex dump
 0xa3, 0x96, 0x6a, 0x77, 0x7c, 0xf4, 0x54, 0x2d, 0xe6, 0xfc, 0xb4, 0x1e, 0xc2, 0x03, 0xf7, 0xa3,
 0xe4, 0x25, 0x67, 0xe9,
hex dump done
-}
t_cs_line_private :: [Word8]
t_cs_line_private =
 [
 0xa3, 0x96, 0x6a, 0x77, 0x7c, 0xf4, 0x54, 0x2d, 0xe6, 0xfc, 0xb4, 0x1e, 0xc2, 0x03, 0xf7, 0xa3,
 0xe4, 0x25, 0x67, 0xe9
 ]

{-
scs->line_privatehex dump
 0x0e, 0x58, 0x79, 0xc8, 0x39, 0xb1, 0x26, 0x1f, 0x1f, 0xd2, 0x3c, 0xfb, 0x41, 0xf9, 0x2a, 0xb7,
 0xc5, 0xcd, 0xda, 0x60,
hex dump done
-}
-- scs is self
t_scs_line_private :: [Word8]
t_scs_line_private =
 [
 0x0e, 0x58, 0x79, 0xc8, 0x39, 0xb1, 0x26, 0x1f, 0x1f, 0xd2, 0x3c, 0xfb, 0x41, 0xf9, 0x2a, 0xb7,
 0xc5, 0xcd, 0xda, 0x60
 ]

{-
secrethex dump
 0x01, 0x90, 0xb0, 0xb9, 0x0e, 0xdd, 0x29, 0x9a, 0x0b, 0xa6, 0xfd, 0xa6, 0x72, 0x6d, 0x87, 0x04,
-}
t_secret :: [Word8]
t_secret =
 [
 0x01, 0x90, 0xb0, 0xb9, 0x0e, 0xdd, 0x29, 0x9a, 0x0b, 0xa6, 0xfd, 0xa6, 0x72, 0x6d, 0x87, 0x04
 ]

{-
hex dump done
packet_raw(inner)hex dump
 0x00, 0xbc, 0x7b, 0x22, 0x74, 0x6f, 0x22, 0x3a, 0x22, 0x66, 0x35, 0x30, 0x66, 0x34, 0x32, 0x33,
 0x63, 0x65, 0x37, 0x66, 0x39, 0x34, 0x66, 0x65, 0x39, 0x38, 0x63, 0x64, 0x64, 0x30, 0x39, 0x32,
 0x36, 0x38, 0x63, 0x37, 0x65, 0x35, 0x37, 0x30, 0x30, 0x31, 0x61, 0x65, 0x64, 0x33, 0x30, 0x30,
 0x62, 0x32, 0x33, 0x30, 0x32, 0x30, 0x38, 0x34, 0x30, 0x61, 0x38, 0x34, 0x61, 0x38, 0x38, 0x31,
 0x63, 0x37, 0x36, 0x37, 0x33, 0x39, 0x34, 0x37, 0x31, 0x22, 0x2c, 0x22, 0x66, 0x72, 0x6f, 0x6d,
 0x22, 0x3a, 0x7b, 0x22, 0x31, 0x61, 0x22, 0x3a, 0x22, 0x37, 0x32, 0x39, 0x62, 0x32, 0x32, 0x35,
 0x32, 0x65, 0x63, 0x31, 0x37, 0x34, 0x30, 0x65, 0x36, 0x37, 0x34, 0x33, 0x37, 0x61, 0x35, 0x30,
 0x35, 0x31, 0x38, 0x61, 0x31, 0x63, 0x31, 0x30, 0x33, 0x31, 0x34, 0x64, 0x37, 0x36, 0x63, 0x33,
 0x61, 0x22, 0x7d, 0x2c, 0x22, 0x6c, 0x69, 0x6e, 0x65, 0x22, 0x3a, 0x22, 0x34, 0x65, 0x32, 0x33,
 0x37, 0x36, 0x31, 0x61, 0x32, 0x62, 0x66, 0x31, 0x61, 0x36, 0x38, 0x38, 0x34, 0x37, 0x35, 0x33,
 0x66, 0x31, 0x61, 0x38, 0x32, 0x64, 0x62, 0x66, 0x36, 0x64, 0x65, 0x34, 0x22, 0x2c, 0x22, 0x61,
 0x74, 0x22, 0x3a, 0x31, 0x33, 0x39, 0x37, 0x30, 0x36, 0x39, 0x36, 0x36, 0x31, 0x7d, 0xa3, 0x45,
 0x0b, 0xfc, 0x3e, 0xaa, 0x43, 0xe7, 0x5c, 0x49, 0x7e, 0xe1, 0x0a, 0x8c, 0x8c, 0x8c, 0xb0, 0xd8,
 0x78, 0x0e, 0x9d, 0x35, 0x27, 0x19, 0xf9, 0x86, 0x3f, 0x7d, 0xc5, 0xf8, 0x7f, 0x20, 0xad, 0xb1,
 0x23, 0xb6, 0xbb, 0xd2, 0xf9, 0x2e,
hex dump done
-}
t_inner :: [Word8]
t_inner =
 [
 0x00, 0xbc, 0x7b, 0x22, 0x74, 0x6f, 0x22, 0x3a, 0x22, 0x66, 0x35, 0x30, 0x66, 0x34, 0x32, 0x33,
 0x63, 0x65, 0x37, 0x66, 0x39, 0x34, 0x66, 0x65, 0x39, 0x38, 0x63, 0x64, 0x64, 0x30, 0x39, 0x32,
 0x36, 0x38, 0x63, 0x37, 0x65, 0x35, 0x37, 0x30, 0x30, 0x31, 0x61, 0x65, 0x64, 0x33, 0x30, 0x30,
 0x62, 0x32, 0x33, 0x30, 0x32, 0x30, 0x38, 0x34, 0x30, 0x61, 0x38, 0x34, 0x61, 0x38, 0x38, 0x31,
 0x63, 0x37, 0x36, 0x37, 0x33, 0x39, 0x34, 0x37, 0x31, 0x22, 0x2c, 0x22, 0x66, 0x72, 0x6f, 0x6d,
 0x22, 0x3a, 0x7b, 0x22, 0x31, 0x61, 0x22, 0x3a, 0x22, 0x37, 0x32, 0x39, 0x62, 0x32, 0x32, 0x35,
 0x32, 0x65, 0x63, 0x31, 0x37, 0x34, 0x30, 0x65, 0x36, 0x37, 0x34, 0x33, 0x37, 0x61, 0x35, 0x30,
 0x35, 0x31, 0x38, 0x61, 0x31, 0x63, 0x31, 0x30, 0x33, 0x31, 0x34, 0x64, 0x37, 0x36, 0x63, 0x33,
 0x61, 0x22, 0x7d, 0x2c, 0x22, 0x6c, 0x69, 0x6e, 0x65, 0x22, 0x3a, 0x22, 0x34, 0x65, 0x32, 0x33,
 0x37, 0x36, 0x31, 0x61, 0x32, 0x62, 0x66, 0x31, 0x61, 0x36, 0x38, 0x38, 0x34, 0x37, 0x35, 0x33,
 0x66, 0x31, 0x61, 0x38, 0x32, 0x64, 0x62, 0x66, 0x36, 0x64, 0x65, 0x34, 0x22, 0x2c, 0x22, 0x61,
 0x74, 0x22, 0x3a, 0x31, 0x33, 0x39, 0x37, 0x30, 0x36, 0x39, 0x36, 0x36, 0x31, 0x7d, 0xa3, 0x45,
 0x0b, 0xfc, 0x3e, 0xaa, 0x43, 0xe7, 0x5c, 0x49, 0x7e, 0xe1, 0x0a, 0x8c, 0x8c, 0x8c, 0xb0, 0xd8,
 0x78, 0x0e, 0x9d, 0x35, 0x27, 0x19, 0xf9, 0x86, 0x3f, 0x7d, 0xc5, 0xf8, 0x7f, 0x20, 0xad, 0xb1,
 0x23, 0xb6, 0xbb, 0xd2, 0xf9, 0x2e
 ]

{-

sending open packet 293 {"type":"ipv4","ip":"208.126.199.195","port":42424}
hex dump of packet out
 0x00, 0x01,
 0x1a,

 -- hmac 20 bytes
 0x1b, 0xb0, 0x92, 0x16, 0xc6, 0x00, 0xe7, 0x7c, 0xa2, 0x27, 0x26, 0x18, 0x33, 0x61, 0x8f, 0x54,
 0x0c, 0x40, 0xd4, 0x83,

 -- line ephemeral public key
 0x3d, 0x0c, 0xb3, 0x3b, 0xa2, 0xcd, 0x28, 0x9e, 0xdc, 0xee, 0x70, 0xd8, 0x1e, 0x5a, 0xab, 0x09,
 0x0b, 0x0c, 0xaa, 0xa8, 0x9c, 0x63, 0xaf, 0xbe, 0x93, 0x96, 0x8e, 0x83, 0xf5, 0x5f, 0x6d, 0x37,
 0x1a, 0x5b, 0x05, 0xaf, 0xd4, 0x01, 0x5d, 0xf0,

 -- encrypted inner packet

 0x12,
 0x59, 0xa8, 0xb2, 0xd7, 0x94, 0xef, 0x01, 0x9a, 0xc8, 0x3a, 0xee, 0x49, 0xc4, 0x93, 0xcb, 0x64,
 0xb8, 0x84, 0x04, 0x92, 0x11, 0x94, 0x44, 0x79, 0xa6, 0x08, 0x6b, 0x0d, 0x6f, 0xb8, 0xa6, 0xfc,
 0x99, 0x00, 0xe6, 0xca, 0xb4, 0x5f, 0x58, 0xb1, 0x12, 0x21, 0xe3, 0x57, 0xaa, 0x74, 0x53, 0x5d,
 0x66, 0x45, 0xbe, 0x44, 0xec, 0x75, 0x9e, 0x39, 0x8b, 0x6c, 0xe4, 0xa0, 0xd3, 0x6d, 0x41, 0xff,
 0xa8, 0x04, 0xb4, 0x96, 0x17, 0x26, 0xa9, 0xce, 0x9a, 0x5a, 0xf6, 0x35, 0x48, 0xf2, 0x52, 0xc2,
 0xb2, 0x57, 0xa4, 0xa8, 0xb6, 0xfe, 0xb4, 0x3a, 0x8f, 0xcb, 0x38, 0x17, 0x0e, 0x94, 0x83, 0xbe,
 0x90, 0x5d, 0x6e, 0xa7, 0x1c, 0xe1, 0xaa, 0x36, 0x39, 0x17, 0xfc, 0x09, 0x5f, 0x6f, 0xc7, 0x46,
 0x23, 0x36, 0xa9, 0xee, 0x6c, 0xef, 0x44, 0xab, 0xa5, 0x7c, 0xa3, 0xa5, 0xe0, 0xde, 0x9a, 0x28,
 0x21, 0xd4, 0x44, 0xe3, 0xd3, 0x48, 0x2f, 0x4e, 0xd5, 0x88, 0x58, 0x0e, 0xcf, 0xa9, 0x14, 0xf1,
 0xe0, 0xaf, 0xa0, 0x62, 0xa3, 0x6e, 0x9f, 0x91, 0x94, 0xb9, 0x7b, 0xaf, 0x4a, 0xd0, 0xbb, 0x80,
 0x4b, 0x30, 0x23, 0xd5, 0xf5, 0x15, 0x3d, 0x51, 0xc5, 0xef, 0x99, 0x79, 0x63, 0x30, 0xa5, 0xa5,
 0x76, 0x33, 0x51, 0x17, 0xb6, 0x20, 0x92, 0xe0, 0xce, 0x43, 0x4b, 0x64, 0xc4, 0x31, 0xec, 0x30,
 0xaf, 0x6c, 0x0b, 0x09, 0x6e, 0x91, 0xde, 0xb7, 0xdc, 0xed, 0x45, 0x83, 0x8d, 0x35, 0x97, 0xb4,
 0x68, 0x0d, 0xe8, 0x70, 0xd9, 0x0d, 0x67, 0x88, 0x93, 0x8e, 0x95, 0x69, 0x1d, 0xde, 0x9c, 0x7a,
 0xf0, 0xb6, 0x42, 0xda, 0x25,
hex dump of packet out done

-}

-- Note : line_public, line_private are the line ephemeral keys
--        id_public is the public key of the intended recipient of the openize

--   if(!ecdh_shared_secret(cs->id_public, cs->line_private, secret)) return packet_free(open);
test_gen_shared_secret = r
  where
    pbs = os2ip $ lbsTocbs $ BL.pack t_cs_line_private
    linePriv = pbs

    id_public = (bsToPoint $ lbsTocbs $ BL.pack t_cs_id_public)

    (ECC.Point sharedX _Y) = ECC.pointMul curve linePriv id_public

    (Just longkey) = i2ospOf 20 sharedX
    key = BC.take 16 longkey

    r = B16.encode key

expected_shared_secret = "0190b0b90edd299a0ba6fda6726d8704"


test_aes_CTR = r
  where
     body = lbsTocbs $ BL.pack t_inner

     -- (Just longkey) = i2ospOf 20 sharedX
     -- key = BC.take 16 longkey
     key = lbsTocbs $ BL.pack t_secret
     Just iv = i2ospOf 16 1
     cbody = encryptCTR (initAES key) iv body
     r = B16.encode cbody

{-
"
1259a8b2d794ef019ac83aee49c493cb
64b884049211944479a6086b0d6fb8a6
fc9900e6cab45f58b11221e357aa7453
5d6645be44ec759e398b6ce4a0d36d41
ffa804b4961726a9ce9a5af63548f252
c2b257a4a8b6feb43a8fcb38170e9483
be905d6ea71ce1aa363917fc095f6fc7
462336a9ee6cef44aba57ca3a5e0de9a
2821d444e3d3482f4ed588580ecfa914
f1e0afa062a36e9f9194b97baf4ad0bb
804b3023d5f5153d51c5ef99796330a5
a576335117b62092e0ce434b64c431ec
30af6c0b096e91deb7dced45838d3597
b4680de870d90d6788938e95691dde9c
7af0b642da25"
-}


innerString = lbsTocbs $ BL.pack t_inner
{-

"\NUL\188
{\"to\":\"f50f423ce7f94fe98cdd09268c7e57001aed300b23020840a84a881c76739471\"
,\"from\":{\"1a\":\"729b2252ec1740e67437a50518a1c10314d76c3a\"}
,\"line\":\"4e23761a2bf1a6884753f1a82dbf6de4\"
,\"at\":1397069661}
\163E\v\252>\170C\231\\I~\225\n\140\140\140\176\216x\SO\157\&5'\EM\249\134?}\197\248\DEL \173\177#\182\187\210\249."

-}


inner_key :: [Word8]
inner_key =
 [
 0xa3, 0x45, 0x0b, 0xfc, 0x3e, 0xaa, 0x43, 0xe7, 0x5c, 0x49, 0x7e, 0xe1, 0x0a, 0x8c, 0x8c, 0x8c, 0xb0, 0xd8,
 0x78, 0x0e, 0x9d, 0x35, 0x27, 0x19, 0xf9, 0x86, 0x3f, 0x7d, 0xc5, 0xf8, 0x7f, 0x20, 0xad, 0xb1,
 0x23, 0xb6, 0xbb, 0xd2, 0xf9, 0x2e
 ]

b64_inner_key = encode $ lbsTocbs $ BL.pack inner_key
{-
"o0UL/D6qQ+dcSX7hCoyMjLDYeA6dNScZ+YY/fcX4fyCtsSO2u9L5Lg=="

{"1a":"o0UL/D6qQ+dcSX7hCoyMjLDYeA6dNScZ+YY/fcX4fyCtsSO2u9L5Lg==","1a_secret":"iollyIcHaGeD/JpUNn/7ef1QAzE="}
-}

-- ---------------------------------------------------------------------

test_hmac = r
  where
    Right bs = decode $ BC.pack "idT0VmmEmSdDF1eMrajIVHP0XZ/8Udgeas1Zxy0va5tD/KP393Ri3w=="
    toPublic = bsToPoint bs
    Right bsPriv = decode $ BC.pack "iollyIcHaGeD/JpUNn/7ef1QAzE="
    ourPriv = os2ip bsPriv
    (ECC.Point secretX _Y) = ECC.pointMul curve ourPriv toPublic
    Just secretMacLong = i2ospOf 20 secretX
    secretMac = BC.take 16 secretMacLong

    macd = lbsTocbs $ BL.pack (line_ephemeral_key2 ++ inner_encrypted2)
    hmacVal = hmac SHA1.hash 64 secretMacLong macd

    r = B16.encode hmacVal
    -- r = B16.encode secretMac

-- "acd954779a8c12f7250d62a923341c6f7ce3b95a"
-- "1bb09216c600e77ca227261833618f540c40d483" expected

{-
secret: 
"1bca855c0ce80729feb6acdfbffddc7c 5c 2b a0 1c"
 1bca855c0ce80729feb6acdfbffddc7c ,
expected secret

hmac expected:  
 6d8ab807b303b993426b535509a416b6372eaa43,
"7b67a79c193c15e5a5194fd8bbf9ae798ee9e8e1"

-}



 -- line ephemeral public key
line_public_key :: [Word8]
line_public_key =
 [
 0x3d, 0x0c, 0xb3, 0x3b, 0xa2, 0xcd, 0x28, 0x9e, 0xdc, 0xee, 0x70, 0xd8, 0x1e, 0x5a, 0xab, 0x09,
 0x0b, 0x0c, 0xaa, 0xa8, 0x9c, 0x63, 0xaf, 0xbe, 0x93, 0x96, 0x8e, 0x83, 0xf5, 0x5f, 0x6d, 0x37,
 0x1a, 0x5b, 0x05, 0xaf, 0xd4, 0x01, 0x5d, 0xf0
 ]

inner_encrypted :: [Word8]
inner_encrypted =
 [
 0x12,
 0x59, 0xa8, 0xb2, 0xd7, 0x94, 0xef, 0x01, 0x9a, 0xc8, 0x3a, 0xee, 0x49, 0xc4, 0x93, 0xcb, 0x64,
 0xb8, 0x84, 0x04, 0x92, 0x11, 0x94, 0x44, 0x79, 0xa6, 0x08, 0x6b, 0x0d, 0x6f, 0xb8, 0xa6, 0xfc,
 0x99, 0x00, 0xe6, 0xca, 0xb4, 0x5f, 0x58, 0xb1, 0x12, 0x21, 0xe3, 0x57, 0xaa, 0x74, 0x53, 0x5d,
 0x66, 0x45, 0xbe, 0x44, 0xec, 0x75, 0x9e, 0x39, 0x8b, 0x6c, 0xe4, 0xa0, 0xd3, 0x6d, 0x41, 0xff,
 0xa8, 0x04, 0xb4, 0x96, 0x17, 0x26, 0xa9, 0xce, 0x9a, 0x5a, 0xf6, 0x35, 0x48, 0xf2, 0x52, 0xc2,
 0xb2, 0x57, 0xa4, 0xa8, 0xb6, 0xfe, 0xb4, 0x3a, 0x8f, 0xcb, 0x38, 0x17, 0x0e, 0x94, 0x83, 0xbe,
 0x90, 0x5d, 0x6e, 0xa7, 0x1c, 0xe1, 0xaa, 0x36, 0x39, 0x17, 0xfc, 0x09, 0x5f, 0x6f, 0xc7, 0x46,
 0x23, 0x36, 0xa9, 0xee, 0x6c, 0xef, 0x44, 0xab, 0xa5, 0x7c, 0xa3, 0xa5, 0xe0, 0xde, 0x9a, 0x28,
 0x21, 0xd4, 0x44, 0xe3, 0xd3, 0x48, 0x2f, 0x4e, 0xd5, 0x88, 0x58, 0x0e, 0xcf, 0xa9, 0x14, 0xf1,
 0xe0, 0xaf, 0xa0, 0x62, 0xa3, 0x6e, 0x9f, 0x91, 0x94, 0xb9, 0x7b, 0xaf, 0x4a, 0xd0, 0xbb, 0x80,
 0x4b, 0x30, 0x23, 0xd5, 0xf5, 0x15, 0x3d, 0x51, 0xc5, 0xef, 0x99, 0x79, 0x63, 0x30, 0xa5, 0xa5,
 0x76, 0x33, 0x51, 0x17, 0xb6, 0x20, 0x92, 0xe0, 0xce, 0x43, 0x4b, 0x64, 0xc4, 0x31, 0xec, 0x30,
 0xaf, 0x6c, 0x0b, 0x09, 0x6e, 0x91, 0xde, 0xb7, 0xdc, 0xed, 0x45, 0x83, 0x8d, 0x35, 0x97, 0xb4,
 0x68, 0x0d, 0xe8, 0x70, 0xd9, 0x0d, 0x67, 0x88, 0x93, 0x8e, 0x95, 0x69, 0x1d, 0xde, 0x9c, 0x7a,
 0xf0, 0xb6, 0x42, 0xda, 0x25
 ]


{-

alanz@alanz-laptop:~/mysrc/github/alanz/telehash-c$ ./bin/ping
loaded hashname 7ecf6a5884d483fde2f6a027e33e6e1756efdb70925557c3e3f776b35329aef5
cs->id_publichex dump
 0x89, 0xd4, 0xf4, 0x56, 0x69, 0x84, 0x99, 0x27, 0x43, 0x17, 0x57, 0x8c, 0xad, 0xa8, 0xc8, 0x54,
 0x73, 0xf4, 0x5d, 0x9f, 0xfc, 0x51, 0xd8, 0x1e, 0x6a, 0xcd, 0x59, 0xc7, 0x2d, 0x2f, 0x6b, 0x9b,
 0x43, 0xfc, 0xa3, 0xf7, 0xf7, 0x74, 0x62, 0xdf,
hex dump done
cs->line_privatehex dump
 0x2b, 0xef, 0x56, 0x9c, 0x61, 0x0c, 0x0a, 0xb0, 0x21, 0xc1, 0xaf, 0x20, 0x66, 0x36, 0xec, 0x4a,
 0xd5, 0x03, 0x14, 0x92,
hex dump done
cs->line_publichex dump
 0x99, 0x2e, 0xfa, 0x4b, 0x0d, 0xcb, 0x58, 0x13, 0xb6, 0x70, 0xc4, 0x30, 0x7e, 0x40, 0x37, 0x3a,
 0x3c, 0xd3, 0x83, 0xb8, 0x36, 0x29, 0x1b, 0x5d, 0x3b, 0x83, 0xcb, 0xcb, 0x62, 0x94, 0x01, 0x00,
 0xca, 0x0c, 0x28, 0x5f, 0x04, 0xcf, 0xa4, 0xeb,
hex dump done
scs->line_privatehex dump
 0xeb, 0x49, 0x01, 0x01, 0x5b, 0x79, 0x34, 0x1c, 0x61, 0xe8, 0x35, 0x43, 0xfc, 0xdf, 0x59, 0xb3,
 0x4f, 0x23, 0x6d, 0x52,
hex dump done
secret1hex dump
 0xe4, 0x9d, 0xe3, 0xf7, 0x57, 0xac, 0xc8, 0x2e, 0x1b, 0xca, 0x4f, 0x91, 0xd5, 0x0b, 0x19, 0x04,

hex dump done
packet_raw(inner)hex dump
 0x00, 0xbc, 0x7b, 0x22, 0x74, 0x6f, 0x22, 0x3a, 0x22, 0x66, 0x35, 0x30, 0x66, 0x34, 0x32, 0x33,
 0x63, 0x65, 0x37, 0x66, 0x39, 0x34, 0x66, 0x65, 0x39, 0x38, 0x63, 0x64, 0x64, 0x30, 0x39, 0x32,
 0x36, 0x38, 0x63, 0x37, 0x65, 0x35, 0x37, 0x30, 0x30, 0x31, 0x61, 0x65, 0x64, 0x33, 0x30, 0x30,
 0x62, 0x32, 0x33, 0x30, 0x32, 0x30, 0x38, 0x34, 0x30, 0x61, 0x38, 0x34, 0x61, 0x38, 0x38, 0x31,
 0x63, 0x37, 0x36, 0x37, 0x33, 0x39, 0x34, 0x37, 0x31, 0x22, 0x2c, 0x22, 0x66, 0x72, 0x6f, 0x6d,
 0x22, 0x3a, 0x7b, 0x22, 0x31, 0x61, 0x22, 0x3a, 0x22, 0x37, 0x32, 0x39, 0x62, 0x32, 0x32, 0x35,
 0x32, 0x65, 0x63, 0x31, 0x37, 0x34, 0x30, 0x65, 0x36, 0x37, 0x34, 0x33, 0x37, 0x61, 0x35, 0x30,
 0x35, 0x31, 0x38, 0x61, 0x31, 0x63, 0x31, 0x30, 0x33, 0x31, 0x34, 0x64, 0x37, 0x36, 0x63, 0x33,
 0x61, 0x22, 0x7d, 0x2c, 0x22, 0x6c, 0x69, 0x6e, 0x65, 0x22, 0x3a, 0x22, 0x62, 0x31, 0x39, 0x32,
 0x34, 0x61, 0x38, 0x65, 0x38, 0x65, 0x39, 0x63, 0x61, 0x61, 0x37, 0x36, 0x39, 0x33, 0x65, 0x36,
 0x31, 0x35, 0x65, 0x36, 0x35, 0x34, 0x33, 0x39, 0x33, 0x35, 0x30, 0x37, 0x22, 0x2c, 0x22, 0x61,
 0x74, 0x22, 0x3a, 0x31, 0x33, 0x39, 0x37, 0x31, 0x36, 0x34, 0x37, 0x38, 0x38, 0x7d, 0xa3, 0x45,
 0x0b, 0xfc, 0x3e, 0xaa, 0x43, 0xe7, 0x5c, 0x49, 0x7e, 0xe1, 0x0a, 0x8c, 0x8c, 0x8c, 0xb0, 0xd8,
 0x78, 0x0e, 0x9d, 0x35, 0x27, 0x19, 0xf9, 0x86, 0x3f, 0x7d, 0xc5, 0xf8, 0x7f, 0x20, 0xad, 0xb1,
 0x23, 0xb6, 0xbb, 0xd2, 0xf9, 0x2e,
hex dump done
secret2hex dump
 0x1b, 0xca, 0x85, 0x5c, 0x0c, 0xe8, 0x07, 0x29, 0xfe, 0xb6, 0xac, 0xdf, 0xbf, 0xfd, 0xdc, 0x7c,

sending open packet 293 {"type":"ipv4","ip":"208.126.199.195","port":42424}
hex dump of packet out
 0x00, 0x01, 0x1a,

 -- hmac 20 bytes
 0x6d, 0x8a, 0xb8, 0x07, 0xb3, 0x03, 0xb9, 0x93, 0x42, 0x6b, 0x53, 0x55, 0x09, 0xa4, 0x16, 0xb6,
 0x37, 0x2e, 0xaa, 0x43,

 -- line ephemeral public key
 0x99, 0x2e, 0xfa, 0x4b, 0x0d, 0xcb, 0x58, 0x13, 0xb6, 0x70, 0xc4, 0x30, 0x7e, 0x40, 0x37, 0x3a,
 0x3c, 0xd3, 0x83, 0xb8, 0x36, 0x29, 0x1b, 0x5d, 0x3b, 0x83, 0xcb, 0xcb, 0x62, 0x94, 0x01, 0x00,
 0xca, 0x0c, 0x28, 0x5f, 0x04, 0xcf, 0xa4, 0xeb,

 -- encrypted inner
 0xcd,
 0xb5, 0xda, 0xdf, 0xac, 0x08, 0xe3, 0x14, 0x9b, 0xe2, 0xe2, 0x52, 0x43, 0x5c, 0xf5, 0xfd, 0xca,
 0xa4, 0x6b, 0x58, 0xbd, 0xeb, 0x57, 0x12, 0xf0, 0x38, 0xb0, 0x75, 0xc7, 0xac, 0xa3, 0x46, 0x17,
 0xdd, 0x28, 0xa4, 0xa3, 0x7e, 0xd2, 0x8b, 0x98, 0xcd, 0x71, 0x0c, 0xc3, 0x21, 0x86, 0x8f, 0xb6,
 0x2a, 0xc8, 0x9b, 0x10, 0xfe, 0xd0, 0x99, 0xf6, 0x16, 0xfb, 0xeb, 0xe3, 0x6b, 0xea, 0x74, 0xc7,
 0x77, 0xfc, 0x04, 0x39, 0xa5, 0x5e, 0x26, 0xab, 0x17, 0xca, 0x69, 0xad, 0x11, 0x00, 0x73, 0xad,
 0x20, 0x4a, 0xd4, 0x9f, 0xda, 0xf5, 0xef, 0xef, 0x40, 0x42, 0x3e, 0xdb, 0xba, 0x90, 0x55, 0xea,
 0xf0, 0xa9, 0x7f, 0xe4, 0x5f, 0xbb, 0xe5, 0xb5, 0xb0, 0x00, 0x9b, 0xa9, 0x1c, 0x18, 0xbd, 0xea,
 0xe4, 0x94, 0x16, 0x78, 0x8b, 0x5b, 0xec, 0x74, 0x4f, 0x4f, 0x31, 0xf7, 0xa7, 0x3e, 0xf8, 0x56,
 0xc3, 0x68, 0xe6, 0xf8, 0xcf, 0x89, 0x37, 0x13, 0x0d, 0x16, 0x15, 0x47, 0x79, 0xd2, 0x57, 0xd1,
 0x45, 0x82, 0x8b, 0x83, 0x5f, 0x1f, 0x5b, 0x3b, 0xfc, 0x1c, 0x4a, 0x13, 0xce, 0xb0, 0x94, 0xbc,
 0x7a, 0x74, 0x8d, 0xb7, 0x46, 0xec, 0x80, 0xe6, 0x1b, 0xdc, 0xa1, 0x95, 0x20, 0x6d, 0xa6, 0x5a,
 0x25, 0x77, 0x33, 0x02, 0xf8, 0x10, 0x97, 0xcf, 0x9f, 0x39, 0x33, 0xfa, 0x65, 0x67, 0xe3, 0x87,
 0x16, 0xd4, 0xa5, 0xb6, 0x39, 0x65, 0xa0, 0x61, 0x66, 0x83, 0x8b, 0x65, 0x6e, 0x2f, 0x29, 0x5d,
 0x11, 0xb4, 0x62, 0x1a, 0x36, 0x49, 0x36, 0xd2, 0x09, 0x96, 0x4a, 0xe2, 0x69, 0xa3, 0x94, 0x6e,
 0x10, 0x92, 0xb2, 0x09, 0x44,
hex dump of packet out done
r
-}

 -- line ephemeral public key
line_ephemeral_key2 :: [Word8]
line_ephemeral_key2 =
 [
 0x99, 0x2e, 0xfa, 0x4b, 0x0d, 0xcb, 0x58, 0x13, 0xb6, 0x70, 0xc4, 0x30, 0x7e, 0x40, 0x37, 0x3a,
 0x3c, 0xd3, 0x83, 0xb8, 0x36, 0x29, 0x1b, 0x5d, 0x3b, 0x83, 0xcb, 0xcb, 0x62, 0x94, 0x01, 0x00,
 0xca, 0x0c, 0x28, 0x5f, 0x04, 0xcf, 0xa4, 0xeb
 ]

inner_encrypted2 :: [Word8]
inner_encrypted2 =
 [
 0xcd,
 0xb5, 0xda, 0xdf, 0xac, 0x08, 0xe3, 0x14, 0x9b, 0xe2, 0xe2, 0x52, 0x43, 0x5c, 0xf5, 0xfd, 0xca,
 0xa4, 0x6b, 0x58, 0xbd, 0xeb, 0x57, 0x12, 0xf0, 0x38, 0xb0, 0x75, 0xc7, 0xac, 0xa3, 0x46, 0x17,
 0xdd, 0x28, 0xa4, 0xa3, 0x7e, 0xd2, 0x8b, 0x98, 0xcd, 0x71, 0x0c, 0xc3, 0x21, 0x86, 0x8f, 0xb6,
 0x2a, 0xc8, 0x9b, 0x10, 0xfe, 0xd0, 0x99, 0xf6, 0x16, 0xfb, 0xeb, 0xe3, 0x6b, 0xea, 0x74, 0xc7,
 0x77, 0xfc, 0x04, 0x39, 0xa5, 0x5e, 0x26, 0xab, 0x17, 0xca, 0x69, 0xad, 0x11, 0x00, 0x73, 0xad,
 0x20, 0x4a, 0xd4, 0x9f, 0xda, 0xf5, 0xef, 0xef, 0x40, 0x42, 0x3e, 0xdb, 0xba, 0x90, 0x55, 0xea,
 0xf0, 0xa9, 0x7f, 0xe4, 0x5f, 0xbb, 0xe5, 0xb5, 0xb0, 0x00, 0x9b, 0xa9, 0x1c, 0x18, 0xbd, 0xea,
 0xe4, 0x94, 0x16, 0x78, 0x8b, 0x5b, 0xec, 0x74, 0x4f, 0x4f, 0x31, 0xf7, 0xa7, 0x3e, 0xf8, 0x56,
 0xc3, 0x68, 0xe6, 0xf8, 0xcf, 0x89, 0x37, 0x13, 0x0d, 0x16, 0x15, 0x47, 0x79, 0xd2, 0x57, 0xd1,
 0x45, 0x82, 0x8b, 0x83, 0x5f, 0x1f, 0x5b, 0x3b, 0xfc, 0x1c, 0x4a, 0x13, 0xce, 0xb0, 0x94, 0xbc,
 0x7a, 0x74, 0x8d, 0xb7, 0x46, 0xec, 0x80, 0xe6, 0x1b, 0xdc, 0xa1, 0x95, 0x20, 0x6d, 0xa6, 0x5a,
 0x25, 0x77, 0x33, 0x02, 0xf8, 0x10, 0x97, 0xcf, 0x9f, 0x39, 0x33, 0xfa, 0x65, 0x67, 0xe3, 0x87,
 0x16, 0xd4, 0xa5, 0xb6, 0x39, 0x65, 0xa0, 0x61, 0x66, 0x83, 0x8b, 0x65, 0x6e, 0x2f, 0x29, 0x5d,
 0x11, 0xb4, 0x62, 0x1a, 0x36, 0x49, 0x36, 0xd2, 0x09, 0x96, 0x4a, 0xe2, 0x69, 0xa3, 0x94, 0x6e,
 0x10, 0x92, 0xb2, 0x09, 0x44
 ]



-- -------------------------------------------------------------
{-
  console.log("AZ inner.js.from=",inner.js.from,crypto.createHash("sha1").update(inner.body).digest("hex"));

AZ inner.js.from= { '1a': 'o0UL/D6qQ+dcSX7hCoyMjLDYeA6dNScZ+YY/fcX4fyCtsSO2u9L5Lg==' } 729b2252ec1740e67437a50518a1c10314d76c3a
-}
testHash = B16.encode r
  where
    Right bs = decode "o0UL/D6qQ+dcSX7hCoyMjLDYeA6dNScZ+YY/fcX4fyCtsSO2u9L5Lg=="
    r = SHA1.hash $ bs


