{-# LANGUAGE OverloadedStrings #-}
module TeleHash.New.Crypto1a
  (
    -- cset_1a
    crypt_init_1a
  , crypt_new_1a
  , crypt_private_1a
  , crypt_lineize_1a
  , crypt_openize_1a
  , crypt_deopenize_1a
  , crypt_line_1a
  , mkHashFromBS
  , mkHashFromB64
  ) where

-- | Implement Cypher Suite 1a for TeleHash, as per
-- https://github.com/telehash/telehash.org/blob/master/cs/1a.md

{-

Now

    ECC secp160r1 - small key sizes, balance of strong crypto (~1024bit) and still supportable with low cpu
    HMAC-SHA256 - common implementations available for embedded environments
    AES-128-CTR - low impact cipher, many implementations including hardware ones

was
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
import Data.Bits
import Data.ByteString.Base64
import Data.Int
import Data.List
import Data.Maybe
import Data.Word
import System.Time
import TeleHash.New.Convert
import TeleHash.New.Packet
import TeleHash.New.Types
import TeleHash.New.Utils

-- import qualified Crypto.Hash.SHA1 as SHA1
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

crypt_init_1a :: TeleHash ()
crypt_init_1a = do
  return ()

{-
int crypt_init_1a()
{
  struct timeval tv;
  unsigned int seed;
  gettimeofday(&tv, NULL);
  seed = (getpid() << 16) ^ tv.tv_sec ^ tv.tv_usec;
  srandom(seed); // srandomdev() is not universal
  uECC_set_rng(&RNG);
  return 0;
}
-}

-- ---------------------------------------------------------------------

mkHashFromBS :: BC.ByteString -> Hash
mkHashFromBS bs = Hash (BC.unpack $ B16.encode $ SHA256.hash $ bs)

mkHashFromB64 :: String -> Hash
mkHashFromB64 str = mkHashFromBS bs
  where
    Right bs = decode $ BC.pack str

-- ---------------------------------------------------------------------

crypt_new_1a :: Maybe String -> Maybe BC.ByteString -> TeleHash (Maybe Crypto)
crypt_new_1a mPubStr mPubBin = do
  mbs <- case mPubStr of
          Just pub -> do
            let mbs = decode $ BC.pack pub
            case mbs of
              Left _err -> do
                logT $ "invalid public key b64decode failed:" ++ pub
                return Nothing
              Right bs -> return (Just bs)

          Nothing -> return mPubBin
  case mbs of
    Nothing -> return Nothing
    Just bs -> do
      if BC.length bs /= 40
        then do logT $ "invalid public key wrong len:" ++ (show bs)
                return Nothing
        else do
          -- create line ephemeral key
          -- uECC_make_key(cs->line_public, cs->line_private);
          sw <- get
          let ((pubk,privk) ,g') = generate (swRNG sw) curve
          put $ sw { swRNG = g' } -- must come before next line, else update lost

          timeNow <- io getClockTime
          randomHexVal <- randomHEX 16
          let
            -- create the public key
              pubkey = PublicKey curve (bsToPoint bs)


              hexName = mkHashFromBS bs
              newParts = [("1a",unHash hexName)]
              cs = Crypt1a
                      { cs1aIdPrivate   = Nothing
                      , cs1aIdPublic    = Public1a pubkey
                      , cs1aLinePrivate = Private1a privk
                      , cs1aLinePublic  = Public1a pubk
                      , cs1aSeq         = 0
                      , cs1aKeyOut      = Nothing
                      , cs1aKeyIn       = Nothing
                      }
              c = Crypto
                      { cCsid      = "1a"
                      , cPart      = hexName
                      , cIsPrivate = False
                      , cLined     = LineNone
                      , cKeyLen    = 40
                      , cAtOut     = timeNow
                      , cAtIn      = Nothing
                      , cLineOut   = BC.pack randomHexVal
                      , cLineHex   = randomHexVal
                      , cLineIn    = ""
                      , cKey       = bs
                      , cCs        = cs
                      }

          logT $ "crypt_loadkey_1a:hexName=" ++ show (hexName)
          logT $ "crypt_loadkey_1a:pubkey=" ++ show (B16.encode bs)
          logT $ "crypt_loadkey_1a:parts=" ++ show (cPart c)
          return (Just c)



{-
int crypt_new_1a(crypt_t c, unsigned char *key, int len)
{
  unsigned char hash[32];
  crypt_1a_t cs;
  
  if(!key || len <= 0) return 1;
  
  c->cs = malloc(sizeof(struct crypt_1a_struct));
  memset(c->cs, 0, sizeof (struct crypt_1a_struct));
  cs = (crypt_1a_t)c->cs;

  if(len == uECC_BYTES*2)
  {
    memcpy(cs->id_public,key,uECC_BYTES*2);
  }else{
    // try to base64 decode in case that's the incoming format
    if(key[len] != 0 || base64_binlength((char*)key,0) != uECC_BYTES*2 || base64dec(cs->id_public,(char*)key,0)) return -1;
  }
  
  // generate fingerprint
  crypt_hash(cs->id_public,uECC_BYTES*2,hash);

  // create line ephemeral key
  uECC_make_key(cs->line_public, cs->line_private);

  // alloc/copy in the public values (free'd by crypt_free)  
  c->part = malloc(32*2+1);
  c->keylen = uECC_BYTES*2;
  c->key = malloc(c->keylen);
  memcpy(c->key,cs->id_public,uECC_BYTES*2);
  util_hex(hash,32,(unsigned char*)c->part);

  return 0;
}
-}

-- ---------------------------------------------------------------------

crypt_private_1a :: Crypto -> String -> TeleHash Crypto
crypt_private_1a c key = do
  let
    privatekey = pk
      where
        mbsp = decode $ BC.pack key
        pk = case mbsp of
          Left _err -> Nothing
          Right bsp -> Just $ Private1a privkey
            where
              i = os2ip bsp
              privkey = PrivateKey curve i
  return $ c { cIsPrivate = True
             , cCs = (cCs c) { cs1aIdPrivate = privatekey }
             }

{-
int crypt_private_1a(crypt_t c, unsigned char *key, int len)
{
  crypt_1a_t cs = (crypt_1a_t)c->cs;

  if(!key || len <= 0) return 1;

  if(len == uECC_BYTES)
  {
    memcpy(cs->id_private,key,uECC_BYTES);
  }else{
    // try to base64 decode in case that's the incoming format
    if(key[len] != 0 || base64_binlength((char*)key,0) != uECC_BYTES || base64dec(cs->id_private,(char*)key,0)) return -1;
  }

  c->isprivate = 1;
  return 0;
}
-}

-- ---------------------------------------------------------------------

fold1 :: BC.ByteString -> BC.ByteString
fold1 inVal = lbsTocbs $ foldn 16 (cbsTolbs inVal)

foldn :: Int64 -> BL.ByteString -> BL.ByteString
foldn n inVal = BL.pack r
  where
    (front,back) = BL.splitAt n inVal
    r = map (\(f,b) -> xor f b) $ BL.zip front back

{-
void fold1(unsigned char in[32], unsigned char out[16])
{
  unsigned char i;
  for(i=0;i<16;i++) out[i] = in[i] ^ in[i+16];
}
-}


-- ---------------------------------------------------------------------

fold3 :: BC.ByteString -> BC.ByteString
fold3 inVal = lbsTocbs f3
  where
    f1 = foldn 16 (cbsTolbs inVal)
    f2 = foldn 8 f1
    f3 = foldn 4 f2

{-
void fold3(unsigned char in[32], unsigned char out[4])
{
  unsigned char i, buf[16];
  for(i=0;i<16;i++) buf[i] = in[i] ^ in[i+16];
  for(i=0;i<8;i++) buf[i] ^= buf[i+8];
  for(i=0;i<4;i++) out[i] = buf[i] ^ buf[i+4];
}
-}

-- ---------------------------------------------------------------------

crypt_lineize_1a :: Crypto -> TxTelex -> TeleHash (Crypto,Maybe LinePacket)
crypt_lineize_1a c p = do
  let
    cs = cCs c
    (LP body) = toLinePacket (tPacket p)

    ivPrefix = lbsTocbs $ BL.pack $ take 12 (repeat (0::Word8))
    iv = lbsTocbs $ Binary.encode (cs1aSeq cs)
    ivc = BC.append ivPrefix iv

    keyOut = (gfromJust "crypt_lineize_1a" $ cs1aKeyOut cs)
    ctx = initAES keyOut
    cbody = encryptCTR ctx ivc body

    hm = hmac SHA256.hash 64 keyOut (BC.append iv cbody)
    hmFinal = fold3 hm

    final = foldl' BC.append BC.empty [cLineIn c,hmFinal,iv,cbody]

    rc = c { cCs = cs { cs1aSeq = (cs1aSeq cs) + 1 } }
    r = (rc,Just $ toLinePacket (Packet HeadEmpty (Body final)))
  return r

  -- 16 bytes of lineIn
  --  4 bytes of fold3 of hmac (put in at end)
  --  4 bytes of seq
  -- nn bytes of aes encrypted body

{-
packet_t crypt_lineize_1a(crypt_t c, packet_t p)
{
  packet_t line;
  unsigned char iv[16], hmac[32];
  crypt_1a_t cs = (crypt_1a_t)c->cs;

  line = packet_chain(p);
  packet_body(line,NULL,16+4+4+packet_len(p));
  memcpy(line->body,c->lineIn,16);
  memcpy(line->body+16+4,&(cs->seq),4);
  memset(iv,0,16);
  memcpy(iv+12,&(cs->seq),4);
  cs->seq++;

  aes_128_ctr(cs->keyOut,packet_len(p),iv,packet_raw(p),line->body+16+4+4);

  hmac_256(cs->keyOut,16,line->body+16+4,4+packet_len(p),hmac);
  fold3(hmac,line->body+16);

  return line;
}
-}

-- ---------------------------------------------------------------------

crypt_openize_1a :: Crypto -> Crypto -> OpenizeInner -> TeleHash (Maybe LinePacket)
crypt_openize_1a self c inner = do
  -- logT $ "crypt_openize_1a entered"
  -- logT $ "crypt_openize_1a:(self,c,inner)=" ++ show (self,c,inner)
  let cs  = cCs c -- line crypto
      scs = cCs self -- self/own crypto
  let Public1a (PublicKey _ linePub) = (cs1aIdPublic cs)
      Public1a (PublicKey _ ourPub)  = (cs1aIdPublic scs)
      js = lbsTocbs $ Aeson.encode inner
      -- innerPacket = Packet (HeadJson js) (Body $ pointTow8s linePub)
      innerPacket = Packet (HeadJson js) (Body $ pointTow8s ourPub)
      (LP innerPacketBody) = toLinePacket innerPacket
      body = innerPacketBody

  -- logT $ "crypt_openize_1a:js=" ++ (BC.unpack js)
  -- logT $ "crypt_openize_1a:linePub=" ++ show (B16.encode $ pointTow8s linePub)

  -- get the shared secret to create the iv+key for the open aes
  -- uses  line private key and destination public key
  secret <- uECC_shared_secret (cs1aIdPublic cs) (cs1aLinePrivate cs)
  -- logT $ "crypt_openize_1a:secret=" ++ show (B16.encode secret)
  let hash = fold1 $ SHA256.hash secret

      Just iv = i2ospOf 16 1
      cbody = encryptCTR (initAES hash) iv body

  -- generate secret for hmac
  -- line public, own private
  secret2 <- uECC_shared_secret (cs1aIdPublic cs) (gfromJust "crypt_openize_1a" $ cs1aIdPrivate scs)
  -- logT $ "crypt_openize_1a:secret2=" ++ show (B16.encode secret2)
  let (Public1a (PublicKey _ linePub)) = cs1aLinePublic cs
      macd = BC.append (pointTow8s linePub) cbody
      hmacVal = fold3 $ hmac SHA256.hash 64 secret2 macd

  let bodyFinal = BC.append hmacVal macd

  return $ Just $ toNetworkPacket $ OpenPacket 0x1a bodyFinal

{-
// create a new open packet
packet_t crypt_openize_1a(crypt_t self, crypt_t c, packet_t inner)
{
  unsigned char secret[uECC_BYTES], iv[16], hash[32];
  packet_t open;
  int inner_len;
  crypt_1a_t cs = (crypt_1a_t)c->cs, scs = (crypt_1a_t)self->cs;

  open = packet_chain(inner);
  packet_json(open,&(self->csid),1);
  inner_len = packet_len(inner);
  if(!packet_body(open,NULL,4+40+inner_len)) return NULL;

  // copy in the line public key
  memcpy(open->body+4, cs->line_public, 40);

  // get the shared secret to create the iv+key for the open aes
  if(!uECC_shared_secret(cs->id_public, cs->line_private, secret)) return packet_free(open);
  crypt_hash(secret,uECC_BYTES,hash);
  fold1(hash,hash);
  memset(iv,0,16);
  iv[15] = 1;

  // encrypt the inner
  aes_128_ctr(hash,inner_len,iv,packet_raw(inner),open->body+4+40);

  // generate secret for hmac
  if(!uECC_shared_secret(cs->id_public, scs->id_private, secret)) return packet_free(open);
  hmac_256(secret,uECC_BYTES,open->body+4,40+inner_len,hash);
  fold3(hash,open->body);

  return open;
}
-}

-- ---------------------------------------------------------------------

crypt_deopenize_1a :: Crypto -> NetworkPacket -> TeleHash DeOpenizeResult
crypt_deopenize_1a self open = do
  let cs = cCs self
  case open of
    LinePacket _ -> do
      logT $ "crypt_deopenize_1a:trying to deopenize a line packet"
      return DeOpenizeVerifyFail
    OpenPacket _ pbody -> do
      if BC.length pbody <= 44 -- c version checks for 44
        then do
          logT $ "crypt_deopenize_1a:body length too short:" ++ show (BC.length pbody)
          return DeOpenizeVerifyFail
        else do
          let
            mac1  = B16.encode $ BC.take 4 pbody
            pubBs = BC.take 40 $ BC.drop 4 pbody
            cbody = BC.drop 44 pbody
            linePubPoint = (bsToPoint pubBs)
            linePub = Public1a (PublicKey curve linePubPoint)

          -- get the shared secret to create the iv+key for the open aes
            pk@(Private1a (PrivateKey _ ourPriv)) = gfromJust "crypt_deopenize_1a.2" $ cs1aIdPrivate cs

          secret <- uECC_shared_secret linePub pk
          let hash = fold1 $ SHA256.hash secret
              Just iv = i2ospOf 16 1

          -- decrypt the inner
          let body = decryptCTR (initAES hash) iv cbody
              Just inner = fromLinePacket (LP body)

          logT $ "crypt_deopenize_1a:inner=" ++ show inner
          let HeadJson js = paHead inner
          logT $ "crypt_deopenize_1a:inner json=" ++ show (js)

          -- generate secret for hmac
          let Body ekey = paBody inner
          if BC.length ekey /= 40
            then do
              logT $ "crypt_deopenize_1a:got invalid public key size:" ++ show (BC.length ekey)
              return DeOpenizeVerifyFail
            else do
              let epub = Public1a $ PublicKey curve (bsToPoint $ ekey)
              secret2 <- uECC_shared_secret epub pk

              -- verify
              let hmacVal = B16.encode $ fold3 $ hmac SHA256.hash 64 secret2 (BC.drop 4 pbody)
              if hmacVal /= mac1
                then do
                  logT $ "crypt_deopenize_1a:invalid hmac:" ++ show (hmacVal,mac1)
                  return DeOpenizeVerifyFail
                else do
                  -- stash the hex line key w/ the inner
                  let Just json = Aeson.decode (cbsTolbs js) :: Maybe Aeson.Value

                  let ret = DeOpenize
                       { doLinePub = linePub -- ecc value
                       , doKey = unBody $ paBody inner
                       , doJs = json
                       , doCsid = "1a"
                       }
                  return ret

{-
packet_t crypt_deopenize_1a(crypt_t self, packet_t open)
{
  unsigned char secret[uECC_BYTES], iv[16], b64[uECC_BYTES*2*2], hash[32];
  packet_t inner, tmp;
  crypt_1a_t cs = (crypt_1a_t)self->cs;

  if(open->body_len <= (4+40)) return NULL;
  inner = packet_new();
  if(!packet_body(inner,NULL,open->body_len-(4+40))) return packet_free(inner);

  // get the shared secret to create the iv+key for the open aes
  if(!uECC_shared_secret(open->body+4, cs->id_private, secret)) return packet_free(inner);
  crypt_hash(secret,uECC_BYTES,hash);
  fold1(hash,hash);
  memset(iv,0,16);
  iv[15] = 1;

  // decrypt the inner
  aes_128_ctr(hash,inner->body_len,iv,open->body+4+40,inner->body);

  // load inner packet
  if((tmp = packet_parse(inner->body,inner->body_len)) == NULL) return packet_free(inner);
  packet_free(inner);
  inner = tmp;

  // generate secret for hmac
  if(inner->body_len != uECC_BYTES*2) return packet_free(inner);
  if(!uECC_shared_secret(inner->body, cs->id_private, secret)) return packet_free(inner);

  // verify
  hmac_256(secret,uECC_BYTES,open->body+4,open->body_len-4,hash);
  fold3(hash,hash);
  if(memcmp(hash,open->body,4) != 0) return packet_free(inner);

  // stash the hex line key w/ the inner
  util_hex(open->body+4,40,b64);
  packet_set_str(inner,"ecc",(char*)b64);

  return inner;
}
-}

-- ---------------------------------------------------------------------

-- |makes sure all the crypto line state is set up, and creates line keys if exist
crypt_line_1a :: DeOpenizeResult -> Crypto -> TeleHash (Maybe Crypto)
crypt_line_1a DeOpenizeVerifyFail _ = return Nothing
crypt_line_1a open c = do
  let cs = cCs c
  seqVal <- randomWord32

  -- do the diffie hellman
  let line_public = doLinePub open
  secret <- uECC_shared_secret line_public (cs1aLinePrivate cs)

  -- make the line keys
  let lineOut = cLineOut c
      lineIn  = cLineIn c

      keyOutCtx = SHA256.updates SHA256.init [secret,lineOut,lineIn]
      keyOut = fold1 (SHA256.finalize keyOutCtx)

      keyInCtx = SHA256.updates SHA256.init [secret,lineIn,lineOut]
      keyIn = fold1 (SHA256.finalize keyInCtx)

  return $ Just c { cCs = cs { cs1aKeyOut = Just keyOut
                             , cs1aKeyIn = Just keyIn
                             , cs1aSeq = seqVal
                             }}

{-
// makes sure all the crypto line state is set up, and creates line keys if exist
int crypt_line_1a(crypt_t c, packet_t inner)
{
  unsigned char line_public[uECC_BYTES*2], secret[uECC_BYTES], input[uECC_BYTES+16+16], hash[32];
  char *hecc;
  crypt_1a_t cs;
  
  cs = (crypt_1a_t)c->cs;
  hecc = packet_get_str(inner,"ecc"); // it's where we stashed it
  if(!hecc || strlen(hecc) != uECC_BYTES*4) return 1;
  crypt_rand((unsigned char*)&(cs->seq),4); // init seq to random start

  // do the diffie hellman
  util_unhex((unsigned char*)hecc,uECC_BYTES*4,line_public);
  if(!uECC_shared_secret(line_public, cs->line_private, secret)) return 1;

  // make line keys!
  memcpy(input,secret,uECC_BYTES);
  memcpy(input+uECC_BYTES,c->lineOut,16);
  memcpy(input+uECC_BYTES+16,c->lineIn,16);
  crypt_hash(input,uECC_BYTES+16+16,hash);
  fold1(hash,cs->keyOut);

  memcpy(input+uECC_BYTES,c->lineIn,16);
  memcpy(input+uECC_BYTES+16,c->lineOut,16);
  crypt_hash(input,uECC_BYTES+16+16,hash);
  fold1(hash,cs->keyIn);

  return 0;
}
-}

-- ---------------------------------------------------------------------

pointTow8s :: ECC.Point -> B.ByteString
pointTow8s ECC.PointO = B.empty
pointTow8s (ECC.Point i1 i2)= B.append i1_20 i2_20
  where
    (Just i1_20) = i2ospOf 20 i1
    (Just i2_20) = i2ospOf 20 i2

-- ---------------------------------------------------------------------

privToBs p = pbs
  where
    (Just pbs) = i2ospOf 20 p

-- ---------------------------------------------------------------------

bsToPoint :: BC.ByteString -> ECC.Point
bsToPoint bs = (ECC.Point i1 i2)
  where
    (b1,b2) = B.splitAt 20 bs
    i1 = os2ip b1
    i2 = os2ip b2


-- ---------------------------------------------------------------------

uECC_shared_secret :: TeleHash.New.Types.PublicKey -> TeleHash.New.Types.PrivateKey -> TeleHash BC.ByteString
uECC_shared_secret publicKey privateKey = do
  let (Public1a (PublicKey _ pubPoint)) = publicKey
      (Private1a (PrivateKey _ privPoint)) = privateKey
  let (ECC.Point sharedX _Y) = ECC.pointMul curve privPoint pubPoint
      (Just longkey) = i2ospOf 20 sharedX
  return longkey
  -- assert False undefined
{-
int uECC_shared_secret(const uint8_t p_publicKey[uECC_BYTES*2], const uint8_t p_privateKey[uECC_BYTES], uint8_t p_secret[uECC_BYTES])
{
    EccPoint l_public;
    uECC_word_t l_private[uECC_WORDS];
    uECC_word_t l_random[uECC_WORDS];

    g_rng((uint8_t *)l_random, sizeof(l_random));

    vli_bytesToNative(l_private, p_privateKey);
    vli_bytesToNative(l_public.x, p_publicKey);
    vli_bytesToNative(l_public.y, p_publicKey + uECC_BYTES);

    EccPoint l_product;
    EccPoint_mult(&l_product, &l_public, l_private, (vli_isZero(l_random) ? 0: l_random), vli_numBits(l_private, uECC_WORDS));

    vli_nativeToBytes(p_secret, l_product.x);

    return !EccPoint_isZero(&l_product);
}
-}

-- ---------------------------------------------------------------------
