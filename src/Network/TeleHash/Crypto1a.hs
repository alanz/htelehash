{-# LANGUAGE OverloadedStrings #-}

-- | Implement Cypher Suite 1a for TeleHash, as per
-- https://github.com/telehash/telehash.org/blob/master/cs/1a.md

module Network.TeleHash.Crypto1a
  (
    cset_1a

  , crypt_init_1a
  -- , crypt_new_1a
  -- , crypt_private_1a
  -- , crypt_lineize_1a
  -- , crypt_openize_1a
  -- , crypt_deopenize_1a
  -- , crypt_line_1a
  -- , crypt_delineize_1a
  , mkHashFromBS
  , mkHashFromB64
  ) where


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
import Data.Bits
import Data.ByteString.Base64
import Data.Int
import Data.List
import Data.Word
import System.Time
import Network.TeleHash.Convert
import Network.TeleHash.Packet
import Network.TeleHash.Types
import Network.TeleHash.Utils

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.PubKey.ECC.Prim as ECC
import qualified Crypto.Types.PubKey.ECC as ECC
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

-- ---------------------------------------------------------------------

cset_1a :: CSet
cset_1a = CS
  { cs_id        = "1a"
  , cs_init      = crypt_init_1a
  , cs_keygen    = crypt_keygen_1a
  , cs_new       = crypt_new_1a
  , cs_private   = crypt_private_1a
  , cs_lineize   = crypt_lineize_1a
  , cs_openize   = crypt_openize_1a
  , cs_deopenize = crypt_deopenize_1a
  , cs_line      = crypt_line_1a
  , cs_delineize = crypt_delineize_1a
  }

-- ---------------------------------------------------------------------

curve :: ECC.Curve
curve = ECC.getCurveByName ECC.SEC_p160r1

-- ---------------------------------------------------------------------

crypt_init_1a :: TeleHash ()
crypt_init_1a = do
  return ()


-- ---------------------------------------------------------------------

crypt_keygen_1a :: TeleHash (String,String)
crypt_keygen_1a = do
  -- create line ephemeral key
  sw <- get
  let ((pubk,privk) ,g') = generate (swRNG sw) curve
  put $ sw { swRNG = g' } -- must come before next line, else update
                          -- lost

  let (PrivateKey _ priv) = privk
      (PublicKey _ pub) = pubk
      pubStr  = BC.unpack $ B64.encode $ pointTow8s pub
      privStr = BC.unpack $B64.encode $ privToBs priv
  return (pubStr,privStr)

{-
int crypt_keygen_1a(packet_t p)
{
  char b64[uECC_BYTES*4];
  uint8_t id_private[uECC_BYTES], id_public[uECC_BYTES*2];

  // create line ephemeral key
  uECC_make_key(id_public, id_private);

  base64enc(b64,id_public,uECC_BYTES*2);
  packet_set_str(p,"1a",b64);

  base64enc(b64,id_private,uECC_BYTES);
  packet_set_str(p,"1a_secret",b64);

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
              -- newParts = [("1a",unHash hexName)] :: Parts
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
                      , cCs        = CS1a cset_1a cs
                      }

          -- logT $ "crypt_loadkey_1a:randomHexVal=" ++ show (randomHexVal)
          -- logT $ "crypt_loadkey_1a:hexName=" ++ show (hexName)
          -- logT $ "crypt_loadkey_1a:pubkey=" ++ show (B16.encode bs)
          -- logT $ "crypt_loadkey_1a:parts=" ++ show (cPart c)
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
    csnew = (cs1a (cCs c)) { cs1aIdPrivate = privatekey }
  return $ c { cIsPrivate = True
             , cCs = (cCs c) { cs1a = csnew }
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
    cs = cs1a (cCs c)
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

    rc = c { cCs = (cCs c) { cs1a = cs { cs1aSeq = (cs1aSeq cs) + 1 } }}
    r = (rc,Just $ toLinePacket (Packet HeadEmpty (Body final)))
  -- logT $ "crypt_lineize_1a:(p,body)=" ++ show (p,body)
  return r

  -- 16 bytes of lineIn
  --  4 bytes of fold3 of hmac (put in at end)
  --  4 bytes of seq
  -- nn bytes of aes encrypted body

-- ---------------------------------------------------------------------

crypt_openize_1a :: Crypto -> Crypto -> OpenizeInner -> TeleHash (Maybe LinePacket)
crypt_openize_1a self c inner = do
  -- logT $ "crypt_openize_1a entered"
  -- logT $ "crypt_openize_1a:(self,c,inner)=" ++ show (self,c,inner)
  let cs  = cs1a (cCs c) -- line crypto
      scs = cs1a (cCs self) -- self/own crypto
  let -- Public1a (PublicKey _ linePub) = (cs1aIdPublic cs)
      Public1a (PublicKey _ ourPub)  = (cs1aIdPublic scs)
      js = lbsTocbs $ Aeson.encode inner
      -- innerPacket = Packet (HeadJson js) (Body $ pointTow8s linePub)
      innerPacket = Packet (HeadJson js) (Body $ pointTow8s ourPub)
      (LP innerPacketBody) = toLinePacket innerPacket
      body = innerPacketBody

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
  let cs = cs1a (cCs self)
  case open of
    LinePacket _ -> do
      logT $ "crypt_deopenize_1a:trying to deopenize a line packet"
      return DeOpenizeVerifyFail
    PingPongPacket _ -> do
      logT $ "crypt_deopenize_1a:trying to deopenize a pingpong packet"
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
            pk  = gfromJust "crypt_deopenize_1a.2" $ cs1aIdPrivate cs

          secret <- uECC_shared_secret linePub pk
          let hash = fold1 $ SHA256.hash secret
              Just iv = i2ospOf 16 1

          -- decrypt the inner
          let body = decryptCTR (initAES hash) iv cbody
          logH (">>>>:crypt_deopenize_1a:inner " ++ cLineHex self) body

          minner <- io $ fromLinePacket (LP body)
          case minner of
            Nothing -> do
              logT $ "crypt_deopenize_1a: bad line packet received:" ++ show body
              return DeOpenizeVerifyFail
            Just inner -> do
              -- logT $ "crypt_deopenize_1a:inner=" ++ show inner
              let HeadJson js = paHead inner
              -- logT $ "crypt_deopenize_1a:inner json=" ++ show (js)

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

-- ---------------------------------------------------------------------

-- |makes sure all the crypto line state is set up, and creates line keys if exist
crypt_line_1a :: DeOpenizeResult -> Crypto -> TeleHash (Maybe Crypto)
crypt_line_1a DeOpenizeVerifyFail _ = return Nothing
crypt_line_1a open c = do
  let cs = cs1a (cCs c)
  seqVal <- randomWord32

  -- do the diffie hellman
  let line_public = doLinePub open
  secret <- uECC_shared_secret line_public (cs1aLinePrivate cs)

  -- make the line keys
  let lineOut = b16Tobs $ cLineOut c
      lineIn  = cLineIn c

      keyOutCtx = SHA256.updates SHA256.init [secret,lineOut,lineIn]
      keyOut = fold1 (SHA256.finalize keyOutCtx)

      keyInCtx = SHA256.updates SHA256.init [secret,lineIn,lineOut]
      keyIn = fold1 (SHA256.finalize keyInCtx)

  -- logT $ "crypt_line_1a:(secret,lineIn,lineOut)=" ++ show (B16.encode secret,B16.encode lineIn,B16.encode lineOut)
  -- logT $ "crypt_line_1a:(keyIn,keyOut)=" ++ show (B16.encode keyIn,B16.encode keyOut)

  return $ Just c { cCs = (cCs c) { cs1a = cs { cs1aKeyOut = Just keyOut
                                              , cs1aKeyIn = Just keyIn
                                              , cs1aSeq = seqVal
                                              }}}

_tl :: (BC.ByteString, BC.ByteString)
_tl = (B16.encode keyOut,B16.encode keyIn)
  where
    secret  = b16ToCbs "f3112580f84c04c74631d9a31fc010ad3424eb64"
    lineIn  = b16ToCbs "d5f1f542b98912d47187d38e1a847f04"
    lineOut = b16ToCbs "0369025b3e753eb180eb8c92de94c29f"

    keyOutCtx = SHA256.updates SHA256.init [secret,lineOut,lineIn]
    keyOut = fold1 (SHA256.finalize keyOutCtx)

    keyInCtx = SHA256.updates SHA256.init [secret,lineIn,lineOut]
    keyIn = fold1 (SHA256.finalize keyInCtx)

{-
AZ openline(ecdhe,lineInB,lineOutB)
f3112580f84c04c74631d9a31fc010ad3424eb64
d5f1f542b98912d47187d38e1a847f04
0369025b3e753eb180eb8c92de94c29f


AZ openline(encKey,decKey)
0a2db12a9507815d1c420e23c9458f20
9a611d7a9287f2ea71d8320ccf8a97c1
-}


-- ---------------------------------------------------------------------

crypt_delineize_1a :: Crypto -> NetworkTelex -> TeleHash (Either String RxTelex)
crypt_delineize_1a c rxTelex = do
  let cs = cs1a (cCs c)
  let (LinePacket pbody) = ntPacket rxTelex

  if (BC.length pbody < 16)
    then do
      logT $ "crypt_delineize_1a:no / short body"
      return (Left "crypt_delineize_1a:no / short body")
    else do
      -- skip the lineID
      let body = BC.drop 16 pbody
          mac1 = BC.take 4 $ BC.drop 16 pbody
          cbody = BC.drop 20 pbody
          body2 = BC.drop 8 body

      let iv = BC.take 4 $ BC.drop 4 body
          Just ivz = i2ospOf 12 0
          keyIn = gfromJust "crypt_delineize_1a" $ cs1aKeyIn cs

          hm = fold3 $ hmac SHA256.hash 64 keyIn cbody

      if hm /= mac1
        then do
          logT $ "hmac mismatch:" ++ show (B16.encode hm,B16.encode mac1)
          return $ Left $ "hmac mismatch:" ++ show (B16.encode hm,B16.encode mac1)
        else do
          let deciphered = decryptCTR (initAES keyIn) (BC.append ivz iv) body2
          mret <- io $ fromLinePacket (LP deciphered)

          logH (">>>>:crypt_delineize_1a:inner " ++ cLineHex c) deciphered

          -- logT $ "crypt_delineize_1a:mret=" ++ show mret
          case mret of
            Nothing -> return (Left "invalid decrypted packet")
            Just ret -> do
              case paHead ret of
                HeadEmpty -> return (Left "invalid channel packet")
                -- HeadByte _ -> return (Left "invalid channel packet")
                HeadJson js -> do
                  let mjson = Aeson.decode (cbsTolbs js) :: Maybe Aeson.Value
                  case mjson of
                    Nothing -> return (Left "invalid js in packet")
                    Just (Aeson.Object jsHashMap) ->
                      return (Right $ RxTelex { rtId = ntId rxTelex
                                              , rtSender = pJson $ ntSender rxTelex
                                              , rtAt = ntAt rxTelex
                                              , rtJs = jsHashMap
                                              , rtPacket = ret
                                              , rtChanId = Nothing
                                              })
                    Just _ -> return (Left $ "unexpected js type:" ++ show js)


-- ---------------------------------------------------------------------

pointTow8s :: ECC.Point -> B.ByteString
pointTow8s ECC.PointO = B.empty
pointTow8s (ECC.Point i1 i2)= B.append i1_20 i2_20
  where
    (Just i1_20) = i2ospOf 20 i1
    (Just i2_20) = i2ospOf 20 i2

-- ---------------------------------------------------------------------

privToBs :: Integer -> BC.ByteString
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

uECC_shared_secret :: Network.TeleHash.Types.PublicKey -> Network.TeleHash.Types.PrivateKey -> TeleHash BC.ByteString
uECC_shared_secret publicKey privateKey = do
  let (Public1a (PublicKey _ pubPoint)) = publicKey
      (Private1a (PrivateKey _ privPoint)) = privateKey
  let (ECC.Point sharedX _Y) = ECC.pointMul curve privPoint pubPoint
      (Just longkey) = i2ospOf 20 sharedX
  return longkey
  -- assert False undefined

-- ---------------------------------------------------------------------
