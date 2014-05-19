module Network.TeleHash.Ext.Path
  (
    ext_path
  , path_send
  , path_free
  ) where


import Control.Exception
import Control.Monad

import Network.TeleHash.Paths
import Network.TeleHash.Types
import Network.TeleHash.Utils
import Network.TeleHash.SwitchApi
import Network.TeleHash.SwitchUtils

import qualified Data.Aeson as Aeson


-- ---------------------------------------------------------------------

ext_path :: TChan -> TeleHash ()
ext_path c = do
  logT $ "ext_path entered for:" ++ showChan c
  let
    respFunc p = do
      if packet_has_key p "err"
        then do
          logT $ "ext_path:err in path packet:" ++ showJson (rtJs p)
        else do
          logT $ "ext_path:TODO path packet:" ++ showJson (rtJs p)
          let pathsJsValue = packet_get p "paths"
              mps = parseJsVal (gfromJust "ext_path" pathsJsValue) :: Maybe [PathJson]
          logT $ "ext_path:mps=" ++ show mps
          case mps of
            Nothing -> do
              logT $ "exp_path:could not parse paths:" ++ showJson (rtJs p)
            Just ps -> do
              let ps1 = filter (\pa -> pjsonType pa == PtIPv4) ps
              -- TODO: more detailed filtering of these paths
              forM_ ps1 $ \ path -> do
                mp1 <- chan_packet (chUid c) True
                let p3 = packet_set (gfromJust "ext_path" mp1) "path" path
                putPathIfNeeded (chTo c) (pathFromPathJson path)
                logT $ "ext_path:p3=" ++ showJson (tJs p3)
                chan_send (chUid c) p3

  util_chan_popall c (Just respFunc)

{-
void ext_path(chan_t c)
{
  packet_t p;
  while((p = chan_pop(c)))
  {
    DEBUG_PRINTF("TODO path packet %.*s\n", p->json_len, p->json);
    packet_free(p);
  }
}
-}

-- ---------------------------------------------------------------------

path_send :: HashName -> TeleHash ()
path_send to = do
  logT $ "path_send " ++ show to
  c <- chan_new to "path" Nothing
  let c2 = c { chHandler = Just path_handler }
  putChan c2
  mp <- chan_packet (chUid c2) True
  case mp of
    Nothing -> do
      logT $ "path_send:failed to make channel packet"
      return ()
    Just p -> do
      chan_send (chUid c2) p

-- ---------------------------------------------------------------------

path_handler :: Uid -> TeleHash ()
path_handler cid = do
  logT $ "path_handler entered for " ++ show cid


-- ---------------------------------------------------------------------

path_free :: PathJson -> TeleHash ()
path_free _path = return ()

{-
void path_free(path_t p)
{
  if(p->id) free(p->id);
  if(p->json) free(p->json);
  free(p);
}
-}
