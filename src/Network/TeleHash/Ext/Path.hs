module Network.TeleHash.Ext.Path
  (
    ext_path
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
  logT $ "ext_path entered for:" ++ show (chId c, chUid c)
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
                let p1 = packet_new (chTo c)
                    p2 = packet_set_int p1 "c" (unChannelId $ chId c)
                    p3 = packet_set p2 "path" path
                    p4 = p3 { tOut = path }
                putPathIfNeeded (chTo c) (pathFromPathJson path)
                logT $ "ext_path:p3=" ++ showJson (tJs p4)
                chan_send (chUid c) p4

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
