module Network.TeleHash.Ext.Path
  (
    ext_path
  , path_send
  , path_free
  ) where


import Control.Exception
import Control.Monad
import Control.Monad.State

import Data.Maybe

import Network.TeleHash.Dht
import Network.TeleHash.Hn
import Network.TeleHash.Paths
import Network.TeleHash.SwitchApi
import Network.TeleHash.SwitchUtils
import Network.TeleHash.Types
import Network.TeleHash.Utils

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
  c <- getChan cid

  let
    -- TODO: make respFunc a first class func to be called from switch_receive
    respFunc p = do
      logT $ "ext_link:respFunc:processing " ++ showJson (rtJs p)
      -- always respond/ack, except if there is an error or end
      let merr = packet_get_str p "err"
          mend = packet_get_str p "end"
      if any isJust [merr,mend]
        then return ()
        else do
          let mv = packet_get p "path"
              mlp = case mv of
                Just v  -> parseJsVal v :: Maybe PathJson
                Nothing -> Nothing
          case mlp of
            Nothing -> return ()
            Just lp@(PIPv4 pipv4) -> do
              case pjsonIp lp of
                Nothing -> return ()
                Just ip -> do
                  logT $ "path_handler:checking ip:" ++ show ip
                  if isLocalIP ip
                    then return ()
                    else do
                      logT $ "path_handler:got our remote ip:" ++ show ip
                      sw <- get
                      put $ sw {swExternalIPP = Just pipv4 }
            Just lp -> do
              logT $ "path_handler:unexpected path type :" ++ show lp

  util_chan_popall c (Just respFunc)

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
