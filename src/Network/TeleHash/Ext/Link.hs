{-# LANGUAGE OverloadedStrings #-}
module Network.TeleHash.Ext.Link
  (
    ext_link
  ) where

import Control.Exception
import Control.Monad.State
import Data.Maybe
import System.Time

import Network.TeleHash.Dht
import Network.TeleHash.Hn
import Network.TeleHash.SwitchApi
import Network.TeleHash.SwitchUtils
import Network.TeleHash.Types
import Network.TeleHash.Utils
import Network.TeleHash.Ext.Path
import Network.TeleHash.Ext.Seek


import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

-- ---------------------------------------------------------------------

-- |Accept a DHT link
-- This should only be called for the first packet received on a given
-- link channel.
ext_link :: Uid -> TeleHash ()
ext_link cid = do
  c <- getChan cid
  logR $ "ext_link entered for:" ++ showChan c
  let
    respFunc p = do
      logR $ "ext_link:respFunc:processing " ++ showJson (rtJs p)
      c2 <- getChan (chUid c)

      let mlp = parseJs (rtJs p) :: Maybe LinkReply
      case mlp of
        Nothing -> do
          logR $ "ext_link:unexpected packet:" ++ show p
          return ()
        Just (LinkReplyErr _err) -> do
          logR $ "ext_link:got err:" ++ show p
          end_link (chUid c)
          return ()
        Just LinkReplyEnd -> do
          logR $ "ext_link:link ended"
          end_link (chUid c)
          return ()
        Just lrp -> do

          -- add in this link
          add_link (chUid c)

          -- send a response if this is a new incoming
          -- if(!chan.sentAt) packet.from.link();
          case chLast c2 of
            Just _ -> do
              -- send_keepalive (chUid c2)
              logT $ "ext_link:c2=" ++ show c2
              assert False undefined
              return ()
            Nothing -> do
              hc <- getHN (chTo c)
              logT $ "ext_link:creating new link to hn:" ++ show (hHashName hc)
              void $ link_hn (hHashName hc) (Just $ chUid c2)
              -- send_keepalive (chUid c2)

          process_link_sees (chUid c2) p lrp

          -- this is a new link, request a path
          -- logT $ "ext_link:request a path: " ++ show (chTo c2)
          path_send (chTo c2)


  util_chan_popall c (Just respFunc)

  c3 <- getChan (chUid c)
  putChan $ c3 { chHandler = Just link_handler }

-- ---------------------------------------------------------------------

end_link :: Uid -> TeleHash ()
end_link cid = do
  c <- getChan cid
  deleteFromDht (chTo c)
  void $ withHN (chTo c) $ \hc -> hc { hState  = LsIdle }
  chan_fail (cid) Nothing

-- ---------------------------------------------------------------------

add_link :: Uid -> TeleHash ()
add_link cid = do
   c <- getChan cid
   -- hc <- getHN (chTo c)
   now <- io $ getClockTime
   void $ withHN (chTo c) $ \hc ->
     hc { hLinkAge  = Just now
        , hLinkChan = Just (chUid c)
        , hState    = LsLinked
        }
   putChanInHnIfNeeded (chTo c) cid

   logT $ "add_link:inserting into dht:" ++ show (chTo c)
   -- Check if we have the current hn in our dht
   insertIntoDht (chTo c)

-- ---------------------------------------------------------------------

-- |Process an incoming link ack, flagged as a seed, containing
-- possible "see" values
process_link_sees :: Uid -> RxTelex -> LinkReply -> TeleHash ()
process_link_sees cid p lrp = do
  let (isSeed,sees) = case lrp of
        LinkReplyKeepAlive is -> (is,[])
        LinkReplyNormal is sv -> (is,sv)
        _                     -> (False,[])
  c <- getChan cid

{-
-- js version does this

  // look for any see and check to see if we should create a link
  if(Array.isArray(packet.js.see)) packet.js.see.forEach(function(address){
    var hn = packet.from.sees(address);
    if(!hn || hn.linked) return;
    if(self.buckets[hn.bucket].length < defaults.link_k) hn.link();
  });

-}

  -- let sees1 = []
  logT $ "process_link_sees:PROCESSING SEES:" ++ show sees
  forM_ sees $ \see -> do
    logT $ "process_link_sees:see=" ++ show see
    sw <- get
    let fields = Text.splitOn "," (Text.pack see)
    mhn <- hn_fromaddress (map Text.unpack fields) (chTo c)
    case mhn of
      Just hn -> do
        hc <- hn_get hn
        if isJust (hLinkAge hc)
          then do
            logT $ "process_link_sees:isJust hLinkAge for" ++ show (hHashName hc)
            return () -- nothing to do
          else do
            -- create a link to the new bucket
            (_distance,bucket) <- getBucketContentsForHn (hHashName hc)
            if not (Set.member (hHashName hc) bucket) &&
               Set.size bucket <= (swDhtK sw)
              then do
                logT $ "process_link_sees:creating new link to:" ++ show (hHashName hc)
                void $ link_hn (hHashName hc) Nothing
              else do
                logT $ "process_link_sees:NOT creating new link to:" ++ show (hHashName hc,_distance,bucket,swDhtK sw)
                return ()
      Nothing -> do
        logT $ "process_link_sees:got junk see:" ++ show see


  -- TODO: check for and process bridges
  return ()

-- ---------------------------------------------------------------------

{-
  // track other hashnames this one sees
  hn.sees = function(address)
  {
    if(typeof address != "string") warn("invalid see address",address,hn.hashname);
    if(typeof address != "string") return false;
    var parts = address.split(",");
    if(!self.isHashname(parts[0]) || parts[0] == self.hashname) return false;
    var see = self.whois(parts[0]);
    if(!see) return false;
    // save suggested path if given/valid
    if(parts.length >= 4 && parts[2].split(".").length == 4 && parseInt(parts[3]) > 0) see.pathGet({type:"ipv4",ip:parts[2],port:parseInt(parts[3])});
    if(!see.vias) see.vias = {};
    // save suggested csid if we don't know one yet
    see.vias[hn.hashname] = see.cisd || parts[1];
    return see;
  }

-}

-- ---------------------------------------------------------------------

-- |Set up during first message received on a link, will process all
-- subsequent messages.
link_handler :: Uid -> TeleHash ()
link_handler cid = do
  c <- getChan cid
  logT $ "link_handler:processing " ++ showChan c

  util_chan_popall c $ Just $ \p -> do
    -- ignore if this isn't the main link
    -- if(!packet.from || !packet.from.linked || packet.from.linked != chan) return;
    from <- getHN (chTo c)
    if not (isJust (hLinkChan from) && fromJust (hLinkChan from) == (chUid c))
      then do return ()
      else do
        if packet_has_key p "err"
          then do
            logR $ "LINKDOWN:" ++ showChanShort c ++ ": " ++ packet_get_str_always p "err"
            logT $ "link_handler:(hLinkChan from, chUid c):" ++ show (hLinkChan from, chUid c)
            end_link (chUid c)
            -- if this HN was ever active, try to re-start it on a new
            -- channel
            void $ withHN (chTo c) $ \hc -> hc { hLinkChan = Nothing }
            void $ link_hn (chTo c) Nothing
          else do
            -- update seed status
            case packet_get p "seed" of
              Nothing -> return ()
              Just (Aeson.Bool isSeed) -> do
                logT $ "link_handler:updating hIsSeed for " ++ (show (chTo c,isSeed))
                void $ withHN (chTo c) $ \hc -> hc { hIsSeed = isSeed }
              Just _ -> do
                logT $ "link_handler:got strange seed value for:" ++ show p

            -- only send a response if we've not sent one in a while
            now <- io $ getClockTime
            c2 <- getChan cid
            mSentAt <- case chLast c2 of
              Nothing -> return Nothing
              Just pj -> do
                path <- getPath (chTo c2) pj
                return $ pAtOut path
            if isTimeOut now mSentAt param_link_timeout_secs
              then do
                send_keepalive cid
                return ()
              else return ()

{-

function inMaintenance(err, packet, chan)
{
  // ignore if this isn't the main link
  if(!packet.from || !packet.from.linked || packet.from.linked != chan) return;
  var self = packet.from.self;
  if(err)
  {
    debug("LINKDOWN",packet.from.hashname,err);
    delete packet.from.linked;
    var index = self.buckets[packet.from.bucket].indexOf(packet.from);
    if(index > -1) self.buckets[packet.from.bucket].splice(index,1);
    // if this channel was ever active, try to re-start it
    if(chan.recvAt) packet.from.link();
    return;
  }

  // update seed status
  packet.from.seed = packet.js.seed;

  // only send a response if we've not sent one in a while
  if((Date.now() - chan.sentAt) > Math.ceil(defaults.link_timer/2)) chan.send({js:{seed:self.seed}});
}

-}

-- ---------------------------------------------------------------------

send_keepalive :: Uid -> TeleHash ()
send_keepalive cid = do
  c <- getChan cid
  mreply <- chan_packet (chUid c) True
  case mreply of
    Nothing -> do
      c3 <- getChan (chUid c)
      logT $ "ext_link:send_keepalive:could not create a channel packet for " ++ show c3
    Just reply -> do
      sw <- get
      let reply2 = packet_set reply "seed" (swIsSeed sw)
      chan_send (chUid c) reply2

-- ---------------------------------------------------------------------
{-
Link state machine

States
------

IDLE          : not linked
LINK_RX_WAIT  : their link up, waiting for ours
LINK_TX_WAIT  : waiting for our link to come up
LINK_TX_WAIT2 : ours up, waiting for theirs
LINKED        : both up
FAIL1         : ping missed, re-open original
FAIL2         : ping missed, re-open new

Transitions
------------

State         event         action(s)         next

-- remote initiated
IDLE          link_rx       tx_link,tx_ack LINK_RX_WAIT

LINK_RX_WAIT  link_rx       tx_link,tx_ack LINK_RX_WAIT
LINK_RX_WAIT  link_ack       -             LINKED
LINK_RX_WAIT  ack_timeout   tx_end         IDLE
LINK_RX_WAIT  link_cmd      tx_link        LINK_RX_WAIT
LINK_RX_WAIT  ping_rx       tx_ping_ack    LINK_RX_WAIT
LINK_RX_WAIT  end_rx        tx_end         IDLE
LINK_RX_WAIT  err_rx        tx_end         IDLE


-- own initiated
IDLE          link cmd      tx_link        LINK_TX_WAIT

LINK_TX_WAIT  link_ack       -             LINK_TX_WAIT2
LINK_TX_WAIT  ack_timeout    -             IDLE
LINK_TX_WAIT  link_rx       tx_ack         LINK_RX_WAIT
LINK_TX_WAIT  end_rx        tx_end         IDLE
LINK_TX_WAIT  err_rx        tx_end         IDLE

LINK_TX_WAIT2 link_rx       tx_ack         LINKED
LINK_TX_WAIT2 timer         tx_ping        LINK_TX_WAIT2
LINK_TX_WAIT2 ping_timeout  tx_end         IDLE
LINK_TX_WAIT2 end_rx        tx_end         IDLE
LINK_TX_WAIT2 err_rx        tx_end         IDLE

-- maintenance [essentially two parallel state machines]
LINKED        timer         tx_ping        LINKED
LINKED        ping_timeout  tx_end,reopen  FAIL1
LINKED        ping_rx       tx_ping_ack    LINKED
LINKED        end_rx        tx_end         IDLE
LINKED        err_rx        tx_end         IDLE

-- failure/retry process
FAIL1         open_timeout  reopen2        FAIL2
FAIL1         link_ack      -              LINK_TX_WAIT2
FAIL1         link_rx       tx_link,tx_ack LINK_RX_WAIT

FAIL2         open_timeout  seek           LINK_TX_WAIT


Events
------
link_rx : link message received from remote

link_ack : positive response received from remote to our link message

ack_timout : no response received from remote to our link message
             within designated time

link_cmd : we choose to send a link message

timer : periodic (29sec) event, used to initiate ping messages

ping_rx : keepalive received from remote

ping_timeout : no response received from remote to our ping message
             within designated time

end_rx : message received from remote with end set

err_rx : message received from remote with err set
         [Note: could also mean underlying channel failure]

open_timeout : no response to open message within timeout

Actions
--------
tx_link : send link message to remote
tx_ack  : send link ack message to remote
tx_end  : send end message to remote
tx_ping : send keepalive message to remote
tx_err  : will not be sent by this state machine, but may be sent by lower layers
reopen  : resend original OPEN message on all known paths to remote, as
          well as original peer to original seed, queue tx_link for when open
reopen2 : send new OPEN message on all known paths to remote, as
          well as original peer to original seed, queue tx_link for when open
seek:   : throw away all knowlede of the remote and start the connection
          process from the beginning, queue tx_link for when open

-}
