{-# LANGUAGE QuasiQuotes, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell #-}

module TeleHash.Web 
       ( 
       runWeb
       ) where 

-- From http://www.yesodweb.com/book/rest
import Control.Concurrent
import TeleHash.Controller
import Yesod

data R = R (Chan Signal) (Chan Reply)

-- ---------------------------------------------------------------------

mkYesod "R" [parseRoutes|
/ RootR GET
-- /#String NameR GET
/fn/#String FnR GET
|]
instance Yesod R where
    approot _ = ""

-- ---------------------------------------------------------------------
    
getRootR :: GHandler sub R RepHtml
getRootR = defaultLayout $ do
    setTitle "Homepage"
    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.4/jquery.min.js"
    addJulius [julius|
$(function(){
    $("a").click(function(){
        jQuery.getJSON($(this).attr("href"), function(o){
            $("div").text(o.fn);
        });
        return false;
    });
});
|]
    let fns = words "Switch Counts"
    addHamlet [hamlet|
<div id="fn">
    Your results will be placed here if you have Javascript enabled.
<ul>
    $forall fn <- fns
        <li>
            <a href="@{FnR fn}">#{fn}
|]

-- ---------------------------------------------------------------------

getFnR fn = do
    let widget = do
          setTitle $ toHtml fn
          addHamlet [hamlet|Looks like you have Javascript off. Fn: #{fn}|]
    -- let json = jsonMap [("fn", jsonScalar fn)]
    (R ch1 ch2) <- getYesod      
    sw <- liftIO (querySwitch ch1 ch2)
    -- let json = jsonMap [("fn", jsonScalar $ prettySwitch sw)]
    let json = case fn of
          "Switch" -> jsonMap [("fn", jsonScalar $ prettySwitch sw)]
          "Counts" -> jsonMap [("fn", jsonScalar $ prettyCounts sw)]
          _        -> jsonMap [("fn", jsonScalar $ "unknown fn:" ++ fn)]
    defaultLayoutJson widget json

-- ---------------------------------------------------------------------
    
prettySwitch (ReplyGetSwitch sw) =    
  ("Connected:" ++ (show $ swConnected sw) ++ ",Seeds:" ++ (show $ swSeeds sw) 
   ++ ",IPP:" ++ (show $ swSelfIpp sw) ++ ",Hash:" ++ (show $ swSelfHash sw))
  
-- ---------------------------------------------------------------------
    
prettyCounts (ReplyGetSwitch sw) =    
  ("Tx:" ++ (show $ swCountTx sw) ++ ",Rx:" ++ (show $ swCountRx sw) )
  
-- ---------------------------------------------------------------------

runWeb :: Chan Signal -> Chan Reply -> IO ()
runWeb ch1 ch2  = warpDebug 4000 (R ch1 ch2)

-- EOF