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


mkYesod "R" [parseRoutes|
/ RootR GET
/#String NameR GET
/fn/#String FnR GET
|]
instance Yesod R where
    approot _ = ""

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
    let fns = words "Switch"
    addHamlet [hamlet|
<div id="fn">
    Your results will be placed here if you have Javascript enabled.
<ul>
    $forall fn <- fns
        <li>
            <a href="@{FnR fn}">#{fn}
|]

getNameR name = do
    let widget = do
            setTitle $ toHtml name
            addHamlet [hamlet|Looks like you have Javascript off. Name: #{name}|]
    let json = jsonMap [("name", jsonScalar name)]
    defaultLayoutJson widget json

getFnR fn = do
    let widget = do
          setTitle $ toHtml fn
          addHamlet [hamlet|Looks like you have Javascript off. Fn: #{fn}|]
    -- let json = jsonMap [("fn", jsonScalar fn)]
    (R ch1 ch2) <- getYesod      
    sw <- liftIO (querySwitch ch1 ch2)
    -- let json = jsonMap [("fn", jsonScalar "hello world")]
    let json = jsonMap [("fn", jsonScalar $ show sw)]
    defaultLayoutJson widget json

runWeb ch1 ch2  = warpDebug 4000 (R ch1 ch2)

-- EOF