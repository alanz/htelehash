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
/stats StatsR GET
/fn/#String FnR GET
|]

instance Yesod R where


-- ---------------------------------------------------------------------

getRootR :: GHandler sub R RepHtml
getRootR = defaultLayout $ do
    setTitle "Homepage"
    addHamlet [hamlet|
<ul>
  <li> <a href="@{FnR "Master"}">Master
  <li> <a href="@{FnR "RawMaster"}">RawMaster
  <li> <a href="@{StatsR}">stats

|]
{-

-}
-- ---------------------------------------------------------------------

getFnR :: [Char] -> Handler RepHtml
getFnR fn = do
    -- let json = jsonMap [("fn", jsonScalar fn)]
    (R ch1 ch2) <- getYesod
    sw <- liftIO (queryMaster ch1 ch2)
    -- let json = jsonMap [("fn", jsonScalar $ prettyMaster sw)]
    let json = case fn of
          "RawMaster" -> show sw
          "Master"    -> prettyMaster sw
          "Counts"    -> prettyCounts sw
          _           -> "unknown fn:" ++ fn
    let widget = do
          setTitle $ toHtml fn
          addHamlet [hamlet|Fn: #{fn}<br/><pre>#{json}</pre>|]
    defaultLayout widget

-- ---------------------------------------------------------------------

getStatsR :: Handler RepHtml
getStatsR = do
  (R ch1 ch2) <- getYesod
  sw <- liftIO (queryMaster ch1 ch2)
  let content = prettyCounts sw

  let widget = do
        setTitle $ "Stats"
        addHamlet [hamlet|Content = #{content}.|]

  defaultLayout widget

-- ---------------------------------------------------------------------

prettyMaster :: Reply -> String
prettyMaster (ReplyGetMaster self) =
  ("Connected:" ++ (show $ selfConnected self) ++ ",Seeds:" ++ (show $ selfSeeds self)
   ++ ",IPP:" ++ (show $ selfSelfIpp self) ++ ",Hash:" ++ (show $ selfSelfHash self))

-- ---------------------------------------------------------------------

prettyCounts :: Reply -> String
prettyCounts (ReplyGetMaster self) =
  ("Online:" ++ (show $ selfCountOnline self) ++ ",Tx:" ++ (show $ selfCountTx self) ++ ",Rx:" ++ (show $ selfCountRx self) )

-- ---------------------------------------------------------------------

runWeb :: Chan Signal -> Chan Reply -> IO ()
runWeb ch1 ch2  = warpDebug 4000 (R ch1 ch2)

-- EOF