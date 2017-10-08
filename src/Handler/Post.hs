{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
module Handler.Post where

import Import
import Data.Time (getCurrentTime)
import Helpers.Form (titleSettings, urlSettings)
import Models.Entry (isUrlImg)

postForm :: UserId -> Form Entry
postForm userId extra = do
  fId <- newIdent
  tId <- newIdent
  (urlRes, urlView) <- mreq textField (urlSettings fId) Nothing
  (titleRes, titleView) <- mreq textField (titleSettings tId) Nothing
  time <- liftIO getCurrentTime
  let entryRes = Entry userId False 0 0 0 time <$> titleRes <*> urlRes <*> pure Nothing
  let widget = [whamlet|
    #{extra}
    <div .field>
      <label .label>
        ^{fvLabel titleView}
      <div .control>
        ^{fvInput titleView}
    <div .field>
      <label .label>
        ^{fvLabel urlView}
      <div .control>
        ^{fvInput urlView}
  |]
  return (entryRes, widget)

getPostR :: Handler Html
getPostR = do
  (uid, _) <- requireAuthPair
  (formWidget, formEnctype) <- generateFormPost $ postForm uid
  defaultLayout $ do
    setTitle "Post an URL"
    $(widgetFile "post")

postPostR :: Handler Html
postPostR = do
  (uid, _) <- requireAuthPair
  ((result, formWidget), formEnctype) <- runFormPost $ postForm uid
  time <- liftIO getCurrentTime
  case result of
    FormSuccess entry -> do
      existingEntry <- runDB $ getBy $ UniqueEntry (entryUrl entry)
      _ <- case existingEntry of
        Just (Entity eId _) -> do
          setWarningMessage "This entry was already posted!"
          redirect $ EntryR eId
        Nothing -> do
          eid <- runDB $ do
            e <- insert $ entry { entryIsImage = isUrlImg (unpack $ entryUrl entry)
                                , entryTimeStamp = time
                                }
            _ <- update uid [ UserNumEntries +=. 1 ]
            return e
          setSuccessMessage "Entry added."
          redirect $ EntryR eid
      defaultLayout $ do
        setTitle "Post an URL"
        $(widgetFile "post")
    _ -> do
      setErrorMessage "There was an error, please try again."
      defaultLayout $ do
        setTitle "Post an URL"
        $(widgetFile "post")
