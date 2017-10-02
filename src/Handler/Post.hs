{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
module Handler.Post where

import Import
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))
import Data.Time (getCurrentTime)

titleSettings :: Text -> FieldSettings master
titleSettings tid = FieldSettings
  { fsLabel = "Title"
  , fsAttrs = [ ("placeholder", "Enter a title")
              , ("class", "input is-large") ]
  , fsTooltip = Nothing
  , fsName = Just "title"
  , fsId = Just tid
  }

urlSettings :: Text -> FieldSettings master
urlSettings fid = FieldSettings
  { fsLabel = "Url"
  , fsAttrs = [ ("placeholder", "Enter a URL")
              , ("class", "input is-large") ]
  , fsTooltip = Nothing
  , fsName = Just "url"
  , fsId = Just fid
  }

postForm :: UserId -> Form Entry
postForm userId extra = do
  fId <- newIdent
  tId <- newIdent
  (urlRes, urlView) <- mreq textField (urlSettings fId) Nothing
  (titleRes, titleView) <- mreq textField (titleSettings tId) Nothing
  time <- liftIO getCurrentTime
  let entryRes = Entry userId False 0 0 time <$> titleRes <*> urlRes
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
  maid <- maybeAuthId
  case maid of
    Just mid -> do
      (formWidget, formEnctype) <- generateFormPost $ postForm mid
      defaultLayout $ do
        setTitle "Post an URL"
        $(widgetFile "post")
    Nothing -> defaultLayout ([whamlet|<p>unreachable?|])

errorDisplay :: Widget -> Enctype -> Handler Html
errorDisplay formWidget formEnctype = do
  setErrorMessage "There was an error, please try again."
  defaultLayout $ do
    setTitle "Post an URL"
    $(widgetFile "post")

findAndRedirect :: Text -> Widget -> Enctype -> Handler Html
findAndRedirect url formWidget formEnctype = do
  -- entries <- runDB $ selectList [EntryUrl ==. url] []
  entries <- runDB $ E.select $ E.from $ \e -> do
    E.where_ (e ^. EntryUrl E.==. E.val url)
    return e
  case entries of
    [] -> errorDisplay formWidget formEnctype
    (Entity eid _ : _) -> do
      setWarningMessage "The entry was already posted."
      redirect $ EntryR eid

postPostR :: Handler Html
postPostR = do
  maid <- maybeAuthId
  case maid of
    Just mid -> do
      ((result, formWidget), formEnctype) <- runFormPost $  postForm mid
      case result of
        FormSuccess entry -> do
          eres <- try $ runDB $ insert entry
          case eres of
            Left (SomeException _) ->
              findAndRedirect (entryUrl entry) formWidget formEnctype
            Right eid -> do
              setSuccessMessage "Entry added."
              redirect $ EntryR eid
        _ -> errorDisplay formWidget formEnctype
    Nothing -> defaultLayout ([whamlet|<p>unreachable?|])
