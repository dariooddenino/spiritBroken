{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Helpers.Form where

import Import
import qualified Data.Text.Read
import Text.Julius (juliusFile)

data FormAndHandler = forall a. FormAndHandler (Form a) (FormResult a -> Handler Html)

runMultipleFormsPost :: [FormAndHandler] -> Handler Html
runMultipleFormsPost [] = redirect HomeR
runMultipleFormsPost (FormAndHandler form handler : t) = do
  ((res, _), _) <- runFormPost form
  case res of
    FormMissing ->
      runMultipleFormsPost t
    _ ->
      handler res

rangeField :: Field Handler Int
rangeField = Field
  { fieldParse = parseHelper $ \s ->
      case Data.Text.Read.signed Data.Text.Read.decimal s of
        Right (a, "") -> Right a
        _ -> Left $ MsgInvalidInteger s
  , fieldView = \theId name attrs val isReq -> do
      let myId = String theId
      toWidget [hamlet|
        $newline never
        <input id="#{theId}" name="#{name}" *{attrs} type="range" step=1 :isReq:required="" value="#{showVal val}">
        <output for="#{theId}">
      |]
      toWidget $(juliusFile "templates/slider.julius")
  , fieldEnctype = UrlEncoded
  }
  where
    showVal = either id (pack . showI)
    showI x = show (fromIntegral x :: Integer)

rangeSettings :: Text -> FieldSettings master
rangeSettings vId = FieldSettings
  { fsLabel = "Vote"
  , fsName = Just "value"
  , fsId = Just vId
  , fsTooltip = Nothing
  , fsAttrs = [ ("class", "slider has-output is-fullwidth")
              , ("min", "0")
              , ("max", "1000")
              ]
  }

areaField :: Field Handler Text
areaField = Field
  { fieldParse = parseHelper Right
  , fieldView = \theId name attrs _ isReq ->
      [whamlet|
        $newline never
        <textarea id="#{theId}" name="#{name}" *{attrs} :isReq:required="">
      |]
  , fieldEnctype = UrlEncoded
  }

areaSettings :: Text -> FieldSettings App
areaSettings aid = FieldSettings
  { fsLabel = "Add a comment"
  , fsName = Just "comment"
  , fsId = Just aid
  , fsTooltip = Nothing
  , fsAttrs = [ ("class", "textarea")
              , ("placeholder", "Write something...")
              ]}

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

entryVoteFormIdent :: EntryId -> Text
entryVoteFormIdent eid = "entryvote" <> showKey eid

entryCommentFormIdent :: EntryId -> Text
entryCommentFormIdent eid = "entrycomment" <> showKey eid
