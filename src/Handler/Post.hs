{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Post where

import Import
-- import Control.Monad.Catch (catch)
-- import qualified Database.Esqueleto as E
-- import Database.Esqueleto ((^.))

-- Entry json
--     userId UserId
--     url Text
--     UniqueEntry


--     deriving Eq
--     deriving Show

-- bTextField :: Field Handler Text
-- bTextField = Field
--   { fieldEnctype = UrlEncoded
--   , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
--       [whamlet|
--         <input id=#{idAttr} name=#{nameAttr} *{otherAttrs} type=text>
--       |]
--   }

-- postFormA :: UserId -> Form Entry
-- postFormA userId = renderDivs $ Entry
--   <$> pure userId
--   <*> areq textField "Url" Nothing

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
  (urlRes, urlView) <- mreq textField (urlSettings fId) Nothing
  let entryRes = Entry userId <$> urlRes
  let widget = [whamlet|
    #{extra}
    <div .field>
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
            Left (SomeException _) -> defaultLayout $ do
              setTitle "Post an url"
              setErrorMessage "URL already posted!"
              $(widgetFile "post")
            Right _ -> do
              redirect HomeR
        _ -> defaultLayout $ do
          setTitle "Post an URL"
          setMessage $ "There was an error, please try again."
          $(widgetFile "post")
    Nothing -> defaultLayout ([whamlet|<p>unreachable?|])
