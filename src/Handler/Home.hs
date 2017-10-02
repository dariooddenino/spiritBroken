{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Handler.Home where

import Import
import qualified Database.Esqueleto as E

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

displayEntry id isImage avgVote numVotes timeStamp title url = $(widgetFile "entry-line")

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
  entries :: [Entity Entry] <- runDB $ selectList [] []
  (numUsers:_) :: [E.Value Int] <- runDB $
    E.select . E.from $ \(_ :: E.SqlExpr (Entity User)) -> return E.countRows
  (numEntries:_) :: [E.Value Int] <- runDB $
    E.select . E.from $ \(_ :: E.SqlExpr (Entity Entry)) -> return E.countRows
  (numVotes:_) :: [E.Value Int] <- runDB $
    E.select . E.from $ \(_ :: E.SqlExpr (Entity Vote)) -> return E.countRows
  (numComments:_) :: [E.Value Int] <- runDB $
    E.select . E.from $ \(_ :: E.SqlExpr (Entity Comment)) -> return E.countRows
  defaultLayout $ do
    setTitle "SpiritBroken!"
    $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = getHomeR
-- postHomeR = do
--   entries :: [Entity Entry] <- runDB $ selectList [] []
--   defaultLayout $ do
--     setTitle "SpiritBroken"
--     $(widgetFile "homepage")

-- sampleForm :: Form FileForm
-- sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
--     <$> fileAFormReq "Choose a file"
--     <*> areq textField textSettings Nothing
--     -- Add attributes like the placeholder and CSS classes.
--     where textSettings = FieldSettings
--             { fsLabel = "What's on the file?"
--             , fsTooltip = Nothing
--             , fsId = Nothing
--             , fsName = Nothing
--             , fsAttrs =
--                 [ ("class", "form-control")
--                 , ("placeholder", "File description")
--                 ]
--             }

-- commentIds :: (Text, Text, Text)
-- commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")
