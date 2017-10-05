{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.User where

import Import

getUserR :: UserId -> Handler Html
getUserR userId = do
  user <- runDB $ get404 userId
  timeStamp <- liftIO $ printTime $ userSignupTime user
  defaultLayout $ do
    setTitle . toHtml $ userName user <> "'s profile page"
    $(widgetFile "user")
