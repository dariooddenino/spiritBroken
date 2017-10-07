{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Models.User where

import Import.NoFoundation
import Yesod.Auth.GoogleEmail2 (Person, getPerson, getUserAccessToken)

getAuthInfo :: Creds master -> Manager -> HandlerT master IO (Maybe Person)
getAuthInfo creds m = case credsPlugin creds of
  "googleemail2" -> do
    token <- getUserAccessToken
    case token of
      Nothing -> return Nothing
      Just t -> getPerson m t
  _ -> return Nothing
