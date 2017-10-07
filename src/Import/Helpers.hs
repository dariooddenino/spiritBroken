{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Import.Helpers where

import Data.Time
import Import.NoFoundation as Import
import Database.Persist.Sql (fromSqlKey)

setInfoMessage :: MonadHandler m => Text -> m ()
setInfoMessage text = setMessage [shamlet|
  <section .hero.is-info.is-bold #message>
    <div .container.is-fluid>
      <i .fa.fa-info-circle>
      #{text}
|]

setSuccessMessage :: MonadHandler m => Text -> m ()
setSuccessMessage text = setMessage [shamlet|
  <section .hero.is-success.is-bold #message>
    <div .container.is-fluid>
      <i .fa.fa-check>
      #{text}
|]

setWarningMessage :: MonadHandler m => Text -> m ()
setWarningMessage text = setMessage [shamlet|
  <section .hero.is-warning.is-bold #message>
    <div .container.is-fluid>
      <i .fa.fa-exclamation-circle>
      #{text}
|]

setErrorMessage :: MonadHandler m => Text -> m ()
setErrorMessage text = setMessage [shamlet|
  <section .hero.is-danger.is-bold #message>
    <div .container.is-fluid>
      <i .fa.fa-exclamation-circle>
      #{text}
|]

showKey :: (ToBackendKey SqlBackend a) => Key a -> Text
showKey k = pack $ show $ fromSqlKey k

printTime :: UTCTime -> IO String
printTime t = do
  now <- getCurrentTime
  let secs = diffUTCTime now t
  if secs < 60
    then pure $ show (floor secs) <> " seconds ago"
  else if secs < 60*60
    then pure $ (show $ floor $ secs / 60) <> " minutes ago"
  else if secs < 60*60*24
    then pure $ (show $ floor $ secs / (60 * 60)) <> " hours ago"
  else if secs < 60 * 60 * 23 * 31
    then pure $ (show $ floor $ secs / (60 * 60 * 24)) <> " days ago"
  else pure $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M %Z" t
