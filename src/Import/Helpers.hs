{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Import.Helpers where

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
