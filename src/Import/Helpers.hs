{-# LANGUAGE QuasiQuotes #-}
module Import.Helpers where

import Import.NoFoundation as Import

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

setErrorMessage :: MonadHandler m => Text -> m ()
setErrorMessage text = setMessage [shamlet|
  <section .hero.is-danger.is-bold #message>
    <div .container.is-fluid>
      <i .fa.fa-exclamation-circle>
      #{text}
|]
