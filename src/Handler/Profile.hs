{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Profile where

import Import
import Data.Monoid (Sum(..))
import Database.Esqueleto ((^.))
import qualified Database.Esqueleto as E

nameSettings :: Text -> FieldSettings master
nameSettings nid = FieldSettings
  { fsLabel = "Change your name"
  , fsAttrs = [("class", "input")]
  , fsTooltip = Nothing
  , fsName = Just "name"
  , fsId = Just nid
  }

nameForm :: Text -> Form Text
nameForm name extra = do
  nId <- newIdent
  (nameRes, nameView) <- mreq textField (nameSettings nId) (Just name)
  let widget = [whamlet|
    #{extra}
    <div .field>
      <label .label>
        ^{fvLabel nameView}
      <div .control>
        ^{fvInput nameView}
  |]
  return (nameRes, widget)


getStats :: UserId -> Handler (Int, Int, Int, Int)
getStats uid = runDB $ do
  votes <- E.select . E.from $ \v -> do
    E.where_ (v ^. VoteUserId E.==. E.val uid)
    return v
  (numComments:_) :: [E.Value Int] <- E.select . E.from $ \(_ :: E.SqlExpr (Entity Comment)) -> return E.countRows
  (numEntries:_) :: [E.Value Int] <- E.select . E.from $ \(_ :: E.SqlExpr (Entity Entry)) -> return E.countRows
  let numVotes = length votes :: Int
      totVotes = getSum $ foldMap (\(Entity _ vote) -> Sum $ voteValue vote) votes
      avgVotes = if numVotes == 0 then 0 else totVotes `quot` numVotes
  return (numVotes, avgVotes, E.unValue numComments, E.unValue numEntries)


title :: User -> Widget
title user = setTitle . toHtml $ userName user <> "'s profile page"

getProfileR :: Handler Html
getProfileR = do
    (uid, user) <- requireAuthPair
    (formWidget, formEnctype) <- generateFormPost $ nameForm (userName user)
    (numVotes, avgVotes, numComments, numEntries) <- getStats uid
    defaultLayout $ do
        title user
        $(widgetFile "profile")

postProfileR :: Handler Html
postProfileR = do
  (uid, user) <- requireAuthPair
  ((result, formWidget), formEnctype) <- runFormPost $ nameForm (userName user)
  (numVotes, avgVotes, numComments, numEntries) <- getStats uid
  case result of
    FormSuccess entry -> do
      (numUsers:_) :: [E.Value Int] <- runDB $ E.select . E.from $ \u -> do
        E.where_ (u ^. UserName E.==. E.val entry)
        return E.countRows
      case numUsers of
        E.Value 0 -> do
          _ <- runDB $ update uid [ UserName =. entry ]
          setSuccessMessage "Name changed!"
          let user' = user { userName = entry }
          let user = user'
          defaultLayout $ do
            title user
            $(widgetFile "profile")
        _ -> do
          setErrorMessage "That name is already used :("
          defaultLayout $ do
            title user
            $(widgetFile "profile")
    _ -> do
      setErrorMessage "There was a problem, try again!"
      defaultLayout $ do
        title user
        $(widgetFile "profile")
