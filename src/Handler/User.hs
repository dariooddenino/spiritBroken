{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.User where

import Import
import Data.Monoid (Sum(..))
import Database.Esqueleto ((^.))
import qualified Database.Esqueleto as E

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

getUserR :: UserId -> Handler Html
getUserR userId = do
  user <- runDB $ get404 userId
  (numVotes, avgVotes, numComments, numEntries) <- getStats userId
  defaultLayout $ do
    setTitle . toHtml $ userName user <> "'s profile page"
    $(widgetFile "user")
