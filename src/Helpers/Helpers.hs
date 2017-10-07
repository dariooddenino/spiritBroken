{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Helpers.Helpers where

import Import
import Text.Read (readMaybe)
import qualified Database.Esqueleto as E

newAverage :: Int -> Int -> Int -> Int
newAverage vote numVotes avgVote = (vote + (numVotes * avgVote )) `quot` (numVotes + 1)

getPage :: Maybe Text -> Int64
getPage mpage = do
  let page = fromMaybe 1 $ (unpack <$> mpage) >>= readMaybe
  if page >= 0
    then page
    else 0

ceilPages :: Int -> Int -> Int
ceilPages n l = case rem n l of
  0 -> quot n l
  _ -> quot n l + 1

getSiteStats :: Handler (Int, Int, Int, Int)
getSiteStats = runDB $ do
  (numUsers:_) :: [E.Value Int] <-
    E.select . E.from $ \(_ :: E.SqlExpr (Entity User)) -> return E.countRows
  (numEntries:_) :: [E.Value Int] <-
    E.select . E.from $ \(_ :: E.SqlExpr (Entity Entry)) -> return E.countRows
  (numVotes:_) :: [E.Value Int] <-
    E.select . E.from $ \(_ :: E.SqlExpr (Entity Vote)) -> return E.countRows
  (numComments:_) :: [E.Value Int] <-
    E.select . E.from $ \(_ :: E.SqlExpr (Entity Comment)) -> return E.countRows
  pure (E.unValue numUsers, E.unValue numEntries, E.unValue numVotes, E.unValue numComments)
