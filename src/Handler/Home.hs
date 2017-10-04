{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Import
import Text.Read (readMaybe)
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

displayEntry :: Entity Entry -> Maybe (Entity Vote) -> Entity User -> Widget
displayEntry (Entity id (Entry userId isImage avgVote numVotes numComments timeStamp title url)) voteE (Entity _ user) = do
  let mValue = case voteE of
        Nothing -> Nothing
        Just (Entity _ m) -> Just $ voteValue m
      name = userName user
  $(widgetFile "entry-line")


pagerLimit :: Int64
pagerLimit = 20
getPage :: Maybe Text -> Int64
getPage mpage = do
  let page = fromMaybe 1 $ (unpack <$> mpage) >>= readMaybe
  if page >= 0
    then page
    else 0

--   n = 50
--   l = 17


ceilPages :: Int -> Int -> Int
ceilPages n l = case rem n l of
  0 -> quot n l
  _ -> quot n l + 1

pager :: Int -> Int64 -> Int64 -> Widget
pager n l c = do
  let cantPrev = c <= 1
      cantNext = fromIntegral (c*l) >= n
      pages = [1..(ceilPages n (fromIntegral l))]
  [whamlet|
  <nav .pagination role="navigation" aria-label="pagination">
    <a .pagination-previous :cantPrev:disabled href=@{HomeR}?page=#{c - 1}>Newer
    <a .pagination-next :cantNext:disabled href=@{HomeR}?page=#{c + 1}>Older
    <ul class="pagination-list">
      $forall page <- pages
        <li>
          <a .pagination-link :page == fromIntegral(c):.is-current href="@{HomeR}?page=#{page}" aria-label="Page #{page}">
            #{page}
|]

getHomeR :: Handler Html
getHomeR = do
  maid <- maybeAuthId
  page <- getPage <$> lookupGetParam "page"
  (entriesNVotes, numUsers, numEntries, numVotes, numComments) <- runDB $ do
    entriesNVotes <- E.select $
      E.from $ \((e `E.LeftOuterJoin` v) `E.InnerJoin` u)-> do
        E.on (e ^. EntryUserId E.==. u ^. UserId)
        E.on $ (E.just (e ^. EntryId) E.==. v E.?. VoteEntryId) E.&&.
          (v E.?. VoteUserId) E.==. E.val maid
        E.orderBy [E.desc (e ^. EntryTimeStamp)]
        E.limit pagerLimit
        E.offset ((page - 1) * pagerLimit)
        return (e, v, u)
      -- E.from $ \(e `E.LeftOuterJoin` v) -> do
      --   E.on $ (E.just (e ^. EntryId) E.==. v E.?. VoteEntryId) E.&&.
      --     (v E.?. VoteUserId) E.==. E.val maid
      --   E.orderBy [E.desc (e ^. EntryTimeStamp)]
      --   E.limit pagerLimit
      --   E.offset ((page - 1) * pagerLimit)
      --   return (e, v)
    (numUsers:_) :: [E.Value Int] <-
      E.select . E.from $ \(_ :: E.SqlExpr (Entity User)) -> return E.countRows
    (numEntries:_) :: [E.Value Int] <-
      E.select . E.from $ \(_ :: E.SqlExpr (Entity Entry)) -> return E.countRows
    (numVotes:_) :: [E.Value Int] <-
      E.select . E.from $ \(_ :: E.SqlExpr (Entity Vote)) -> return E.countRows
    (numComments:_) :: [E.Value Int] <-
      E.select . E.from $ \(_ :: E.SqlExpr (Entity Comment)) -> return E.countRows
    return (entriesNVotes, numUsers, numEntries, numVotes, numComments)
  defaultLayout $ do
    setTitle "SpiritBroken!"
    $(widgetFile "homepage")
