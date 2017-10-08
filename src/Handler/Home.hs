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
import qualified Database.Esqueleto as E
import Models.Entry (getEntriesWithUserAndVote, pagerLimit)
import Helpers.Helpers (getPage, ceilPages, getSiteStats)

displayEntry :: Entity Entry -> Maybe (Entity Vote) -> Entity User -> Widget
displayEntry (Entity id (Entry userId isImage avgVote numVotes numComments timeStamp title url _)) voteE (Entity _ user) = do
  let mValue = case voteE of
        Nothing -> Nothing
        Just (Entity _ m) -> Just $ voteValue m
      name = userName user
  time <- liftIO $ printTime timeStamp
  $(widgetFile "entry-line")


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
  entriesNVotes <- getEntriesWithUserAndVote maid page
  (numUsers, numEntries, numVotes, numComments) <- getSiteStats
  defaultLayout $ do
    setTitle "SpiritBroken!"
    $(widgetFile "homepage")
