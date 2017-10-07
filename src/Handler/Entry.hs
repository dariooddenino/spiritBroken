{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Entry where

import Import
import Models.Entry (retrieveComments)
import Models.Vote (getUserVoteEntity, getVoteValue)
import Helpers.Form (rangeField, rangeSettings, areaField, areaSettings, entryVoteFormIdent, entryCommentFormIdent)

voteForm' :: UserId -> Maybe Int -> Form Vote
voteForm' userId mVote extra = do
  vId <- newIdent
  time <- liftIO getCurrentTime
  (valueRes, valueView) <- mreq rangeField (rangeSettings vId) mVote
  let voteRes = (\v -> Vote v userId time) <$> valueRes
  let widget = [whamlet|
    #{extra}
    ^{fvInput valueView}
  |]
  return (voteRes, widget)

voteForm :: Text -> UserId -> Maybe Int -> Form Vote
voteForm ident uid mVote = identifyForm ident $ voteForm' uid mVote

commentForm' :: UserId -> Form Comment
commentForm' userId extra = do
  aid <- newIdent
  time <- liftIO getCurrentTime
  (messageRes, messageView) <- mreq areaField (areaSettings aid) Nothing
  let comment = (\c -> Comment c userId time) <$> messageRes
  let widget = [whamlet|
    #{extra}
    ^{fvInput messageView}
  |]
  pure (comment, widget)

commentForm :: Text -> UserId -> Form Comment
commentForm ident uid = identifyForm ident $ commentForm' uid

-- generateEntryForms :: Maybe (Key User) -> Maybe (Entity Vote) -> Handler (Maybe (Widget, Enctype), Maybe (Widget, Enctype))
-- generateEntryForms Nothing  _ = pure (Nothing, Nothing)
-- generateEntryForms (Just uId) mEv = do
--   vF <- generateFormPost $ voteForm uId $ getVoteValue mEv
--   cF <- generateFormPost $ commentForm uId
--   pure (Just vF, Just cF)

getEntryR :: EntryId -> Handler Html
getEntryR entryId = do
  (entry, user) <- runDB $ do
    e <- get404 entryId
    u <- get404 $ entryUserId e
    return (e, u)
  let (Entry userId isImage avgVote numVotes numComments _ title url) = entry
      name = userName user
  maid <- maybeAuthId
  mvote <- getUserVoteEntity maid entryId
  commentsAndUAndV <- retrieveComments entryId
  timeStamp <- liftIO $ printTime $ entryTimeStamp entry

  -- (mvF, mcF) <- generateEntryForms maid mvote
  -- let mvForm = renderVoteForm (EntryModR entryId) mvF
  --     mcForm = renderCommentForm (EntryModR entryId) mcF
  mForms <- case maid of
              Nothing -> pure Nothing
              Just uId -> do
                let vIdent = entryVoteFormIdent entryId
                    cIdent = entryCommentFormIdent entryId
                vF <- generateFormPost $ voteForm vIdent uId $ getVoteValue mvote
                cF <- generateFormPost $ commentForm cIdent uId
                pure $ Just (vF, cF)

  defaultLayout $ do
    setTitle $ toHtml $ "Entry #" <> showKey entryId
    $(widgetFile "entry")

displayComment :: Entity Comment -> Entity User -> Maybe (Entity Vote) -> Widget
displayComment cE uE mvE = do
  let (Entity _ comment) = cE
      (Entity uId user) = uE
      mVote = (\(Entity _ vote) -> voteValue vote) <$> mvE
  timeStamp <- liftIO $ printTime $ commentTimeStamp comment
  $(widgetFile "comment")

-- renderCommentForm :: Route App -> Maybe (Widget, Enctype) -> Widget
-- renderCommentForm _ Nothing = pure ()
-- renderCommentForm route (Just (widget, enctype)) = [whamlet|
--   <article .media>
--     <div .media-content>
--       <form method=post action=@{route} enctype=#{enctype}>
--         ^{widget}
--         <button .comment.button.is-fullwidth.is-dark>
--           Comment
-- |]

-- renderVoteForm :: Route App -> Maybe (Widget, Enctype) -> Widget
-- renderVoteForm _ Nothing = pure ()
-- renderVoteForm route (Just (widget, enctype)) = [whamlet|
--   <div .columns>
--     <div .column>
--       <form method=post action=@{route} enctype=#{enctype}>
--         ^{widget}
--         <button .vote.button.is-fullwidth.is-dark>
--           Vote
-- |]

setPageTitle :: (ToBackendKey SqlBackend a) => Key a -> Widget
setPageTitle entryId = setTitle $ toHtml $ "Entry #" ++ showKey entryId
