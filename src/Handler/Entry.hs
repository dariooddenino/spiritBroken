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
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))
import Database.Esqueleto.Internal.Sql (SqlSelect)
import qualified Data.Text.Read
import Text.Julius (juliusFile)

-- rangeField :: (Monad m, Integral i, RenderMessage (HandlerSite m) FormMessage) => Field m i
rangeField :: Field Handler Int
rangeField = Field
  { fieldParse = parseHelper $ \s ->
      case Data.Text.Read.signed Data.Text.Read.decimal s of
        Right (a, "") -> Right a
        _ -> Left $ MsgInvalidInteger s
  , fieldView = \theId name attrs val isReq -> do
      let myId = String theId
      toWidget [hamlet|
        $newline never
        <input id="#{theId}" name="#{name}" *{attrs} type="range" step=1 :isReq:required="" value="#{showVal val}">
        <output for="#{theId}">
      |]
      toWidget $(juliusFile "templates/slider.julius")
  , fieldEnctype = UrlEncoded
  }
  where
    showVal = either id (pack . showI)
    showI x = show (fromIntegral x :: Integer)

rangeSettings :: Text -> FieldSettings master
rangeSettings vId = FieldSettings
  { fsLabel = "Vote"
  , fsName = Just "value"
  , fsId = Just vId
  , fsTooltip = Nothing
  , fsAttrs = [ ("class", "slider has-output is-fullwidth")
              , ("min", "0")
              , ("max", "1000")
              ]
  }

voteForm' :: EntryId -> UserId -> Form Vote
voteForm' entryId userId extra = do
  vId <- newIdent
  time <- liftIO getCurrentTime
  (valueRes, valueView) <- mreq rangeField (rangeSettings vId) (Just 500)
  let voteRes = (\v -> Vote v entryId userId time) <$> valueRes
  let widget = [whamlet|
    #{extra}
    ^{fvInput valueView}
  |]
  return (voteRes, widget)

voteForm :: EntryId -> UserId -> Form Vote
voteForm eid uid = identifyForm "vote" $ voteForm' eid uid

areaField :: Field Handler Text
areaField = Field
  { fieldParse = parseHelper Right
  , fieldView = \theId name attrs _ isReq ->
      [whamlet|
        $newline never
        <textarea id="#{theId}" name="#{name}" *{attrs} :isReq:required="">
      |]
  , fieldEnctype = UrlEncoded
  }


areaSettings :: Text -> FieldSettings App
areaSettings aid = FieldSettings
  { fsLabel = "Add a comment"
  , fsName = Just "comment"
  , fsId = Just aid
  , fsTooltip = Nothing
  , fsAttrs = [ ("class", "textarea")
              , ("placeholder", "Write something...")
              ]}

commentForm' :: EntryId -> UserId -> Form Comment
commentForm' entryId userId extra = do
  aid <- newIdent
  time <- liftIO getCurrentTime
  (messageRes, messageView) <- mreq areaField (areaSettings aid) Nothing
  let comment = (\c -> Comment c userId entryId time) <$> messageRes
  let widget = [whamlet|
    #{extra}
    ^{fvInput messageView}
  |]
  pure (comment, widget)

commentForm :: EntryId -> UserId -> Form Comment
commentForm eid uid = identifyForm "comment" $ commentForm' eid uid

getUserVote :: Maybe (Key User) -> Key Entry -> Handler (Maybe Int)
getUserVote Nothing _ = pure Nothing
getUserVote (Just uId) eId = do
  vs <- runDB $ E.select $ E.from $ \v -> do
    E.where_ (v ^. VoteUserId E.==. E.val uId)
    E.where_ (v ^. VoteEntryId E.==. E.val eId)
    return v
  case vs of
    [] -> pure Nothing
    (Entity _ vote : _) -> pure $ Just $ voteValue vote

generateEntryForms :: Maybe (Key User) -> Key Entry -> Maybe Int -> Handler (Maybe (Widget, Enctype), Maybe (Widget, Enctype))
generateEntryForms Nothing _ _ = pure (Nothing, Nothing)
generateEntryForms (Just uId) eId Nothing = do
  vF <- generateFormPost $ voteForm eId uId
  cF <- generateFormPost $ commentForm eId uId
  pure (Just vF, Just cF)
generateEntryForms (Just uId) eId _ = do
  cF <- generateFormPost $ commentForm eId uId
  pure (Nothing, Just cF)


runEntryForms :: Maybe (Key User) -> Key Entry -> Maybe Int -> Handler (Maybe ((FormResult Vote, Widget), Enctype), Maybe ((FormResult Comment, Widget), Enctype))
runEntryForms Nothing _ _ = pure (Nothing, Nothing)
runEntryForms (Just uId) eId Nothing = do
  vF <- runFormPost $ voteForm eId uId
  cF <- runFormPost $ commentForm eId uId
  pure (Just vF, Just cF)
runEntryForms (Just uId) eId _ = do
  cF <- runFormPost $ commentForm eId uId
  pure (Nothing, Just cF)

renderCommentForm :: Route App -> Maybe (Widget, Enctype) -> Widget
renderCommentForm _ Nothing = pure ()
renderCommentForm route (Just (widget, enctype)) = [whamlet|
  <article .media>
    <div .media-content>
      <form method=post action=@{route} enctype=#{enctype}>
        ^{widget}
        <button .comment.button.is-fullwidth.is-dark>
          Comment
|]

renderVoteForm :: Route App -> Maybe (Widget, Enctype) -> Widget
renderVoteForm _ Nothing = pure ()
renderVoteForm route (Just (widget, enctype)) = [whamlet|
  <div .columns>
    <div .column>
      <form method=post action=@{route} enctype=#{enctype}>
        ^{widget}
        <button .vote.button.is-fullwidth.is-dark>
          Vote
|]

setPageTitle :: (ToBackendKey SqlBackend a) => Key a -> Widget
setPageTitle entryId = setTitle $ toHtml $ "Entry #" ++ showKey entryId

retrieveComments :: EntryId -> Handler [(Entity Comment, Entity User, Maybe (Entity Vote))]
retrieveComments entryId = runDB $
  E.select $
    E.from $ \((c `E.InnerJoin` u) `E.LeftOuterJoin` v) -> do
      E.on $ (v E.?. VoteUserId E.==. E.just (u ^. UserId))
        E.&&. (v E.?. VoteEntryId E.==. E.just (E.val entryId))
      E.on $ (c ^. CommentUserId E.==. u ^. UserId)
        E.&&. (c ^. CommentEntryId E.==. E.val entryId)
      E.orderBy [E.desc (c ^. CommentTimeStamp)]
      return (c, u, v)


getEntryR :: EntryId -> Handler Html
getEntryR entryId = do
  (entry, user) <- runDB $ do
    e <- get404 entryId
    u <- get404 $ entryUserId e
    return (e, u)
  let (Entry userId isImage avgVote numVotes numComments _ title url) = entry
      name = userName user
  maid <- maybeAuthId
  mvote <- getUserVote maid entryId
  commentsAndUAndV <- retrieveComments entryId
  liftIO $ print $ show $ length commentsAndUAndV
  timeStamp <- liftIO $ printTime $ entryTimeStamp entry
  (mvF, mcF) <- generateEntryForms maid entryId mvote
  let mvForm = renderVoteForm (EntryModR entryId) mvF
      mcForm = renderCommentForm (EntryModR entryId) mcF
  defaultLayout $ do
    setPageTitle entryId
    $(widgetFile "entry")

displayComment :: Entity Comment -> Entity User -> Maybe (Entity Vote) -> Widget
displayComment cE uE mvE = do
  let (Entity _ comment) = cE
      (Entity uId user) = uE
      mVote = (\(Entity _ vote) -> voteValue vote) <$> mvE
  timeStamp <- liftIO $ printTime $ commentTimeStamp comment
  $(widgetFile "comment")
