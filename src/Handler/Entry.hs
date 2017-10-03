{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Handler.Entry where

import Import
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))
import Database.Esqueleto.Internal.Sql (SqlSelect)
import qualified Data.Text.Read
import Text.Julius (juliusFile)

rangeField :: (Monad m, Integral i, RenderMessage (HandlerSite m) FormMessage) => Field m i
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

voteForm :: EntryId -> UserId -> Form Vote
voteForm entryId userId extra = do
  vId <- newIdent
  time <- liftIO getCurrentTime
  (valueRes, valueView) <- mreq rangeField (rangeSettings vId) (Just 500)
  let voteRes = (\v -> Vote v entryId userId time) <$> valueRes
  let widget = [whamlet|
    #{extra}
    ^{fvInput valueView}
  |]
  return (voteRes, widget)

renderForm :: Route App -> Enctype -> Widget -> Widget
renderForm route enctype widget = [whamlet|
  <div .columns>
    <div .column>
      <form method=post action=@{route} enctype=#{enctype}>
        ^{widget}
        <button .button.is-fullwidth.is-dark>Vote
|]

getVotes :: (SqlSelect a (Entity Vote), MonadIO m) => Key User -> Key Entry -> E.SqlReadT m [Entity Vote]
getVotes mid entryId = E.select $ E.from $ \v -> do
  E.where_ (v ^. VoteUserId E.==. E.val mid)
  E.where_ (v ^. VoteEntryId E.==. E.val entryId)
  return v

getEntryR :: EntryId -> Handler Html
getEntryR entryId = do
  (Entry userId isImage avgVote numVotes numComments timeStamp title url) <- runDB $ get404 entryId
  maid <- maybeAuthId
  case maid of
    Nothing -> defaultLayout $ do
      let value = 0 :: Int
          voted = False
          mForm = Nothing :: Maybe Widget
      setPageTitle entryId
      $(widgetFile "entry")
    Just mid -> do
      (formWidget, formEnctype) <- generateFormPost $ voteForm entryId mid
      votes <- runDB $ getVotes mid entryId
      case votes of
        [] -> defaultLayout $ do
          let value = 0 :: Int
              voted = False
              mForm = Just $ renderForm (EntryR entryId) formEnctype formWidget
          setPageTitle entryId
          $(widgetFile "entry")
        (Entity _ (Vote value _ _ _): _) -> defaultLayout $ do
          let voted = True
              mForm = Nothing :: Maybe Widget
          setPageTitle entryId
          $(widgetFile "entry")

newAverage :: Int -> Int -> Int -> Int
newAverage vote numVotes avgVote = (vote + (numVotes * avgVote )) `quot` (numVotes + 1)

postEntryR :: EntryId -> Handler Html
postEntryR entryId = do
  entry@(Entry userId isImage avgVote numVotes numComments timeStamp title url) <- runDB $ get404 entryId
  maid <- maybeAuthId
  case maid of
    Just mid -> do
      ((result, formWidget), formEnctype) <- runFormPost $ voteForm entryId mid
      case result of
        FormSuccess vote -> do
          votes <- runDB $ getVotes mid entryId
          liftIO $ print "QUI"
          case votes of
            [] -> do
              let numVotes = entryNumVotes entry + 1
                  avgVote = (voteValue vote + (entryAvgVote entry * entryNumVotes entry)) `quot` numVotes
              _ <- runDB $ do
                insert vote
                update entryId [ EntryNumVotes =. numVotes
                               , EntryAvgVote =. avgVote
                               ]
              setSuccessMessage "Vote successfully added."
              defaultLayout $ do
                let voted = True
                    value = voteValue vote
                    mForm = Nothing :: Maybe Widget
                setPageTitle entryId
                $(widgetFile "entry")
            _ -> do
              setWarningMessage "Already voted."
              defaultLayout $ do
                let voted = True
                    value = voteValue vote
                    mForm = Nothing :: Maybe Widget
                setPageTitle entryId
                $(widgetFile "entry")
        _ -> do
          setErrorMessage "There was an error with your vote, please try again."
          defaultLayout $ do
            let value = 0 :: Int
                voted = False
                mForm = Just $ renderForm (EntryR entryId) formEnctype formWidget
            setTitle $ toHtml $ "Entry #" ++ showKey entryId
            $(widgetFile "entry")
    Nothing -> do
      setErrorMessage "You must be logged in to vote!"
      defaultLayout $ do
        let value = 0 :: Int
            voted = False
            mForm = Nothing :: Maybe Widget
        setPageTitle entryId
        $(widgetFile "entry")


setPageTitle :: (ToBackendKey SqlBackend a) => Key a -> Widget
setPageTitle entryId = setTitle $ toHtml $ "Entry #" ++ showKey entryId
