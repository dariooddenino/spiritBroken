{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.EntryMod where

import Import
import Handler.Entry (voteForm, commentForm, getUserVote)

data FormAndHandler = forall a. FormAndHandler (Form a) (FormResult a -> Handler Html)

newAverage :: Int -> Int -> Int -> Int
newAverage vote numVotes avgVote = (vote + (numVotes * avgVote )) `quot` (numVotes + 1)

runMultipleFormsPost :: [FormAndHandler] -> Handler Html
runMultipleFormsPost [] = redirect HomeR
runMultipleFormsPost (FormAndHandler form handler : t) = do
  ((res, _), _) <- runFormPost form
  case res of
    FormMissing ->
      runMultipleFormsPost t
    _ ->
      handler res

postEntryModR :: EntryId -> Handler Html
postEntryModR entryId = do
  (uid, _) <- requireAuthPair
  runMultipleFormsPost
    [ FormAndHandler (voteForm entryId uid) voteHandler
    , FormAndHandler (commentForm entryId uid) commentHandler
    ]

    where
      voteHandler FormMissing = error "unreachable"
      voteHandler (FormFailure _) = do
        setErrorMessage "There was an error with your vote, please try again."
        redirect $ EntryR entryId
      voteHandler (FormSuccess vote) = do
        entry <- runDB $ get404 entryId
        (uid, user) <- requireAuthPair
        mvote <- getUserVote (Just uid) entryId
        case mvote of
          Nothing -> do
            let numVotes = entryNumVotes entry + 1
                avgVote = newAverage (voteValue vote) (entryNumVotes entry) (entryAvgVote entry)
                uAvgVote = newAverage (voteValue vote) (userNumVotes user) (userAvgVote user)
            _ <- runDB $ do
              _ <- insert vote
              _ <- update entryId [ EntryNumVotes =. numVotes
                                  , EntryAvgVote =. avgVote
                                  ]
              update uid [ UserNumVotes +=. 1
                         , UserAvgVote =. uAvgVote ]
            setSuccessMessage "Vote successfully added."
            redirect $ EntryR entryId
          _ -> do
            setWarningMessage "Already voted."
            redirect $ EntryR entryId

      commentHandler FormMissing = error "unreachable"
      commentHandler (FormFailure _) = do
        setErrorMessage "There was an error with your comment, please try again."
        redirect $ EntryR entryId
      commentHandler (FormSuccess comment) = do
        _ <- runDB $ get404 entryId
        (uid, _) <- requireAuthPair
        _ <- runDB $ do
          _ <- insert comment
          _ <- update entryId [ EntryNumComments +=. 1]
          update uid [ UserNumComments +=. 1 ]
        setSuccessMessage "Message added."
        redirect $ EntryR entryId
