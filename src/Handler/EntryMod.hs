{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.EntryMod where

import Import
import Handler.Entry (voteForm, commentForm)
import Helpers.Form (runMultipleFormsPost, FormAndHandler(..), entryVoteFormIdent, entryCommentFormIdent)
import Models.Vote (getUserVoteEntity)
import Helpers.Helpers (newAverage)

postEntryModR :: EntryId -> Handler Html
postEntryModR entryId = do
  (uid, _) <- requireAuthPair
  runMultipleFormsPost
    [ FormAndHandler (voteForm (entryVoteFormIdent entryId) uid Nothing) voteHandler
    , FormAndHandler (commentForm (entryCommentFormIdent entryId) uid) commentHandler
    ]

    where
      voteHandler FormMissing = error "unreachable"
      voteHandler (FormFailure _) = do
        setErrorMessage "There was an error with your vote, please try again."
        redirect $ EntryR entryId
      voteHandler (FormSuccess vote) = do
        entry <- runDB $ get404 entryId
        (uid, user) <- requireAuthPair

        mvote <- getUserVoteEntity (Just uid)entryId
        case mvote of
          Nothing -> do
            let numVotes = entryNumVotes entry + 1
                avgVote = newAverage (voteValue vote) (entryNumVotes entry) (entryAvgVote entry)
                uAvgVote = newAverage (voteValue vote) (userNumVotes user) (userAvgVote user)
            _ <- runDB $ do
              v <- insert vote
              _ <- update entryId [ EntryNumVotes =. numVotes
                                  , EntryAvgVote =. avgVote
                                  ]
              _ <- insert $ EntryVote entryId v
              update uid [ UserNumVotes +=. 1
                         , UserAvgVote =. uAvgVote ]
            setSuccessMessage "Vote successfully added."
            redirect $ EntryR entryId
          Just (Entity vId oldVote) -> do
            let avgVote = (entryAvgVote entry) + (voteValue vote - voteValue oldVote) `quot` (entryNumVotes entry)
                uAvgVote = (userAvgVote user) + (voteValue vote - voteValue oldVote) `quot` (userNumVotes user)
            _ <- runDB $ do
              _ <- update uid [ UserAvgVote =. uAvgVote ]
              _ <- update entryId [ EntryAvgVote =. avgVote ]
              update vId [ VoteValue =. voteValue vote
                         , VoteTimeStamp =. voteTimeStamp vote
                         ]
            setSuccessMessage "Vote successfully modified."
            redirect $ EntryR entryId

      commentHandler FormMissing = error "unreachable"
      commentHandler (FormFailure _) = do
        setErrorMessage "There was an error with your comment, please try again."
        redirect $ EntryR entryId
      commentHandler (FormSuccess comment) = do
        _ <- runDB $ get404 entryId
        (uid, _) <- requireAuthPair
        _ <- runDB $ do
          c <- insert comment
          _ <- update entryId [ EntryNumComments +=. 1]
          _ <- insert $ EntryComment entryId c
          update uid [ UserNumComments +=. 1 ]
        setSuccessMessage "Message added."
        redirect $ EntryR entryId
