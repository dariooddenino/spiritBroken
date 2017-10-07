{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
module Models.Entry where

import Import
import Text.RE.TDFA.String
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))

pagerLimit :: Int64
pagerLimit = 20

isUrlImg :: String -> Bool
isUrlImg s = matched $ s ?=~ [reBI|\.(jpg|png|gif)|]

retrieveComments :: EntryId -> Handler [(Entity Comment, Entity User, Maybe (Entity Vote))]
retrieveComments entryId = runDB $
  E.select $
  E.from $ \ (vote `E.InnerJoin` entryVote `E.RightOuterJoin` entryComment `E.InnerJoin` comment `E.InnerJoin` user) -> do
    E.on $ (user ^. UserId E.==. comment ^. CommentUserId)
      E.&&. (vote E.?. VoteUserId E.==. E.just (user ^. UserId))
    E.on $ (comment ^. CommentId E.==. entryComment ^. EntryCommentComment)
      E.&&. (entryComment ^. EntryCommentEntry E.==. E.val entryId)
    E.on (E.just (entryComment ^. EntryCommentEntry) E.==. entryVote E.?. EntryVoteEntry)
    E.on (entryVote E.?. EntryVoteVote E.==. vote E.?. VoteId)
    E.orderBy [E.desc (comment ^. CommentTimeStamp)]
    return (comment, user, vote)

getEntriesWithUserAndVote :: Maybe UserId -> Int64 -> Handler [(Entity Entry, Maybe (Entity Vote), Entity User)]
getEntriesWithUserAndVote muid page = runDB $
  E.select $
  E.from $ \ (vote `E.InnerJoin` entryVote `E.RightOuterJoin` entry `E.InnerJoin` user) -> do
    E.on (user ^. UserId E.==. entry ^. EntryUserId)
    E.on (E.just (entry ^. EntryId) E.==. entryVote E.?. EntryVoteEntry)
    E.on $ (entryVote E.?. EntryVoteVote E.==. vote E.?. VoteId)
      E.&&. (vote E.?. VoteUserId E.==. E.val muid)
    E.orderBy [E.desc (entry ^. EntryTimeStamp)]
    E.limit pagerLimit
    E.offset ((page - 1) * pagerLimit)
    return (entry, vote, user)
