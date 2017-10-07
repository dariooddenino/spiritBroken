module Models.Vote where

import Import
import qualified Database.Esqueleto as E

getUserVoteEntity :: Maybe (Key User) -> Key Entry -> Handler (Maybe (Entity Vote))
getUserVoteEntity muId eId = do
  votes <- runDB $
    E.select $
    E.from $ \ (vote `E.InnerJoin` entryVote) -> do
      E.on $ (entryVote E.?. EntryVoteVote E.==. vote E.?. VoteId) E.&&.
        (entryVote E.?. EntryVoteEntry E.==. E.just (E.val eId)) E.&&.
        (vote E.?. VoteUserId E.==. E.val muId)
      pure vote
  pure $ join $ listToMaybe votes

getVoteValue :: Maybe (Entity Vote) -> Maybe Int
getVoteValue Nothing = Nothing
getVoteValue (Just (Entity _ vote)) = Just $ voteValue vote
