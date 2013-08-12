module Handler.Vote where

import Import
import Types
import Handler.Helpers
import Data.Text (pack)

voteForm user prop rank = renderDivs $ Vote user prop
  <$> areq (radioFieldList ranks) "Your choice" rank
    where ranks = [(pack $ show x,x) | x <- [StrongReject .. StrongAccept]]

postNewVoteR proposalId = snd $ newEntryR VoteR $ do
    userId <- entityKey <$> requireAuth
    issue <- runDB $ do
      proposal <- get404 $ proposalId
      get404 $ proposalAddresses proposal
    when (issueStatus issue >= Closed) $ do
      permissionDenied "Cannot vote on closed issue"
    return $ voteForm userId proposalId Nothing

getVoteR :: VoteId -> Handler Html
getVoteR voteId = do
  userId <- requireAuthId
  (Vote authorId propId rank) <- runDB $ get404 voteId
  prop <- runDB $ get404 propId
  issue <- runDB $ get404 $ proposalAddresses prop
  when (authorId /= userId) $ permissionDenied "Attempt to see another's vote"
  (widget,_) <- generateFormPost $ voteForm userId propId (Just rank)
  simpleForm widget
      
postVoteR :: VoteId -> Handler Html
postVoteR = postEntryR VoteR $ \voteId -> do
  userId <- requireAuthId
  (Vote authorId propId rank) <- runDB $ get404 voteId
  prop <- runDB $ get404 propId
  issue <- runDB $ get404 $ proposalAddresses prop
  when (authorId /= userId) $ permissionDenied "Attempt to modify another's vote"
  when (issueStatus issue >= Closed) $ permissionDenied "Cannot vote on closed issue"
  return $ voteForm userId propId (Just rank)
  

    


