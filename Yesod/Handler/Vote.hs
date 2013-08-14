module Handler.Vote where

import Import
import Types
import Handler.Helpers
import Data.Text (pack)

voteForm :: UserId -> ProposalId -> Maybe Rank -> Form Vote
voteForm user prop rank = renderDivs $ Vote user prop
  <$> areq (radioFieldList ranks) "Your choice" rank
    where ranks = [(pack $ show x,x) | x <- [StrongReject .. StrongAccept]]

postVoteR proposalId = do
    userId <- requireAuthId
    issue <- runDB $ do
      proposal <- get404 $ proposalId
      get404 $ proposalAddresses proposal
    when (issueStatus issue >= Closed) $ do
      permissionDenied "Cannot vote on closed issue"
    let form = voteForm userId proposalId Nothing
    ((res, form), _) <- runFormPost form
    case res of
        FormSuccess e -> do
           upd <- runDB $ insertBy e
           case upd of
             Left oldVote -> do 
                runDB $ replace (entityKey oldVote) e
                setMessage "Vote updated"
             Right newVoteId -> setMessage "Vote cast"
           redirect (VoteR proposalId)
        err -> resubmit err form    

getVoteR :: ProposalId -> Handler Html
getVoteR propId = do
  userId <- requireAuthId
  rank <- runDB $ ((voteRank . entityVal) <$>) <$> getBy (UniqueVote userId propId)
  (widget,_) <- generateFormPost $ voteForm userId propId rank
  simpleForm widget
    
    


