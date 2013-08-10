module Handler.Issue where

import Import
import Handler.Helpers
import Types
import Data.Time.Clock
import Yesod.Auth
import Handler.Proposal

issueForm name author domain publicly description created = renderDivs $ Issue
    <$> areq textField "Name" name
    <*> pure author 
    <*> (mkStatus <$> areq boolField "Freeze (ready to be voted on; CANNOT BE UNDONE)" (Just False))
    <*> areq domainField "Domain" domain
    <*> areq boolField "Make your authorship public" publicly
    <*> areq textareaField "Description" description
    <*> pure created
    <*> pure epoch
    <*> pure epoch
  where mkStatus freeze = if freeze then Frozen else Draft

(getNewIssueR,postNewIssueR) = newEntryR IssueR $ do
   user <- requireAuth
   now <- liftIO $ getCurrentTime
   return $ issueForm Nothing (entityKey user) Nothing Nothing Nothing now

postIssueR :: IssueId -> Handler Html
postIssueR = postEntryR IssueR $ \issueId -> do
  user <- requireAuthId
  now <- liftIO $ getCurrentTime
  (Issue name authorId status domainId publicly description created upForVote closed) <- runDB $ get404 issueId
  when (authorId /= user) $ permissionDenied "Attempt to modify another's issue"
  when (status /= Draft) $ permissionDenied "Attempt to modify a frozen issue"  
  return $ issueForm Nothing user Nothing Nothing Nothing now


getIssueR :: IssueId -> Handler Html
getIssueR issueId = do
  userId <- entityKey <$> requireAuth
  now <- liftIO $ getCurrentTime
  (Issue name authorId status domainId publicly description created upForVote closed) <- runDB $ get404 issueId
  domain <- runDB $ get404 domainId
  author <- runDB $ get404 authorId
  proposals <- runDB $ selectList [ProposalAddresses ==. issueId] []
    
  (form,_) <- generateFormPost $ issueForm (Just name) userId (Just domainId) (Just publicly) (Just description) now
  (proposalForm,_) <- generateFormPost $ proposalForm Nothing userId issueId Nothing Nothing
  let showAuthor = publicly || authorId == userId
      canUpdate = status == Draft && authorId == userId
      frozen = status >= Frozen
  defaultLayout [whamlet|   
       <html>
         <head>
           <title>Issue: #{name}
         <body>
           <h1>
              #{name}
           <ul>
              <li> #{show status}
              $if showAuthor
                 <li> created by 
                      <a href=@{UserR authorId}> #{userName author}
              <li> in domain 
                   <a href=@{DomainR domainId}> #{domainName domain}
              <li> Last updated: #{created}
              $if frozen
                <li> Voting starts: #{upForVote}
                <li> Closing: #{closed}
           #{description}
           <h2> Proposals
           <ul>
              $forall p <- proposals 
                <li> 
                  <a href=@{ProposalR $ entityKey p}> #{proposalName $ entityVal $ p}
           <h2> Add proposal
              <form method=post action=@{NewProposalR issueId}>
                 ^{proposalForm}
               <div>
                 <input type=submit>               
           $if canUpdate
             <h2> Update Issue
               <form method=post>
                 ^{form}
               <div>
                 <input type=submit>               
       |]
 
  


