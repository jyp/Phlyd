module Handler.Proposal where

import Types
import Import
import Handler.Helpers

proposalForm name author addresses publicly description = renderDivs $ Proposal
  <$> areq textField "Name" name
  <*> pure author 
  <*> pure addresses
  <*> areq boolField "Make your authorship public" publicly
  <*> areq textareaField "Description" description
  
postNewProposalR issueId = snd $ newEntryR ProposalR $ do
   userId <- entityKey <$> requireAuth
   issue <- runDB $ get404 issueId
   when (issueStatus issue /= Draft) $ 
     permissionDenied "Can't update frozen issue"
   return $ proposalForm Nothing userId issueId Nothing Nothing

getProposalR :: ProposalId -> Handler Html
getProposalR proposalId = do
  userId <- entityKey <$> requireAuth
  (Proposal name authorId addresses publicly description) <- runDB $ get404 proposalId
  issue <- runDB $ get404 addresses
  author <- runDB $ get404 authorId
  (form,_) <- generateFormPost $ proposalForm (Just name) userId addresses (Just publicly) (Just description)
  let showAuthor = publicly || authorId == userId
      canUpdate = issueStatus issue == Draft && authorId == userId
  defaultLayout [whamlet|
       <html>
         <head>
           <title>Proposal: #{name}
         <body>
           <h1>
              #{name}
           <ul> 
              $if showAuthor
                 <li> created by 
                      <a href=@{UserR authorId}> #{userName author}
              <li> adresses
                 <a href=@{IssueR addresses}> #{issueName issue} 
           #{description}
           $if canUpdate
             <h2> Update Issue
               <form method=post>
                 ^{form}
               <div>
                 <input type=submit>               
    |]

postProposalR :: ProposalId -> Handler Html
postProposalR = postEntryR ProposalR $ \proposalId -> do
  user <- requireAuthId
  (Proposal name authorId addresses publicly description) <- runDB $ get404 proposalId
  issue <- runDB $ get404 addresses
  when (authorId /= user) $ permissionDenied "Attempt to modify another's issue"
  when (issueStatus issue /= Draft) $ permissionDenied "Attempt to modify a frozen issue"  
  return $ proposalForm Nothing user addresses Nothing Nothing
  
