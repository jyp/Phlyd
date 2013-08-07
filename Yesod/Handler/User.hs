{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.User where

import Yesod.Auth
import Data.Text (unpack)
import Import
import Handler.Delegs

getUserR :: UserId -> Handler Html
getUserR userId = do
  user <- requireAuth
  User ident pass perm name <- runDB $ get404 userId
  ((_, form), _) <- runFormPost $ delegForm (entityKey user) (Just userId) Nothing Nothing Nothing
  ds <- runDB $ do
     ds <- selectList [DelegateSource ==. userId, DelegatePublicly ==. True] []
     forM (map entityVal ds) $ \(Delegate _source target dom amount _pub) -> do
       targetName <- maybe "<NO USER>" userName <$> get target
       domName <- maybe "<NO DOM>" domainName <$> get dom
       return (target,targetName,dom,domName,amount)
  defaultLayout [whamlet|
    <html>
      <head>
        <title>User #{name}
      <body>
        <h1> #{name} 
        <ul>
          <li> permissions = #{show perm}

        <h3> Public delegations of this user

        <ul>
            $forall (target,targetName,dom,domName,amount) <- ds
              <li> in domain 
                   <a href=@{DomainR dom}> #{domName} 
                   to 
                   <a href=@{UserR target}> #{targetName} 
                   by #{amount}

        <h3> Public delegations to this user
        TODO

        <h3> Delegations from you to this user
        TODO

        <h3> Add delegation (from you) to this user
        <form method=post action=@{NewDelegateR} >
             ^{form}
             <div>
               <input type=submit>           
    |]



