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
  ((_, form), _) <- runFormPost $ newDelegForm (entityKey user) (Just userId) Nothing
  ds <- runDB $ do
     ds <- selectList [DelegateSource ==. userId, DelegatePublicly ==. True] []
     forM ds $ \(Delegate _source target dom amount _pub) -> 
       targetName <- maybe "<NO USER>" userName <$> get target
       domName <- maybe "<NO DOM>" domainName <$> get dom
       return (target,targetName,dom,domName,amount)
  defaultLayout [whamlet|
    <html>
      <head>
        <title>User #{ident}
      <body>
        <h1> #{name} 
        <ul>
          <li> permissions = #{show(perm)}
          <li> identifier = #{ident}
          <li> name = #{name}
        <h2> Public delegations of this user
          <ul>
            $forall (f,(target,targetName,dom,domName,amount)) <- ds
              <li> In <a href=@{DomainR dom}>#{domName}
                   To <a href=@{UserR target}>#{targetName} 
                   By #{amount}
        <h2> Delegations from you to this user
        <h2> Public delegations to this user
        <h2> Add delegation (from you) to this user
          <form method=post action=@{DelegateR} >
             ^{form}
             <div>
               <input type=submit>           
    |]



