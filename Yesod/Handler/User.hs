{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.User where

import Yesod.Auth
import Data.Text (unpack)
import Import
import Handler.Delegs

getUserR :: UserId -> Handler Html
getUserR userId = do
  user <- requireAuth
  (User ident pass perm name,ds) <- runDB $ 
    (,) <$> get404 userId
        <*> selectList [DelegateSource ==. userId, DelegatePublicly ==. True] []
  ((_, form), _) <- runFormPost $ newDelegForm (entityKey user) (Just userId) Nothing

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
            $forall Delegate _source target <- ds
              <li> In domain ... to ... (weight)
        <h2> Add delegation to this user
          <form method=post action=@{DelegateR} >
             ^{form}
             <div>
               <input type=submit>           
    |]



