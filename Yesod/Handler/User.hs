{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.User where

import Yesod.Auth
import Data.Text (unpack)
import Import

getUserR :: UserId -> Handler Html
getUserR userId = do
  muser <- maybeAuth
  (User ident pass perm name) <- runDB $ get404 userId

  defaultLayout [whamlet|
    <html>
      <head>
        <title>User #{ident}
      <body>
        <h1> #{name} 
        <li>
          <ul> permissions = #{show(perm)}
          <ul> identifier = #{ident}
          <ul> identifier = #{name}
    |]


