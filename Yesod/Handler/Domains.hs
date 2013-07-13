{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Domains where

import Yesod.Auth
import Data.Tree
import Data.Text (unpack)
import Import

treeToString :: Forest Dom -> Forest String
treeToString = fmap (fmap ((unpack . domainName . entityVal)))

newDomainForm parent = renderDivs $ Domain
   <$> areq textField "Name" Nothing
   <*> areq textareaField "Description" Nothing
   <*> pure parent

getDomainsR :: Handler Html
getDomainsR = do
    muser <- maybeAuth
    doms <- drawForest <$> treeToString <$> mkForest <$> runDB (selectList [] [])

    (form, _) <- generateFormPost $ newDomainForm Nothing
    defaultLayout $ [whamlet|
    $doctype 5
    <html>
      <head>
        <title>Domains
      <body>
        <pre>
          #{doms}
        <h2> Add top-level domain
          <form method=post>
             ^{form}
             <div>
               <input type=submit>
    |]

postDomainsR :: Handler Html
postDomainsR = postDomainR' Nothing

getDomainR :: DomainId -> Handler Html
getDomainR domainId = do
  (Domain name desc parent,doms) <- runDB $ do
    d@(Domain name desc parent) <- get404 domainId
    doms <- drawForest <$> treeToString <$> flip forestFrom (Just domainId) <$> selectList [] []
    return (d,doms)
  (form, _) <- generateFormPost $ newDomainForm (Just domainId)
  muser <- maybeAuth
  defaultLayout $ [whamlet|
    <html>
      <head>
        <title>Domain #{name}
      <body>
        <h1> #{name} Domain
        <p>
          #{desc}
        Subdomains:
        <pre>
          #{doms}
        <h2> Add direct subdomain
          <form method=post>
             ^{form}
             <div>
               <input type=submit>
    |]

postDomainR :: DomainId -> Handler Html
postDomainR d = postDomainR' (Just d)  

postDomainR' :: Maybe DomainId -> Handler Html
postDomainR' domainId = do
    ((res, form), _) <- runFormPost $ newDomainForm domainId 
    case res of
        FormSuccess d -> do
            domId <- runDB $ insert d
            setMessage "Domain added"
            redirect (DomainR domId)
        _ -> defaultLayout [whamlet|
<form method=post>
    ^{form}
    <div>
        <input type=submit>
|]
 
   


