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
  Domain name desc parent <- runDB $ get404 domainId
  (form, _) <- generateFormPost $ newDomainForm (Just domainId)
  doms <- drawForest <$> treeToString <$> flip forestFrom (Just domainId) <$> runDB (selectList [] [])
  muser <- maybeAuth
  defaultLayout $ [whamlet|
    <html>
      <head>
        <title>Domain #{name}
      <body>
        <title>H1 Domain: #{name}
        Subdomains:
        <pre>
          #{doms}
        <h2> Add direct subdomain
          <form method=post>
             ^{form}
             <div>
               <input type=submit>
    |]  

postDomainR' :: Maybe DomainId -> Handler Html
postDomainR' domainId = do
    ((res, form), _) <- runFormPost $ newDomainForm domainId
    case res of
        FormSuccess d -> do
            domId <- runDB $ insert d
            setMessage "Domain added"
            redirect DomainsR
        _ -> defaultLayout [whamlet|
<form method=post>
    ^{form}
    <div>
        <input type=submit>
|]
 
   


