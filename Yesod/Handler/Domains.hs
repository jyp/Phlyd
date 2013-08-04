{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Domains where

import Yesod.Auth
import Data.Tree
import Data.Text (unpack,pack)
import Import
import qualified Text.Blaze.Html5 as H

getDomainsR :: Handler Html
getDomainsR = getDomainR' Nothing

postDomainsR :: Handler Html
postDomainsR = postDomainR' Nothing

getDomainR :: DomainId -> Handler Html
getDomainR = getDomainR' . Just

postDomainR :: DomainId -> Handler Html
postDomainR d = postDomainR' (Just d)  

domToDesc render d = case entityVal d of 
  (Domain name desc _parent) -> [
    [hamlet| <a href=@{DomainR (entityKey d)}>#{name} |] render
   ]

forestTable :: Forest [Html] -> Html
forestTable = H.table . forestTable' ""

forestTable' :: String -> Forest [Html] -> Html
forestTable' padding rows = 
  forM_ (zip [1..] rows) $ 
    \(n,Node (col0:cols) subrows) -> H.tr $ do
       let pad = show n ++ "." ++ padding
       H.td $ do toHtml pad
                 col0
       forM cols H.td 
       forestTable' pad subrows

newDomainForm parent = renderDivs $ Domain
   <$> areq textField "Name" Nothing
   <*> areq textareaField "Description" Nothing
   <*> pure parent

{-
newDelegForm dom from = renderDivs $ Delegation from 
   <$> areq textField "Delegate" Nothing
   <*> pure dom
   <$> areq intField "Proportion" (Just 10)
   <*> areq boolField "Make this delegation publicly visible" Nothing
   <*> pure parent
-}

getDomainR' :: Maybe DomainId -> Handler Html
getDomainR' mDomainId = do
  user <- requireAuth
  (mDomain,doms,delegs) <- runDB $ do
    d <- mapM get404 mDomainId
    doms <- flip forestFrom mDomainId <$> selectList [] []
    delegs <- selectList [DelegateSource ==. entityKey user] []
    return (d,doms,delegs)
  (form, _) <- generateFormPost $ newDomainForm mDomainId
  let (name,desc) = case mDomain of 
                           Just (Domain name desc _) -> (name,desc)
                           Nothing -> ("TOP LEVEL",Textarea "")
  render <- getUrlRenderParams
  defaultLayout [whamlet|
    <html>
      <head>
        <title> #{name} Domain
      <body>
        <h1> #{name} Domain
        <p> #{desc}
        <p> Subdomains:
        #{forestTable $ fmap (fmap (domToDesc render)) doms}

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
            redirect (DomainR domId)
        _ -> defaultLayout [whamlet|
             <form method=post>
                 ^{form}
                 <div>
                     <input type=submit>
             |]
 
   


