module Handler.Delegs where

import Yesod.Auth
import Data.Text (unpack)
import Import


newDelegForm from to dom = renderBootstrap $ Delegate from
    <$> areq (selectField users)   "Delegate" to
    <*> areq (selectField domains) "Domain" dom
    <*> areq intField "Proportion" (Just 10)
    <*> areq boolField "Make this delegation publicly visible" (Just False)
    where
        domains = do
            entities <- runDB $ selectList [] [Asc DomainName]
            optionsPairs $ map (\e -> (domainName $ entityVal e, entityKey e)) entities
        users = do
            entities <- runDB $ selectList [] [Asc UserName]
            optionsPairs $ map (\e -> (userName $ entityVal e, entityKey e)) entities

postDelegateR :: Handler Html
postDelegateR = do
    user <- requireAuth
    ((res, form), _) <- runFormPost $ newDelegForm (entityKey user) Nothing Nothing
    case res of
        FormSuccess d -> do
            _delegId <- runDB $ insert d
            setMessage "Delegation added"
            redirect (DomainsR)
        _ -> defaultLayout [whamlet|
<form method=post>
    ^{form}
    <div>
        <input type=submit>
|]
 
