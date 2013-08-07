module Handler.Delegs where

import Yesod.Auth
import Import


newDelegForm from to dom proportion public = renderBootstrap $ Delegate from
    <$> areq (selectField users)   "Delegate" to
    <*> areq (selectField domains) "Domain" dom
    <*> areq intField "Proportion" proportion
    <*> areq boolField "Make this delegation publicly visible" public
    where
        domains = do
            entities <- runDB $ selectList [] [Asc DomainName]
            optionsPairs $ map (\e -> (domainName $ entityVal e, entityKey e)) entities
        users = do
            entities <- runDB $ selectList [] [Asc UserName]
            optionsPairs $ map (\e -> (userName $ entityVal e, entityKey e)) entities


postNewDelegateR :: Handler Html
postNewDelegateR = do
    user <- requireAuth
    ((res, form), _) <- runFormPost $ newDelegForm (entityKey user) Nothing Nothing Nothing Nothing
    case res of
        FormSuccess d -> do
            delegId <- runDB $ insert d
            setMessage "Delegation added"
            redirect (DelegateR delegId)
        _ -> defaultLayout [whamlet|
<form method=post>
    ^{form}
    <div>
        <input type=submit>
|]

postDelegateR :: DelegateId -> Handler Html
postDelegateR delegateId = do
    user <- requireAuth
    (Delegate source target dom amount public) <- runDB $ get404 delegateId
    case source == entityKey user of
       False -> defaultLayout [whamlet| Attempt to modify another's delegation |]
       True -> do
          ((res, form), _) <- runFormPost $ newDelegForm (entityKey user) (Just target) (Just dom) (Just amount) (Just public)
          case res of
              FormSuccess d -> do
                      runDB $ replace delegateId d
                      setMessage "Delegation updated"
                      redirect (DelegateR delegateId)
              err -> defaultLayout [whamlet|
                      #{show err}
                      <form method=post>
                          ^{form}
                          <div>
                              <input type=submit>
                      |]

getDelegateR :: DelegateId -> Handler Html
getDelegateR delegateId = do
    user <- requireAuth
    (Delegate source target dom amount public) <- runDB $ get404 delegateId
    case source == entityKey user of
      True -> do 
        ((_, form), _) <- runFormGet $ newDelegForm (entityKey user) (Just target) (Just dom) (Just amount) (Just public)
        defaultLayout [whamlet|
          <form method=post>
            ^{form}
            <div>
              <input type=submit>
        |]
      False -> defaultLayout [whamlet| You do not own this delegation. |]


 
