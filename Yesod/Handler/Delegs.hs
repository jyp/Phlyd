module Handler.Delegs where

import Yesod.Auth
import Import
import Handler.Helpers

delegForm :: UserId -> Maybe UserId -> Maybe DomainId -> Maybe Int -> Maybe Bool -> Form Delegate
delegForm from to dom proportion public = renderBootstrap $ Delegate from
    <$> areq userField "Delegate" to
    <*> areq domainField "Domain" dom
    <*> areq intField "Proportion" proportion
    <*> areq boolField "Make this delegation publicly visible" public

postNewDelegateR :: Handler Html
postNewDelegateR = do
    user <- requireAuth
    ((res, form), _) <- runFormPost $ delegForm (entityKey user) Nothing Nothing Nothing Nothing
    case res of
        FormSuccess d -> do
            delegId <- runDB $ insert d
            setMessage "Delegation added"
            redirect (DelegateR delegId)
        err -> resubmit err form

postDelegateR :: DelegateId -> Handler Html
postDelegateR delegateId = do
    user <- requireAuth
    (Delegate source target dom amount public) <- runDB $ get404 delegateId
    case source == entityKey user of
       False -> defaultLayout [whamlet| Attempt to modify another's delegation |]
       True -> do
          ((res, form), _) <- runFormPost $ delegForm (entityKey user) (Just target) (Just dom) (Just amount) (Just public)
          case res of
              FormSuccess d -> do
                      runDB $ replace delegateId d
                      setMessage "Delegation updated"
                      redirect (DelegateR delegateId)
              err -> resubmit err form

getDelegateR :: DelegateId -> Handler Html
getDelegateR delegateId = do
    user <- requireAuth
    (Delegate source target dom amount public) <- runDB $ get404 delegateId
    case source == entityKey user of
      True -> do 
        (form,_)  <- generateFormPost $ delegForm (entityKey user) (Just target) (Just dom) (Just amount) (Just public)
        simpleForm form
      False -> defaultLayout [whamlet| You do not own this delegation. |]


 
