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


(_,postNewDelegateR) = newEntryR DelegateR $ do
    user <- requireAuth
    return $ delegForm (entityKey user) Nothing Nothing Nothing Nothing

postDelegateR :: DelegateId -> Handler Html
postDelegateR = postEntryR DelegateR $ \delegateId -> do
    user <- requireAuth
    (Delegate source target dom amount public) <- runDB $ get404 delegateId
    when (source /= entityKey user) $ 
       permissionDenied "Attempt to modify another's delegation"
    return $ delegForm (entityKey user) (Just target) (Just dom) (Just amount) (Just public)

getDelegateR :: DelegateId -> Handler Html
getDelegateR delegateId = do
    user <- requireAuth
    (Delegate source target dom amount public) <- runDB $ get404 delegateId
    when (source /= entityKey user) $ permissionDenied "You do not own this delegation"
    (form,_)  <- generateFormPost $ delegForm (entityKey user) (Just target) (Just dom) (Just amount) (Just public)
    simpleForm form


 
