module Handler.Helpers where

import Yesod.Auth
import Import
import Data.Time.Clock
import Data.Time.Calendar
import Text.Blaze

showE (FormFailure xs) = mconcat xs
showE FormMissing = "Missing form data"
showE _ = "Success? what are we doing here??"

epoch = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

instance ToMarkup UTCTime where
  toMarkup = toMarkup . show

userField = selectField $ do
            entities <- runDB $ selectList [] [Asc UserName]
            optionsPairs $ map (\e -> (userName $ entityVal e, entityKey e)) entities

domainField = selectField $ do
            entities <- runDB $ selectList [] [Asc DomainName]
            optionsPairs $ map (\e -> (domainName $ entityVal e, entityKey e)) entities

simpleForm form = do
        defaultLayout [whamlet|
          <form method=post>
            ^{form}
            <div>
              <input type=submit>
        |]

resubmit err form = do 
        defaultLayout [whamlet|
               #{showE err}
               <form method=post>
                   ^{form}
                   <div>
                       <input type=submit>
             |]

postEntryR route form entryId = do
  ((res, form), _) <- runFormPost =<< form entryId
  case res of
        FormSuccess e -> do
                runDB $ replace entryId e
                setMessage "Entry updated"
                redirect (route entryId)
        err -> resubmit err form

newEntryR route form = (get,post) where
 get = do
   (widget,_) <- generateFormPost =<< form
   simpleForm widget
 post = do 
   ((res, widget), _) <- runFormPost =<< form
   case res of
     FormSuccess e -> do
         entryId <- runDB $ insert e
         setMessage "Entry inserted"
         redirect (route entryId)
     err -> resubmit err widget


