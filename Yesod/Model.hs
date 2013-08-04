module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Typeable (Typeable)
import Data.Time (UTCTime)
import Types
import Data.Tree
import Control.Applicative

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

type Dom = Entity Domain

childrenOf :: [Dom] -> Maybe DomainId -> [Dom]
childrenOf doms parent = [ d | d <- doms, domainParent (entityVal d) == parent ] 

forestFrom :: [Dom] -> Maybe DomainId -> Forest Dom
forestFrom doms d = map (treeFrom doms) (doms `childrenOf` d)

treeFrom :: [Dom] -> Dom -> Tree Dom
treeFrom doms d = Node d (doms `forestFrom` (Just $ entityKey d))

mkForest :: [Dom] -> Forest Dom
mkForest doms = forestFrom doms Nothing

isAdminUser :: User -> Bool
isAdminUser = (Administrator ==) . userPermissions
  

