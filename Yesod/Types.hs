module Types where

import Prelude
import Database.Persist.TH

data UserPermissions = Administrator | RegularUser
     deriving (Show, Read, Eq)
derivePersistField "UserPermissions"

data IssueStatus = Draft | VoteOngoing | Closed
     deriving (Show, Read, Eq)
derivePersistField "IssueStatus"
data ProposalStatus = Enabled | Disabled | Accepted 
     deriving (Show, Read, Eq)
derivePersistField "ProposalStatus"

