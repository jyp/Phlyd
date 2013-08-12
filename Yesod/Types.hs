module Types where

import Prelude
import Database.Persist.TH

data UserPermissions = Administrator | RegularUser
     deriving (Show, Read, Eq)
derivePersistField "UserPermissions"

data IssueStatus = Draft | Frozen | VoteOngoing | Closed
     deriving (Show, Read, Eq, Ord)
derivePersistField "IssueStatus"

data Rank = StrongReject | Reject | WeakReject | Borderline | WeakAccept | Accept | StrongAccept
     deriving (Show, Read, Eq, Ord, Enum)
derivePersistField "Rank"

