module Api.Marvel.Types.MultiplayerVariant where

import Prelude

import Data.Text qualified as T
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import Json

data MultiplayerVariant = Solo | WithFriends
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance PersistField MultiplayerVariant where
  toPersistValue = PersistText . T.pack . show
  fromPersistValue (PersistText "Solo") = Right Solo
  fromPersistValue (PersistText "WithFriends") = Right WithFriends
  fromPersistValue _ = Left "invalid multiplayer variant"

instance PersistFieldSql MultiplayerVariant where
  sqlType _ = SqlString
