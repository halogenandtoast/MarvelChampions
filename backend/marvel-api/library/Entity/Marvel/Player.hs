{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Entity.Marvel.Player
  ( module Entity.Marvel.Player
  ) where

import Data.UUID
import Database.Persist.TH
import Entity.Marvel.Game
import Entity.User
import Json
import Orphans ()
import Relude

mkPersist sqlSettings [persistLowerCase|
MarvelPlayer sql=marvel_players
  userId UserId OnDeleteCascade
  marvelGameId MarvelGameId OnDeleteCascade
  identityId UUID
  UniquePlayer userId marvelGameId
  deriving Generic Show
|]

instance ToJSON MarvelPlayer where
  toJSON = genericToJSON $ aesonOptions $ Just "marvelPlayer"
  toEncoding = genericToEncoding $ aesonOptions $ Just "marvelPlayer"
