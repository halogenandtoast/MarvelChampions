{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Entity.Marvel.Game (
  module Entity.Marvel.Game,
) where

import Prelude

import Api.Marvel.Types.MultiplayerVariant
import Data.Aeson.Diff
import Data.Aeson.Types
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID
import Database.Persist.Postgresql.JSON ()
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics (Generic)
import Json
import Marvel.Game
import Marvel.Message
import Orphans ()

data Step = Step
  { stepPatchUp :: Patch
  , stepPatchDown :: Patch
  , stepMessages :: [Message]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance PersistFieldSql [Step] where
  sqlType _ = SqlString

instance PersistField [Step] where
  toPersistValue = toPersistValue . toJSON
  fromPersistValue val =
    fromPersistValue val >>= fmapLeft T.pack . parseEither parseJSON
   where
    fmapLeft f (Left a) = Left (f a)
    fmapLeft _ (Right a) = Right a -- Rewrap to fix types.

share
  [mkPersist sqlSettings]
  [persistLowerCase|
MarvelGame sql=marvel_games
  Id UUID default=uuid_generate_v4()
  name Text
  currentData Game
  steps [Step]
  log [Text]
  multiplayerVariant MultiplayerVariant
  deriving Generic Show
|]

instance ToJSON MarvelGame where
  toJSON = genericToJSON $ aesonOptions $ Just "marvelGame"
  toEncoding = genericToEncoding $ aesonOptions $ Just "marvelGame"
