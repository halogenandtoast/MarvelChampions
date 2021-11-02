module Entity.Marvel.MarvelDBDecklist where

import Relude

import Data.Aeson.Types
import qualified Data.Text as T
import Database.Persist.Postgresql.JSON ()
import Database.Persist.Sql
import Marvel.Card.Code

fmapLeft :: (a -> b) -> Either a c -> Either b c
fmapLeft f (Left a) = Left (f a)
fmapLeft _ (Right a) = Right a -- Rewrap to fix types.

data MarvelDBDecklist = MarvelDBDecklist
  { slots :: Map CardCode Int
  , investigator_code :: CardCode
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance PersistFieldSql MarvelDBDecklist where
  sqlType _ = SqlString

instance PersistField MarvelDBDecklist where
  toPersistValue = toPersistValue . toJSON
  fromPersistValue val =
    fromPersistValue val >>= fmapLeft T.pack . parseEither parseJSON
