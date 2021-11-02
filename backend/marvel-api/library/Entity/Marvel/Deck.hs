{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Entity.Marvel.Deck
  ( module Entity.Marvel.Deck
  ) where

import Data.UUID
import Database.Persist.Postgresql.JSON ()
import Database.Persist.TH
import Entity.Marvel.MarvelDBDecklist
import Entity.User
import Json
import Orphans ()
import Relude

share [mkPersist sqlSettings] [persistLowerCase|
MarvelDeck sql=arkham_decks
  Id UUID default=uuid_generate_v4()
  userId UserId OnDeleteCascade
  name Text
  investigatorName Text
  list MarvelDBDecklist
  deriving Generic Show
|]

instance ToJSON MarvelDeck where
  toJSON = genericToJSON $ aesonOptions $ Just "marvelDeck"
  toEncoding = genericToEncoding $ aesonOptions $ Just "marvelDeck"
