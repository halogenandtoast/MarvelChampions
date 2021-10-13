module Marvel.Villain.Villains.Klaw where

import Marvel.Prelude
import Marvel.Villain.Attrs
import Marvel.Villain.Cards qualified as Cards

newtype Klaw = Klaw VillainAttrs
  deriving anyclass IsVillain
  deriving newtype (Show, Eq, ToJSON, FromJSON)

klaw :: VillainCard Klaw
klaw = villain Klaw Cards.klaw
