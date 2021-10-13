module Marvel.Villain.Villains.Rhino where

import Marvel.Prelude
import Marvel.Villain.Attrs
import Marvel.Villain.Cards qualified as Cards

newtype Rhino = Rhino VillainAttrs
  deriving anyclass IsVillain
  deriving newtype (Show, Eq, ToJSON, FromJSON)

rhino :: VillainCard Rhino
rhino = villain Rhino Cards.rhino
