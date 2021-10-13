module Marvel.AlterEgo.AlterEgos.CarolDanvers where

import Marvel.Prelude

import Marvel.AlterEgo.Attrs
import Marvel.AlterEgo.Cards qualified as Cards

carolDanvers :: IdentityId -> CarolDanvers
carolDanvers =
  alterEgo CarolDanvers Cards.carolDanvers (HP 12) (HandSize 6) (Rec 4)

newtype CarolDanvers = CarolDanvers AlterEgoAttrs
  deriving anyclass IsAlterEgo
  deriving newtype (Show, Eq, HasStartingHP, HasIdentityAttrs)
