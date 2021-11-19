module Marvel.AlterEgo.AlterEgos.CarolDanvers where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo.Attrs
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Message
import Marvel.Stats

carolDanvers :: AlterEgoCard CarolDanvers
carolDanvers =
  alterEgo CarolDanvers Cards.carolDanvers (HP $ Static 12) (HandSize 6) (Rec 4)

newtype CarolDanvers = CarolDanvers AlterEgoAttrs
  deriving anyclass IsAlterEgo
  deriving newtype (Show, Eq, HasStartingHP, HasHandSize, ToJSON, FromJSON, HasCardCode, Entity)

instance HasAbilities CarolDanvers where
  getAbilities _ = []

instance RunMessage CarolDanvers where
  runMessage _ = pure
