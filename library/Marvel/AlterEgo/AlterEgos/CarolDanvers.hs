module Marvel.AlterEgo.AlterEgos.CarolDanvers where

import Marvel.Prelude

import Marvel.AlterEgo.Attrs
import Marvel.GameValue
import Marvel.Message
import Marvel.AlterEgo.Cards qualified as Cards

carolDanvers :: AlterEgoCard CarolDanvers
carolDanvers =
  alterEgo CarolDanvers Cards.carolDanvers (HP $ Static 12) (HandSize 6) (Rec 4)

newtype CarolDanvers = CarolDanvers AlterEgoAttrs
  deriving anyclass IsAlterEgo
  deriving newtype (Show, Eq, HasStartingHP, HasIdentityAttrs, ToJSON, FromJSON)

instance RunMessage CarolDanvers where
  runMessage msg (CarolDanvers x) = CarolDanvers <$> runMessage msg x
