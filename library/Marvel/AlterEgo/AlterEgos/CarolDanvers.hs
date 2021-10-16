module Marvel.AlterEgo.AlterEgos.CarolDanvers where

import Marvel.Prelude

import Marvel.AlterEgo.Attrs
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.GameValue
import Marvel.Message

carolDanvers :: AlterEgoCard CarolDanvers
carolDanvers =
  alterEgo CarolDanvers Cards.carolDanvers (HP $ Static 12) (HandSize 6) (Rec 4)

newtype CarolDanvers = CarolDanvers AlterEgoAttrs
  deriving anyclass IsAlterEgo
  deriving newtype (Show, Eq, HasStartingHP, ToJSON, FromJSON)

instance HasIdentityAttrs CarolDanvers where
  identityAttrsL = lens
    (\(CarolDanvers attrs) -> view identityAttrsL attrs)
    \(CarolDanvers attrs) x -> CarolDanvers $ set identityAttrsL x attrs

instance RunMessage CarolDanvers where
  runMessage msg (CarolDanvers x) = CarolDanvers <$> runMessage msg x
