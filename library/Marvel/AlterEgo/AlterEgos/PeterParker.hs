module Marvel.AlterEgo.AlterEgos.PeterParker where

import Marvel.Prelude

import Marvel.AlterEgo.Attrs
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.GameValue
import Marvel.Message

peterParker :: AlterEgoCard PeterParker
peterParker =
  alterEgo PeterParker Cards.peterParker (HP $ Static 10) (HandSize 6) (Rec 3)

newtype PeterParker = PeterParker AlterEgoAttrs
  deriving anyclass IsAlterEgo
  deriving newtype (Show, Eq, HasStartingHP, ToJSON, FromJSON)

instance HasIdentityAttrs PeterParker where
  identityAttrsL = lens
    (\(PeterParker attrs) -> view identityAttrsL attrs)
    \(PeterParker attrs) x -> PeterParker $ set identityAttrsL x attrs

instance RunMessage PeterParker where
  runMessage msg (PeterParker attrs) = PeterParker <$> runMessage msg attrs
