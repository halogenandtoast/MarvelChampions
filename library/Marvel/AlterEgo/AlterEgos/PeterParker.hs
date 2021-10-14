module Marvel.AlterEgo.AlterEgos.PeterParker where

import Marvel.Prelude

import Marvel.AlterEgo.Attrs
import Marvel.GameValue
import Marvel.Message
import Marvel.AlterEgo.Cards qualified as Cards

peterParker :: AlterEgoCard PeterParker
peterParker =
  alterEgo PeterParker Cards.peterParker (HP $ Static 10) (HandSize 6) (Rec 3)

newtype PeterParker = PeterParker AlterEgoAttrs
  deriving anyclass IsAlterEgo
  deriving newtype (Show, Eq, HasStartingHP, HasIdentityAttrs, ToJSON, FromJSON)

instance RunMessage PeterParker where
  runMessage msg (PeterParker attrs) = PeterParker <$> runMessage msg attrs
