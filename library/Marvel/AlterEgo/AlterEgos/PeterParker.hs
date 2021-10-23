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

deriving newtype instance HasIdentityAttrs PeterParker
deriving newtype instance RunMessage PeterParker
