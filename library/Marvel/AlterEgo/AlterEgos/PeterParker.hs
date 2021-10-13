module Marvel.AlterEgo.AlterEgos.PeterParker where

import Marvel.Prelude

import Marvel.AlterEgo.Attrs
import Marvel.AlterEgo.Cards qualified as Cards

peterParker :: IdentityId -> PeterParker
peterParker =
  alterEgo PeterParker Cards.peterParker (HP 10) (HandSize 6) (Rec 3)

newtype PeterParker = PeterParker AlterEgoAttrs
  deriving anyclass IsAlterEgo
  deriving newtype (Show, Eq, HasStartingHP, HasIdentityAttrs)