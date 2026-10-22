module Marvel.Ally.Allies.IronManTonyStark (
  ironManTonyStark,
  IronManTonyStark (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

-- | ponytail: "Reduce the cost to play each upgrade on Iron Man by 1" is not applied: an
-- upgrade's ally is chosen after its cost is paid, so the reduction cannot depend on it;
-- add it once upgrades played on a character choose it before paying.
ironManTonyStark :: AllyCard IronManTonyStark
ironManTonyStark = ally IronManTonyStark Cards.ironManTonyStark (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype IronManTonyStark = IronManTonyStark (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage IronManTonyStark where
  runMessage msg (IronManTonyStark a) = IronManTonyStark <$> runMessage msg a
