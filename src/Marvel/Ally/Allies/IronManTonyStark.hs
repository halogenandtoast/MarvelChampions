module Marvel.Ally.Allies.IronManTonyStark (
  ironManTonyStark,
  IronManTonyStark (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Modifier

ironManTonyStark :: AllyCard IronManTonyStark
ironManTonyStark = ally IronManTonyStark Cards.ironManTonyStark (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype IronManTonyStark = IronManTonyStark (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Reduce the cost to play each upgrade on Iron Man by 1.
instance HasModifiersFor IronManTonyStark where
  getModifiersFor _ target (IronManTonyStark a)
    | isTarget a target = pure [UpgradeOnThisCostReduction 1]
  getModifiersFor _ _ _ = pure []

instance RunMessage IronManTonyStark where
  runMessage msg (IronManTonyStark a) = IronManTonyStark <$> runMessage msg a
