module Marvel.Minion.Minions.LivingLaser (livingLaser, LivingLaser (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Keyword (Keyword (Piercing))
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

livingLaser :: MinionCard LivingLaser
livingLaser = minion LivingLaser Cards.livingLaser (Sch 1) (Atk 2) (HP 4)

newtype LivingLaser = LivingLaser (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor LivingLaser where
  getModifiersFor _ (MinionRef mid) (LivingLaser attrs)
    | mid == minionId attrs = pure [KeywordModifier Piercing]
  getModifiersFor _ _ _ = pure []

instance RunMessage LivingLaser where
  runMessage msg (LivingLaser attrs) = runQueueT $ LivingLaser <$> runMessage msg attrs
