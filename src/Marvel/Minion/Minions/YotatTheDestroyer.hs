module Marvel.Minion.Minions.YotatTheDestroyer (
  yotatTheDestroyer,
  YotatTheDestroyer (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

-- | Guard and retaliate 1 are his keywords.
yotatTheDestroyer :: MinionCard YotatTheDestroyer
yotatTheDestroyer = minion YotatTheDestroyer Cards.yotatTheDestroyer (Sch 1) (Atk 3) (HP 5)

newtype YotatTheDestroyer = YotatTheDestroyer (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage YotatTheDestroyer where
  runMessage msg (YotatTheDestroyer attrs) = YotatTheDestroyer <$> runMessage msg attrs
