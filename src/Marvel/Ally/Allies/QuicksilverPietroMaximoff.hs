module Marvel.Ally.Allies.QuicksilverPietroMaximoff (
  quicksilverPietroMaximoff,
  QuicksilverPietroMaximoff (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Modifier
import Marvel.Ref

quicksilverPietroMaximoff :: AllyCard QuicksilverPietroMaximoff
quicksilverPietroMaximoff = ally QuicksilverPietroMaximoff Cards.quicksilverPietroMaximoff (Thw 1, 1) (Atk 2, 1) (HP 4)

newtype QuicksilverPietroMaximoff = QuicksilverPietroMaximoff (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Action: Ready Quicksilver. (Limit once per phase.) Only while he is exhausted: a
-- ready card cannot be readied (RR Target).
instance HasAbilities QuicksilverPietroMaximoff where
  getAbilities (QuicksilverPietroMaximoff a) =
    [limitedAbility a 1 (PerPhase 1) Action OwnsThis NoCost $ runAbility a 1 | allyExhausted a]

instance RunMessage QuicksilverPietroMaximoff where
  runMessage msg q@(QuicksilverPietroMaximoff attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      ready (allyId attrs)
      pure q
    _ -> QuicksilverPietroMaximoff <$> runMessage msg attrs
