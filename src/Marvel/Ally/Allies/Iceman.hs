module Marvel.Ally.Allies.Iceman where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Window qualified as W

iceman :: AllyCard Iceman
iceman = allyWith Iceman Cards.iceman (Thw 1, 1) (Atk 2, 1) (HP 3) (countersL .~ 3)

newtype Iceman = Iceman (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Iceman where
  getAbilities a = [limitedWindowAbility a 1 (W.MinionEntersPlay W.After MinionCanBeStunned) Response OwnsThis UseCost (runAbility a 1)]

instance RunMessage Iceman where
  runMessage msg a@(Iceman attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [mid | W.MinionEnteredPlay mid <- windows] $ \mid -> stun mid attrs
      pure a
    _ -> Iceman <$> runMessage msg attrs
