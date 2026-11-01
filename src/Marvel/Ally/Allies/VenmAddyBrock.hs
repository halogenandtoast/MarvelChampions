module Marvel.Ally.Allies.VenmAddyBrock where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Payment ()
import Marvel.Question ()
import Marvel.Window qualified as W

venmAddyBrock :: AllyCard VenmAddyBrock
venmAddyBrock = ally VenmAddyBrock Cards.venmAddyBrock (Thw 1, 1) (Atk 1, 1) (HP 3)

newtype VenmAddyBrock = VenmAddyBrock (Attrs Ally)
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor VenmAddyBrock where
  getModifiersFor _ target (VenmAddyBrock attrs) = pure [m | isTarget attrs target, m <- [ThwartModifier $ allyCounters attrs, AttackModifier $ allyCounters attrs]]

instance HasAbilities VenmAddyBrock where
  getAbilities a@(VenmAddyBrock attrs) = [limitedWindowAbility a 1 W.EntersPlayThis HeroResponse OwnsThis NoCost (runAbility a 1) | syncRatioResources (allyPaidWith attrs) > 0]

instance RunMessage VenmAddyBrock where
  runMessage msg e@(VenmAddyBrock attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      placeCounters attrs (syncRatioResources $ allyPaidWith attrs)
      pure e
    _ -> VenmAddyBrock <$> runMessage msg attrs
