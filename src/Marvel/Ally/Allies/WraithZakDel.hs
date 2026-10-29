module Marvel.Ally.Allies.WraithZakDel (wraithZakDel, WraithZakDel (..)) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Window qualified as W

wraithZakDel :: AllyCard WraithZakDel
wraithZakDel = ally WraithZakDel Cards.wraithZakDel (Thw 1, 1) (Atk 3, 2) (HP 3)

newtype WraithZakDel = WraithZakDel (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities WraithZakDel where
  getAbilities a =
    [limitedWindowAbility a 1 (W.BoostCardTurnedFaceup W.When EncounterCardWithBoostAbility)
      HeroInterrupt OwnsThis (ExhaustCost <> DamageThisCost 1) (runAbility a 1)]

instance RunMessage WraithZakDel where
  runMessage msg a@(WraithZakDel attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ (headMay [card | W.FlippedBoostCard card _ <- windows]) cancelBoostEffect
      pure a
    _ -> WraithZakDel <$> runMessage msg attrs
