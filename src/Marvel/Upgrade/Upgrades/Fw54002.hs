{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Upgrade.Upgrades.Fw54002 where
import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource qualified as R
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.Upgrade.Types

import Marvel.Effect.Types
fw54002 :: UpgradeCard Fw54002
fw54002 = upgrade Fw54002 Cards.fw54002

newtype Fw54002 = Fw54002 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw54002 where
  getAbilities a@(Fw54002 attrs) =
    [(ability a 1 Resource (OwnsThis <> PayingFor (CardWithType EventType <> CardWithTrait T.Attack)) ExhaustCost (runAbility a 1)) {abilityChoices = [runAbility a 1, Pay $ ResourcePayment R.Wild]}]

instance RunMessage Fw54002 where
  runMessage msg a@(Fw54002 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      active <- getActiveCost
      for_ active $ \acid -> do
        card <- getActiveCostCard acid
        for_ card $ \played -> createEffect Cards.fw54002 attrs (EventEntity $ EventId $ unCardId $ pcCardId played)
      pure a
    _ -> Fw54002 <$> runMessage msg attrs

newtype Fw54002Effect = Fw54002Effect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

fw54002Effect :: CardEffect Fw54002Effect
fw54002Effect = effectWith Fw54002Effect Cards.fw54002 (modifiersL .~ [DamageDealtModifier 1, PaidWithCyberneticArm])

instance RunMessage Fw54002Effect where
  runMessage msg e@(Fw54002Effect attrs) = runQueueT $ case msg of
    EventMessage eid ResolvedEvent | effectMatcher attrs == EventEntity eid -> e <$ disableEffect (effectId attrs)
    _ -> Fw54002Effect <$> runMessage msg attrs
