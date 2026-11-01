module Marvel.Ally.Allies.PowerManVictorAlvarez (
  powerManVictorAlvarez,
  powerManVictorAlvarezEffect,
  PowerManVictorAlvarez (..),
  PowerManVictorAlvarezEffect (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Effect.Types
import Marvel.Matchers
import Marvel.Payment ()

-- | He enters play with 2 chi counters on him.
powerManVictorAlvarez :: AllyCard PowerManVictorAlvarez
powerManVictorAlvarez =
  allyWith PowerManVictorAlvarez Cards.powerManVictorAlvarez (Thw 1, 1) (Atk 0, 2) (HP 3) (countersL .~ 2)

newtype PowerManVictorAlvarez = PowerManVictorAlvarez (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Action: Discard any number of chi counters from Power Man → he gets +2 ATK for each
-- chi counter discarded this way until the end of the phase.
instance HasAbilities PowerManVictorAlvarez where
  getAbilities a@(PowerManVictorAlvarez _) =
    [ ability a 1 Action OwnsThis (CountersFromThisCost 1) $ runAbility a 1
    ]

instance RunMessage PowerManVictorAlvarez where
  runMessage msg a@(PowerManVictorAlvarez attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ payment -> do
      -- one +2 ATK effect per counter discarded to pay
      for_ [1 .. sum [k | CountersPayment k <- payments payment]] \_ ->
        createEffect Cards.powerManVictorAlvarez attrs (AllyEntity $ AllyWithId $ allyId attrs)
      pure a
    _ -> PowerManVictorAlvarez <$> runMessage msg attrs

newtype PowerManVictorAlvarezEffect = PowerManVictorAlvarezEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

powerManVictorAlvarezEffect :: CardEffect PowerManVictorAlvarezEffect
powerManVictorAlvarezEffect =
  effectWith PowerManVictorAlvarezEffect Cards.powerManVictorAlvarez $
    (modifiersL .~ [AttackModifier 2]) . (endsL ?~ DisableAtEndOfPhase)

instance RunMessage PowerManVictorAlvarezEffect where
  runMessage msg (PowerManVictorAlvarezEffect attrs) = PowerManVictorAlvarezEffect <$> runMessage msg attrs
