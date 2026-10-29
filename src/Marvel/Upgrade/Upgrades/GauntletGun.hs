module Marvel.Upgrade.Upgrades.GauntletGun (gauntletGun, GauntletGun (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Event.Cards qualified as Events
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

gauntletGun :: UpgradeCard GauntletGun
gauntletGun = upgrade GauntletGun Cards.gauntletGun

newtype GauntletGun = GauntletGun (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

warMachineEvents :: CardMatcher
warMachineEvents = CardMatchesAny $ map CardIs [Events.repulsorBeam, Events.targetedStrike, Events.scorchedEarth, Events.fullAuto]

instance HasModifiersFor GauntletGun where
  getModifiersFor _ target (GauntletGun attrs)
    | target == toRef attrs, not (upgradeExhausted attrs) =
        pure [MayGenerateIdentityCountersFor warMachineEvents 1]
  getModifiersFor _ _ _ = pure []

instance HasAbilities GauntletGun where
  getAbilities a =
    [ (ability a 1 Resource (OwnsThis <> PayingFor warMachineEvents) ExhaustCost $ Pay $ ResourcePayment Wild)
        { abilityChoices = [Pay (ResourcePayment Wild), runAbility a 1] } ]

instance RunMessage GauntletGun where
  runMessage msg u@(GauntletGun attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      placeCounters ident 1
      pure u
    _ -> GauntletGun <$> runMessage msg attrs
