module Marvel.Upgrade.Upgrades.PhoenixForce where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

phoenixForceRestrained :: UpgradeCard PhoenixForce
phoenixForceRestrained = upgrade PhoenixForce Cards.phoenixForceRestrained

phoenixForceUnleashed :: UpgradeCard PhoenixForce
phoenixForceUnleashed = upgrade PhoenixForce Cards.phoenixForceUnleashed

newtype PhoenixForce = PhoenixForce (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor PhoenixForce where
  getModifiersFor _ (IdentityRef you) (PhoenixForce a) | you == upgradeController a = do
    hero <- identityMatches HeroIdentity you
    let unleashed = toCardCode a == "34002b"
    pure $ [TraitModifier $ if unleashed then Trait.Unleashed else Trait.Restrained]
      <> [ThwartPenalty 2 | unleashed && hero] <> [AttackModifier 2 | unleashed && hero]
  getModifiersFor _ _ _ = pure []

instance HasAbilities PhoenixForce where
  getAbilities (PhoenixForce a) =
    [ limitedWindowAbility a 1 (W.UpgradeLosesLastCounter W.After $ UpgradeWithId $ upgradeId a)
        ForcedResponse OwnsThis NoCost (runAbility a 1)
    | toCardCode a == "34002a"
    ] <> [ limitedWindowAbility a 2 (W.UpgradeCountersPlaced W.After $ UpgradeWithId $ upgradeId a)
        ForcedResponse OwnsThis NoCost (runAbility a 2)
    | toCardCode a == "34002b", upgradeUses a >= 4
    ]

instance RunMessage PhoenixForce where
  runMessage msg e@(PhoenixForce a) = runQueueT $ case msg of
    RemoveCounters target n | target == toRef a -> do
      let remaining = subtractNatural n (upgradeUses a)
      when (remaining /= upgradeUses a) $
        checkWindows [W.Window W.After $ W.UpgradeCounterAmountChanged (upgradeId a) (upgradeUses a) remaining]
      pure $ PhoenixForce a {upgradeUses = remaining}
    UpgradeMessage uid SpendUpgradeUse | uid == upgradeId a -> runMessage (RemoveCounters (toRef a) 1) e
    PlaceCounters target n | target == toRef a -> runMessage (UpgradeMessage (upgradeId a) $ AddUpgradeUses n) e
    UpgradeMessage uid (AddUpgradeUses n) | uid == upgradeId a -> do
      let total = upgradeUses a + n
      when (n > 0) $
        checkWindows [W.Window W.After $ W.UpgradeCounterAmountChanged (upgradeId a) (upgradeUses a) total]
      pure $ PhoenixForce a {upgradeUses = total}
    RanAbility _ (isTarget a -> True) 1 _ _ -> pure $ PhoenixForce a {upgradeCardDef = Cards.phoenixForceUnleashed}
    RanAbility _ (isTarget a -> True) 2 _ _ -> pure $ PhoenixForce a {upgradeCardDef = Cards.phoenixForceRestrained}
    _ -> PhoenixForce <$> runMessage msg a
