{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Upgrade.Upgrades.AosP50042 where

import Marvel.Prelude
import Marvel.Question
import Marvel.Id
import Marvel.Projection
import Marvel.Card
import Marvel.Cost
import Marvel.Keyword
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Payment
import Marvel.Query
import Marvel.Resource.Types qualified as R
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.AgentsOfShield.PlayerHelpers

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

aosP50042 :: UpgradeCard AosP50042
aosP50042 = upgrade AosP50042 Cards.aosP50042

newtype AosP50042 = AosP50042 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50042 where
  getAbilities (AosP50042 attrs) =
    [limitedWindowAbility attrs 1 (W.WouldTakeDamage (IdentityWithId $ upgradeController attrs) FromAttack AnyValue) Interrupt OwnsThis (DiscardCost $ toRef attrs) (runAbility attrs 1)]

instance RunMessage AosP50042 where
  runMessage msg card@(AosP50042 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      n <- pendingAttackDamage you
      preventDamage you n
      pure card
    _ -> AosP50042 <$> runMessage msg attrs
