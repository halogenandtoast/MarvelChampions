{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Upgrade.Upgrades.AosP50050 where

import Marvel.Prelude
import Marvel.Question
import Marvel.Id
import Marvel.Projection
import Marvel.Card
import Marvel.Cost
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

aosP50050 :: UpgradeCard AosP50050
aosP50050 = upgrade AosP50050 Cards.aosP50050

newtype AosP50050 = AosP50050 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50050 where
  getAbilities (AosP50050 attrs) =
    [limitedWindowAbility attrs 1 (W.EnemyWouldScheme MinionEnemy AnyIdentity) Interrupt OwnsThis (DiscardCost $ toRef attrs) (runAbility attrs 1)]

instance RunMessage AosP50050 where
  runMessage msg card@(AosP50050 attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [(mid,you) | W.EnemySchemeBegins (EnemyMinionId mid) you <- windows] $ \(mid,you) -> cardSpecific mid "informantScheme" you
      pure card
    _ -> AosP50050 <$> runMessage msg attrs
