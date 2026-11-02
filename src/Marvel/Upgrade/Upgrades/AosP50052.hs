{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Upgrade.Upgrades.AosP50052 where

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

aosP50052 :: UpgradeCard AosP50052
aosP50052 = upgrade AosP50052 Cards.aosP50052

newtype AosP50052 = AosP50052 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50052 where
  getAbilities (AosP50052 attrs) =
    [limitedWindowAbility attrs 1 (W.MinionEntersPlay W.After AnyMinion) HeroResponse OwnsThis (DiscardCost $ toRef attrs) (runAbility attrs 1)]

instance RunMessage AosP50052 where
  runMessage msg card@(AosP50052 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      for_ [mid | W.MinionEnteredPlay mid <- windows] $ \mid -> confuseThenDealDamage mid attrs (toDamage 2 $ FromPlayerAttack you)
      pure card
    _ -> AosP50052 <$> runMessage msg attrs
