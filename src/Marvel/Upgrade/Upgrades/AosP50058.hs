{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Upgrade.Upgrades.AosP50058 where

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

aosP50058 :: UpgradeCard AosP50058
aosP50058 = upgrade AosP50058 Cards.aosP50058

newtype AosP50058 = AosP50058 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50058 where
  getAbilities (AosP50058 attrs) =
    [limitedWindowAbility attrs 1 (W.PlayerCardDiscardedFromPlayBy (IdentityWithId $ upgradeController attrs) (CardWithTrait T.Preparation)) Response OwnsThis (DiscardCost $ toRef attrs) (runAbility attrs 1)]

instance RunMessage AosP50058 where
  runMessage msg card@(AosP50058 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      for_ [played | W.PlayerCardDiscardedFromPlay _ played <- windows] $ \played -> do
        discarded <- selectList (InDiscardOf (IdentityWithId you) $ BasicCardMatches $ CardIs $ getCardDef played)
        when (played `elem` discarded) $ addToHand you played
      pure card
    _ -> AosP50058 <$> runMessage msg attrs
