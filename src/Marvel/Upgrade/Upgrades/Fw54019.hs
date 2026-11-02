{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Upgrade.Upgrades.Fw54019 where
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

import Marvel.FalconWinter.Helpers
fw54019 :: UpgradeCard Fw54019
fw54019 = upgrade Fw54019 Cards.fw54019

newtype Fw54019 = Fw54019 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw54019 where
  getAbilities a@(Fw54019 attrs) =
    [ability a 1 HeroAction (OwnsThis <> MinionExists (MinionEngagedWith $ IdentityWithId $ upgradeController attrs)) ExhaustCost (runAbility a 1)]

instance RunMessage Fw54019 where
  runMessage msg a@(Fw54019 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      minions <- selectList $ MinionEngagedWith $ IdentityWithId you
      nextCardReduction you attrs AnyCard (fromIntegral $ length minions)
      pure a
    _ -> Fw54019 <$> runMessage msg attrs
