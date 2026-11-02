{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Upgrade.Upgrades.Fw54008 where
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

fw54008 :: UpgradeCard Fw54008
fw54008 = upgrade Fw54008 Cards.fw54008

newtype Fw54008 = Fw54008 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw54008 where
  getAbilities a@(Fw54008 attrs) =
    [limitedWindowAbility a 1 (W.EnemyDefeated W.After AnyEnemy (AttackFromPlayer $ IdentityWithId $ upgradeController attrs)) HeroResponse (OwnsThis) (DiscardCost $ toTarget attrs) (runAbility a 1)]

instance RunMessage Fw54008 where
  runMessage msg a@(Fw54008 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      ready you
      enemies <- selectList AnyEnemy
      chooseOne you [targeting (enemyRef enemy) $ confuse enemy attrs | enemy <- enemies]
      pure a
    _ -> Fw54008 <$> runMessage msg attrs
