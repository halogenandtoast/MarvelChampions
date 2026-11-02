{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Upgrade.Upgrades.Fw54010 where
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

fw54010 :: UpgradeCard Fw54010
fw54010 = upgrade Fw54010 Cards.fw54010

newtype Fw54010 = Fw54010 (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw54010 where
  getAbilities a@(Fw54010 attrs) =
    [limitedWindowAbility a 1 (W.EnemyDefeated W.After AnyEnemy (AttackFromPlayer $ IdentityWithId $ upgradeController attrs)) HeroResponse (OwnsThis) (ExhaustCost) (runAbility a 1)]

instance HasModifiersFor Fw54010 where
  getModifiersFor _ (IdentityRef you) (Fw54010 attrs)
    | you == upgradeController attrs = pure [TraitModifier T.Spy]
  getModifiersFor _ _ _ = pure []

instance RunMessage Fw54010 where
  runMessage msg a@(Fw54010 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      drawCards you 1
      pure a
    _ -> Fw54010 <$> runMessage msg attrs
