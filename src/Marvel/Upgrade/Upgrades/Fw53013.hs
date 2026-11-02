{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Upgrade.Upgrades.Fw53013 where
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

fw53013 :: UpgradeCard Fw53013
fw53013 = upgrade Fw53013 Cards.fw53013

newtype Fw53013 = Fw53013 (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw53013 where
  getAbilities a@(Fw53013 attrs) =
    [limitedWindowAbility a 1 (W.WouldTakeDamage (IdentityWithId $ upgradeController attrs) FromAnyDamageSource AnyValue) HeroInterrupt (OwnsThis) (ExhaustCost) (runAbility a 1)]

instance HasModifiersFor Fw53013 where
  getModifiersFor _ (IdentityRef you) (Fw53013 attrs)
    | you == upgradeController attrs = pure [DefenseModifier 1]
  getModifiersFor _ _ _ = pure []

instance RunMessage Fw53013 where
  runMessage msg a@(Fw53013 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      preventDamage you 1
      chooseDamage you attrs (toDamage 1 FromAbility) AnyEnemy
      pure a
    _ -> Fw53013 <$> runMessage msg attrs
