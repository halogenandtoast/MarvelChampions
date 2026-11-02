{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Upgrade.Upgrades.Fw53024 where
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

fw53024 :: UpgradeCard Fw53024
fw53024 = upgrade Fw53024 Cards.fw53024

newtype Fw53024 = Fw53024 (Attrs Upgrade)
  deriving anyclass HasModifiersFor
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade Fw53024 where
  upgradeTargets _ = Just . map toTarget <$> selectList (AllyWithTrait T.Aerial <> NotAlly (AllyWithUpgrade $ UpgradeIs Cards.fw53024))

instance HasAbilities Fw53024 where
  getAbilities a@(Fw53024 attrs) =
    [limitedWindowAbility a 1
      (W.AllyWouldTakeDamageOf W.When (AllyWithTrait T.Aerial <> NotAlly (maybe AnyAlly AllyWithId $ upgradeAttachedAlly attrs)) FromConsequential) Interrupt
      (OwnsThis <> AllyExists (maybe AnyAlly AllyWithId (upgradeAttachedAlly attrs) <> UnexhaustedAlly))
      (ExhaustCharacterCost $ AllyCharacters $ maybe AnyAlly AllyWithId $ upgradeAttachedAlly attrs) (runAbility a 1)]

instance RunMessage Fw53024 where
  runMessage msg a@(Fw53024 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      for_ [aid | W.AllyTakeDamage aid dmg <- windows, damageSource dmg == FromConsequential] $ \aid -> preventAllyDamage aid 1
      pure a
    _ -> Fw53024 <$> runMessage msg attrs
