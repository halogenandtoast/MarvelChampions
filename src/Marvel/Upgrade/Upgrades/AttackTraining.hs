module Marvel.Upgrade.Upgrades.AttackTraining where

import Marvel.Prelude
import Marvel.Card.Side ()
import Marvel.Payment ()
import Marvel.Game.Source ()
import Marvel.Hand ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Id ()
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W ()
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

attackTraining :: UpgradeCard AttackTraining
attackTraining = upgrade AttackTraining Cards.attackTraining

newtype AttackTraining = AttackTraining (Attrs Upgrade)
  deriving anyclass (HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor AttackTraining where
  getModifiersFor _ (AllyRef aid) (AttackTraining a) = pure $ if Just aid == upgradeAttachedAlly a then [HitPointModifier 2, AttackModifier 1] else []
  getModifiersFor _ _ _ = pure []

instance RunMessage AttackTraining where
  runMessage msg (AttackTraining attrs) = runQueueT $ case msg of
    _ -> AttackTraining <$> runMessage msg attrs

instance IsUpgrade AttackTraining where
  upgradeTargets _ = Just . map toTarget <$> selectList (AllyWithTrait Trait.XMen <> NotAlly (AllyWithUpgrade $ UpgradeWithTrait Trait.Training))
