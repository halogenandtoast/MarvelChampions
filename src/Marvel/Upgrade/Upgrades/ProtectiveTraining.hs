module Marvel.Upgrade.Upgrades.ProtectiveTraining where

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

protectiveTraining :: UpgradeCard ProtectiveTraining
protectiveTraining = upgrade ProtectiveTraining Cards.protectiveTraining

newtype ProtectiveTraining = ProtectiveTraining (Attrs Upgrade)
  deriving anyclass (HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor ProtectiveTraining where
  getModifiersFor _ (AllyRef aid) (ProtectiveTraining a) = pure $ if Just aid == upgradeAttachedAlly a then [HitPointModifier 3] else []
  getModifiersFor _ _ _ = pure []

instance RunMessage ProtectiveTraining where
  runMessage msg (ProtectiveTraining attrs) = runQueueT $ case msg of
    _ -> ProtectiveTraining <$> runMessage msg attrs

instance IsUpgrade ProtectiveTraining where
  upgradeTargets _ = Just . map toTarget <$> selectList (AllyWithTrait Trait.XMen <> NotAlly (AllyWithUpgrade $ UpgradeWithTrait Trait.Training))
