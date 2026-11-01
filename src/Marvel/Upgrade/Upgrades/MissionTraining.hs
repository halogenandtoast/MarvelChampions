module Marvel.Upgrade.Upgrades.MissionTraining where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W ()
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

missionTraining :: UpgradeCard MissionTraining
missionTraining = upgrade MissionTraining Cards.missionTraining

newtype MissionTraining = MissionTraining (Attrs Upgrade)
  deriving anyclass (HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade MissionTraining where
  upgradeTargets _ = Just . map toTarget <$> selectList
    (AllyWithTrait Trait.XMen <> NotAlly (AllyWithUpgrade $ UpgradeWithTrait Trait.Training))

instance HasModifiersFor MissionTraining where
  getModifiersFor _ (AllyRef aid) (MissionTraining attrs)
    | Just aid == upgradeAttachedAlly attrs = pure [ThwartModifier 1, HitPointModifier 2]
  getModifiersFor _ _ _ = pure []

instance RunMessage MissionTraining where
  runMessage msg (MissionTraining attrs) = runQueueT $ MissionTraining <$> runMessage msg attrs
