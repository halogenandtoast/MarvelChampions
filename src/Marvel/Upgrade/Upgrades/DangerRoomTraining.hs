module Marvel.Upgrade.Upgrades.DangerRoomTraining where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Trait qualified as Trait

dangerRoomTraining :: UpgradeCard DangerRoomTraining
dangerRoomTraining = upgrade DangerRoomTraining Cards.dangerRoomTraining

newtype DangerRoomTraining = DangerRoomTraining (Attrs Upgrade)
  deriving anyclass (HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade DangerRoomTraining where
  upgradeTargets _ = Just . map toTarget <$> selectList
    (AllyWithTrait Trait.XMen <> NotAlly (AllyWithUpgrade $ UpgradeWithTrait Trait.Training))

instance HasModifiersFor DangerRoomTraining where
  getModifiersFor _ (AllyRef aid) (DangerRoomTraining attrs)
    | Just aid == upgradeAttachedAlly attrs = pure [ThwartModifier 1, AttackModifier 1, HitPointModifier 1]
  getModifiersFor _ _ _ = pure []

instance RunMessage DangerRoomTraining where
  runMessage msg (DangerRoomTraining attrs) = runQueueT $ DangerRoomTraining <$> runMessage msg attrs
