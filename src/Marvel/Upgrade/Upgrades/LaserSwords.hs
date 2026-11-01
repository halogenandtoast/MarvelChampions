module Marvel.Upgrade.Upgrades.LaserSwords where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

laserSwords :: UpgradeCard LaserSwords
laserSwords = upgrade LaserSwords Cards.laserSwords

newtype LaserSwords = LaserSwords (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor LaserSwords where
  getModifiersFor _ (UpgradeRef uid) (LaserSwords attrs) = pure [RestrictedSlots 2 | uid == upgradeId attrs]
  getModifiersFor _ (IdentityRef you) (LaserSwords attrs) | you == upgradeController attrs = do
    amount <- getEncounterIconCount
    pure [AttackModifier $ min 4 amount]
  getModifiersFor _ _ _ = pure []

instance RunMessage LaserSwords where
  runMessage msg (LaserSwords attrs) = runQueueT $ case msg of
    _ -> LaserSwords <$> runMessage msg attrs
