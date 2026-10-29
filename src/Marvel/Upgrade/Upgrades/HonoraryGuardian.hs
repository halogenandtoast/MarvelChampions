module Marvel.Upgrade.Upgrades.HonoraryGuardian (honoraryGuardian, HonoraryGuardian (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Id
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

honoraryGuardian :: UpgradeCard HonoraryGuardian
honoraryGuardian = upgrade HonoraryGuardian Cards.honoraryGuardian

newtype HonoraryGuardian = HonoraryGuardian (Attrs Upgrade)
  deriving anyclass (HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade HonoraryGuardian where
  upgradeTargets _ = do
    let named = UpgradeIs Cards.honoraryGuardian
    identities <- filterM (\p -> selectNone (named <> UpgradeOnIdentity (IdentityWithId p))) =<< getPlayers
    allies <- selectList (NotAlly $ AllyWithUpgrade named)
    pure $ Just $ map toTarget identities <> map toTarget allies

instance HasModifiersFor HonoraryGuardian where
  getModifiersFor _ target (HonoraryGuardian attrs) = pure case (target, upgradeAttachedAlly attrs) of
    (AllyRef aid, Just attached) | aid == attached -> modifiers
    (MinionRef mid, _) | upgradeAttachedEnemy attrs == Just (EnemyMinionId mid) -> modifiers
    (IdentityRef ident, Nothing) | ident == upgradeController attrs -> modifiers
    _ -> []
   where modifiers = [HitPointModifier 1, TraitModifier Guardian]

instance RunMessage HonoraryGuardian where
  runMessage msg (HonoraryGuardian attrs) = runQueueT $ HonoraryGuardian <$> runMessage msg attrs
