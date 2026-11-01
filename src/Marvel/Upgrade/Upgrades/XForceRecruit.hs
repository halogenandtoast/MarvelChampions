module Marvel.Upgrade.Upgrades.XForceRecruit where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Entity
import Marvel.Id (EnemyId (EnemyMinionId))
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Trait qualified as Trait

xForceRecruit :: UpgradeCard XForceRecruit
xForceRecruit = upgrade XForceRecruit Cards.xForceRecruit

newtype XForceRecruit = XForceRecruit (Attrs Upgrade)
  deriving anyclass (HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade XForceRecruit where
  upgradeTargets _ = do
    let named = UpgradeIs Cards.xForceRecruit
    identities <- filterM (\p -> selectNone $ named <> UpgradeOnIdentity (IdentityWithId p)) =<< getPlayers
    allies <- selectList $ NotAlly $ AllyWithUpgrade named
    pure $ Just $ map toTarget identities <> map toTarget allies

instance HasModifiersFor XForceRecruit where
  getModifiersFor _ target (XForceRecruit attrs) = pure $ case (target, upgradeAttachedAlly attrs) of
    (AllyRef aid, Just attached) | aid == attached -> bonus
    (IdentityRef ident, Nothing) | ident == upgradeController attrs, isNothing (upgradeAttachedEnemy attrs) -> bonus
    (MinionRef mid, _) | upgradeAttachedEnemy attrs == Just (EnemyMinionId mid) -> bonus
    _ -> []
   where
    bonus = [HitPointModifier 1, TraitModifier Trait.XForce]

instance RunMessage XForceRecruit where
  runMessage msg (XForceRecruit attrs) = runQueueT $ XForceRecruit <$> runMessage msg attrs
