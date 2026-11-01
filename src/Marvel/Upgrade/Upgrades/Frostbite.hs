module Marvel.Upgrade.Upgrades.Frostbite where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Identity.Types ()
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Iceman.Cards qualified as Cards
import Marvel.Iceman.Helpers ()
import Marvel.Upgrade.Types

frostbite :: UpgradeCard Frostbite
frostbite = upgrade Frostbite Cards.frostbite

newtype Frostbite = Frostbite (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Frostbite where
  getModifiersFor _ target (Frostbite attrs) = pure
    [m | Just target == (enemyRef <$> upgradeAttachedEnemy attrs), m <- [AttackPenalty 1, SchemePenalty 1]]
instance HasAbilities Frostbite where
  getAbilities a@(Frostbite attrs) = case upgradeAttachedEnemy attrs of
    Just enemy -> [limitedWindowAbility a 1 (W.EnemyActivates W.After (EnemyWithId enemy) AnyIdentity) ForcedResponse OwnsThis NoCost (runAbility a 1)]
    Nothing -> []
instance RunMessage Frostbite where
  runMessage msg a@(Frostbite attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> a <$ setAside [toCard attrs]
    UpgradeMessage uid (UpgradeAttachedToEnemy enemy) | uid == upgradeId attrs -> do
      attrs' <- runMessage msg attrs
      checkWindows [W.Window W.After $ W.UpgradeAttachedToEnemy (upgradeController attrs) uid Cards.frostbite enemy]
      pure $ Frostbite attrs'
    MinionMessage mid MinionLeavesPlay | upgradeAttachedEnemy attrs == Just (EnemyMinionId mid) -> a <$ setAside [toCard attrs]
    RemoveFromPlay target | Just target == (enemyRef <$> upgradeAttachedEnemy attrs) -> a <$ setAside [toCard attrs]
    RemoveFromGame target | Just target == (enemyRef <$> upgradeAttachedEnemy attrs) -> a <$ setAside [toCard attrs]
    ReturnToHand target | Just target == (enemyRef <$> upgradeAttachedEnemy attrs) -> a <$ setAside [toCard attrs]
    AddToVictoryDisplay target | Just target == (enemyRef <$> upgradeAttachedEnemy attrs) -> a <$ setAside [toCard attrs]
    _ -> Frostbite <$> runMessage msg attrs
