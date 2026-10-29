module Marvel.Upgrade.Upgrades.EvasiveManeuvering (evasiveManeuvering, EvasiveManeuvering (..)) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost.Types
import Marvel.Count
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hero.Heroes.NebulaHero (techniqueResolved)
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

evasiveManeuvering :: UpgradeCard EvasiveManeuvering
evasiveManeuvering = upgrade EvasiveManeuvering Cards.evasiveManeuvering

newtype EvasiveManeuvering = EvasiveManeuvering (Attrs Upgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade EvasiveManeuvering where
  upgradeSpecialCanResolve (EvasiveManeuvering attrs) = do
    (stunnable, confusable) <- evasiveTargets (upgradeController attrs)
    pure $ not (null stunnable && null confusable)

evasiveTargets :: (HasGame m) => IdentityId -> m ([EnemyId], [EnemyId])
evasiveTargets ident = asYou ident do
  enemies <- selectList AnyEnemy
  eligible <- filterM (fmap not . isStalwartEnemy . enemyRef) enemies
  stunnable <- filterM (\e -> selectNone (EnemyWithId e <> StunnedEnemy)) eligible
  confusable <- filterM (\e -> selectNone (EnemyWithId e <> ConfusedEnemy)) eligible
  pure (stunnable, confusable)

instance HasAbilities EvasiveManeuvering where
  getAbilities _ = []

instance HasModifiersFor EvasiveManeuvering where
  getModifiersFor _ (IdentityRef ident) (EvasiveManeuvering attrs)
    | ident == upgradeController attrs = do
        hero <- selectAny (IdentityWithId ident <> HeroIdentity)
        pure $ if hero then [IgnoresGuard, IgnoresPatrol, IgnoresCrisis] else []
  getModifiersFor _ _ _ = pure []

instance RunMessage EvasiveManeuvering where
  runMessage msg u@(EvasiveManeuvering attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      (canStun, canConfuse) <- evasiveTargets ident
      unless (null canStun && null canConfuse) $ chooseOne ident $
        [labeled "$label.stun" $ chooseOne ident
          [targeting (enemyRef e) do
            stun e attrs
            techniqueResolved ident (upgradeId attrs)
          | e <- canStun]
        | not (null canStun)]
        <> [labeled "$label.confuse" $ chooseOne ident
          [targeting (enemyRef e) do
            confuse e attrs
            techniqueResolved ident (upgradeId attrs)
          | e <- canConfuse]
        | not (null canConfuse)]
      pure u
    _ -> EvasiveManeuvering <$> runMessage msg attrs
   where ident = upgradeController attrs
