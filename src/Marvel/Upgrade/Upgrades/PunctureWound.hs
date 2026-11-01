module Marvel.Upgrade.Upgrades.PunctureWound where

import Marvel.Prelude
import Data.List (nub)
import Marvel.Cost
import Marvel.Criteria
import Marvel.Card.Def
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Phase
import Marvel.Window qualified as W
import Marvel.Hero.Cards qualified as Heroes
import Marvel.Ally.Cards qualified as Allies
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

punctureWound :: UpgradeCard PunctureWound
punctureWound = upgrade PunctureWound Cards.punctureWound

newtype PunctureWound = PunctureWound (Attrs Upgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade PunctureWound where
  upgradeTargets _ = do
    attacks <- getAttacksThisTurn
    g <- getGame
    identities <- selectList (IdentityIs Heroes.x23)
    let relevant (IdentityRef you) = you `elem` identities
        relevant source = maybe False (sameCard Allies.honeyBadger) (abilityCardDef g $ refocus source)
        enemies = [enemy | (source, enemy) <- attacks, relevant source]
    valid <- filterM (selectAny . EnemyWithId) enemies
    pure $ Just $ map enemyRef $ nub valid

instance HasModifiersFor PunctureWound where
  getModifiersFor _ target (PunctureWound attrs) = pure [AttackPenalty 1 | Just target == (enemyRef <$> upgradeAttachedEnemy attrs)]

instance HasAbilities PunctureWound where
  getAbilities a@(PunctureWound attrs) =
    [ limitedWindowAbility a 1 (W.PhaseBegins W.After PlayerPhase) ForcedResponse OwnsThis NoCost (runAbility a 1)
    | isJust $ upgradeAttachedEnemy attrs]

instance RunMessage PunctureWound where
  runMessage msg a@(PunctureWound attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      removeFromPlay attrs
      for_ (upgradeAttachedEnemy attrs) $ \enemy -> dealDamage enemy attrs (toDamage 3 FromAbility)
      pure a
    _ -> PunctureWound <$> runMessage msg attrs
