module Marvel.Upgrade.Upgrades.StunGunSilk where
import Marvel.Prelude
import Marvel.Query
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers ()

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.Upgrade.Types

stunGunSilk :: UpgradeCard StunGunSilk
stunGunSilk = upgradeWith StunGunSilk Cards.stunGunSilk ((usesL .~ 2) . (discardIfNoUsesL .~ True))
newtype StunGunSilk = StunGunSilk (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage StunGunSilk where
  runMessage msg u@(StunGunSilk attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      enemies <- selectList (MinionEnemy <> EnemyCanBeStunned)
      chooseOne you [targeting (enemyRef enemy) (stun enemy attrs) | enemy <- enemies]
      pure u
    RanAbility you (isTarget attrs -> True) 2 _ _ -> do
      enemies <- selectList (VillainEnemy <> EnemyCanBeStunned)
      chooseOne you [targeting (enemyRef enemy) (stun enemy attrs) | enemy <- enemies]
      pure u
    _ -> StunGunSilk <$> runMessage msg attrs

instance HasAbilities StunGunSilk where
  getAbilities u@(StunGunSilk _attrs) = [ability u 1 HeroAction (OwnsThis <> EnemyExists (MinionEnemy <> EnemyCanBeStunned)) (ExhaustCost <> UseCost) (runAbility u 1), ability u 2 HeroAction (OwnsThis <> EnemyExists (VillainEnemy <> EnemyCanBeStunned)) (ExhaustCost <> ExactCountersCost 2) (runAbility u 2)]
