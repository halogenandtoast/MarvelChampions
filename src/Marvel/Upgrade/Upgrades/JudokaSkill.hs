module Marvel.Upgrade.Upgrades.JudokaSkill where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Effect.Types
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

judokaSkill :: UpgradeCard JudokaSkill
judokaSkill = upgradeWith JudokaSkill Cards.judokaSkill ((usesL .~ 3) . (discardIfNoUsesL .~ True))

newtype JudokaSkill = JudokaSkill (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities JudokaSkill where
  getAbilities a@(JudokaSkill attrs) =
    [limitedWindowAbility a 1 (W.HeroDefended W.When (IdentityWithId $ upgradeController attrs) AnyEnemy)
      Interrupt OwnsThis UseCost (runAbility a 1)]

instance RunMessage JudokaSkill where
  runMessage msg a@(JudokaSkill attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [enemy | W.HeroDefends _ enemy <- windows] $ \enemy ->
        createEffect Cards.judokaSkill attrs (EnemyEntity $ EnemyWithId enemy)
      pure a
    _ -> JudokaSkill <$> runMessage msg attrs

newtype JudokaSkillEffect = JudokaSkillEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

judokaSkillEffect :: CardEffect JudokaSkillEffect
judokaSkillEffect = effectWith JudokaSkillEffect Cards.judokaSkill (modifiersL .~ [AttackPenalty 2])

instance RunMessage JudokaSkillEffect where
  runMessage msg a@(JudokaSkillEffect attrs) = runQueueT $ case msg of
    VillainMessage vid VillainEndAttack | effectMatcher attrs == EnemyEntity (EnemyWithId $ EnemyVillainId vid) -> a <$ disableEffect (effectId attrs)
    MinionMessage mid MinionEndAttack | effectMatcher attrs == EnemyEntity (EnemyWithId $ EnemyMinionId mid) -> a <$ disableEffect (effectId attrs)
    _ -> JudokaSkillEffect <$> runMessage msg attrs
