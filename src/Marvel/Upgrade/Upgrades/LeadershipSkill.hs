module Marvel.Upgrade.Upgrades.LeadershipSkill where

import Marvel.Prelude
import Data.List (nub)
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Effect.Types
import Marvel.Entity
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Question
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

leadershipSkill :: UpgradeCard LeadershipSkill
leadershipSkill = upgradeWith LeadershipSkill Cards.leadershipSkill ((usesL .~ 3) . (discardIfNoUsesL .~ True))

newtype LeadershipSkill = LeadershipSkill (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities LeadershipSkill where
  getAbilities a = [limitedWindowAbility a 1
    (W.WindowOneOf [W.AllyAttacked W.When AnyAlly AnyEnemy, W.AllyThwarted W.When AnyAlly AnyScheme])
    Interrupt OwnsThis UseCost (runAbility a 1)]

instance RunMessage LeadershipSkill where
  runMessage msg a@(LeadershipSkill attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      let allies = nub $ concatMap (\case W.AllyAttack aid _ -> [aid]; W.AllyThwart aid _ -> [aid]; _ -> []) windows
      for_ allies \aid -> do
        addToAllyBasicPower aid 1
        createEffect Cards.leadershipSkill aid (AllyEntity $ AllyWithId aid)
      pure a
    _ -> LeadershipSkill <$> runMessage msg attrs

-- Survives spending the final counter and discarding the upgrade.
newtype LeadershipSkillEffect = LeadershipSkillEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

leadershipSkillEffect :: CardEffect LeadershipSkillEffect
leadershipSkillEffect = effectWith LeadershipSkillEffect Cards.leadershipSkill
  ((modifiersL .~ [ThwartModifier 1, AttackModifier 1]) . (endsL ?~ DisableAtEndOfPhase))

instance RunMessage LeadershipSkillEffect where
  runMessage msg e@(LeadershipSkillEffect attrs) = runQueueT $ case msg of
    CheckWindows ws | any done ws -> e <$ disableEffect (effectId attrs)
    AllyMessage aid action | effectMatcher attrs == AllyEntity (AllyWithId aid)
      , (case action of AllyAttackConsequential -> True; AllyAttackConsequentialAgainst _ -> True; AllyThwartConsequential -> True; AllyThwartConsequentialOn _ -> True; _ -> False) -> e <$ disableEffect (effectId attrs)
    _ -> LeadershipSkillEffect <$> runMessage msg attrs
   where
    done (W.Window W.After (W.AllyAttack aid _)) = effectMatcher attrs == AllyEntity (AllyWithId aid)
    done (W.Window W.After (W.AllyThwart aid _)) = effectMatcher attrs == AllyEntity (AllyWithId aid)
    done (W.Window W.After (W.AllyAttackCompletedAgainst aid _)) = effectMatcher attrs == AllyEntity (AllyWithId aid)
    done _ = False
