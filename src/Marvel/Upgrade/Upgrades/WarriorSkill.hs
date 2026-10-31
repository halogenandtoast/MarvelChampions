module Marvel.Upgrade.Upgrades.WarriorSkill where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability.Types qualified as Ability
import Marvel.Effect.Types
import Marvel.Event.Types qualified as Event
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Keyword
import Marvel.Window qualified as W
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

warriorSkill :: UpgradeCard WarriorSkill
warriorSkill = upgradeWith WarriorSkill Cards.warriorSkill ((usesL .~ 3) . (discardIfNoUsesL .~ True))

newtype WarriorSkill = WarriorSkill (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities WarriorSkill where
  getAbilities (WarriorSkill a) =
    [limitedWindowAbility a 1 (W.IdentityAttacked W.When (IdentityWithId $ upgradeController a) AnyEnemy)
      Interrupt OwnsThis UseCost (runAbility a 1)]

instance RunMessage WarriorSkill where
  runMessage msg e@(WarriorSkill attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      source <- currentPlayerAttackSource you
      for_ source \case
        EventRef eid -> createEffect Cards.warriorSkill attrs (EventEntity eid)
        _ -> pure ()
      -- The first damage was calculated before the interrupt opened. Later
      -- instances of the same attack read the lasting effect on their event.
      modifyCurrentPlayerAttack you 1 []
      pure e
    _ -> WarriorSkill <$> runMessage msg attrs

newtype WarriorSkillEffect = WarriorSkillEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

warriorSkillEffect :: CardEffect WarriorSkillEffect
warriorSkillEffect = effectWith WarriorSkillEffect Cards.warriorSkill (modifiersL .~ [DamageDealtModifier 1])

instance RunMessage WarriorSkillEffect where
  runMessage msg e@(WarriorSkillEffect attrs) = runQueueT $ case msg of
    EventMessage eid ResolvedEvent | effectMatcher attrs == EventEntity eid -> e <$ disableEffect (effectId attrs)
    EventMessage eid (EventAttacked _ _ _) | effectMatcher attrs == EventEntity eid -> do
      labels <- sourceAbilitySubTypes (EventRef eid)
      unless (Ability.Attack `elem` labels) $ disableEffect (effectId attrs)
      pure e
    CheckWindows windows | any finished windows -> e <$ disableEffect (effectId attrs)
    _ -> WarriorSkillEffect <$> runMessage msg attrs
   where
    finished (W.Window W.After (W.PlayerAttackResolved (EventRef eid) _ _)) = effectMatcher attrs == EventEntity eid
    finished _ = False
