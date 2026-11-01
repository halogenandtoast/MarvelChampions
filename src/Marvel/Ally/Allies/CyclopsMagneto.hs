module Marvel.Ally.Allies.CyclopsMagneto where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource.Types ()
import Marvel.Stats
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window qualified as W
import Marvel.Identity.Types ()

import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

cyclopsMagneto :: AllyCard CyclopsMagneto
cyclopsMagneto = ally CyclopsMagneto Cards.cyclopsMagneto (Thw 2, 1) (Atk 2, 1) (HP 2)

newtype CyclopsMagneto = CyclopsMagneto (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities CyclopsMagneto where
  getAbilities a = [limitedWindowAbility a 1 W.EntersPlayThis Response OwnsThis NoCost (runAbility a 1)]

instance RunMessage CyclopsMagneto where
  runMessage msg e@(CyclopsMagneto attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      enemies <- selectList AnyEnemy
      chooseOne you [targeting (enemyRef enemy) $ lastingModifiers attrs (EnemyEntity $ EnemyWithId enemy) [AttackDamageTakenModifier 1] False | enemy <- enemies]
      pure e
    _ -> CyclopsMagneto <$> runMessage msg attrs
