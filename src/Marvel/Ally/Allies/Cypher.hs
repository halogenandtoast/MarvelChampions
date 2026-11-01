module Marvel.Ally.Allies.Cypher where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability.Types qualified as Ability ()
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

cypher :: AllyCard Cypher
cypher = ally Cypher Cards.cypher (Thw 1, 1) (Atk 1, 1) (HP 2)

newtype Cypher = Cypher (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Cypher where
  getAbilities (Cypher a) =
    [limitedWindowAbility a 1 (W.AllyAttackDamagedEnemy (AllyWithId $ allyId a) ConfusedEnemy) Response OwnsThis NoCost (runAbility a 1)]

instance RunMessage Cypher where
  runMessage msg e@(Cypher a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> e <$ drawCards you 1
    _ -> Cypher <$> runMessage msg a
