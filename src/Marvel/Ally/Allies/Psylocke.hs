module Marvel.Ally.Allies.Psylocke where

import Marvel.Prelude
import Marvel.Ability
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
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

psylocke :: AllyCard Psylocke
psylocke = allyWith Psylocke Cards.psylocke (Thw 2, 1) (Atk 1, 1) (HP 3) (countersL .~ 2)

newtype Psylocke = Psylocke (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Psylocke where
  getAbilities (Psylocke a) =
    [limitedWindowAbility a 1 (W.AllyAttacked W.When (AllyWithId $ allyId a) AnyEnemy)
      Interrupt OwnsThis UseCost (runAbility a 1)]

instance RunMessage Psylocke where
  runMessage msg e@(Psylocke attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [enemy | W.AllyAttack _ enemy <- windows] \enemy ->
        dealDamageAnd enemy attrs (toDamage 1 FromAbility) (confuse enemy attrs)
      pure e
    _ -> Psylocke <$> runMessage msg attrs
