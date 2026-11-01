module Marvel.Minion.Minions.NextEvolutionMutantSlayers where

import Marvel.Minion.Runner
import Marvel.Prelude
import Marvel.Question ()
import Marvel.Card
import Marvel.Entity ()
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Types ()
import Marvel.Ability
import Marvel.Cost
import Marvel.Game.Source
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Name (display)
import Marvel.NextEvolution.Marauders
import Marvel.Window qualified as W
import Marvel.Modifier ()
import Marvel.Ref ()
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted ()

newtype MutantSlayersMinion = MutantSlayersMinion (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40094 :: MinionCard MutantSlayersMinion
ne40094 = minion MutantSlayersMinion Cards.ne40094 (Sch 1) (Atk 1) (HP 5)

ne40095 :: MinionCard MutantSlayersMinion
ne40095 = minion MutantSlayersMinion Cards.ne40095 (Sch 0) (Atk 2) (HP 5)

ne40096 :: MinionCard MutantSlayersMinion
ne40096 = minion MutantSlayersMinion Cards.ne40096 (Sch 1) (Atk 1) (HP 5)

ne40097 :: MinionCard MutantSlayersMinion
ne40097 = minion MutantSlayersMinion Cards.ne40097 (Sch 1) (Atk 1) (HP 4)

ne40098 :: MinionCard MutantSlayersMinion
ne40098 = minion MutantSlayersMinion Cards.ne40098 (Sch 0) (Atk 2) (HP 5)

ne40099 :: MinionCard MutantSlayersMinion
ne40099 = minion MutantSlayersMinion Cards.ne40099 (Sch 1) (Atk 1) (HP 4)

ne40100 :: MinionCard MutantSlayersMinion
ne40100 = minion MutantSlayersMinion Cards.ne40100 (Sch 1) (Atk 0) (HP 4)

instance HasAbilities MutantSlayersMinion where
  getAbilities (MutantSlayersMinion a) =
    [windowAbility a 1 (W.EnemyAttacksPlayerOrAlly W.When (EnemyWithId $ toEnemyId a) AnyIdentity)
      ForcedInterrupt NoCost (runAbility a 1)]

instance RunMessage MutantSlayersMinion where
  runMessage msg e@(MutantSlayersMinion a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      let enemy = toEnemyId a
      you <- getBoostedPlayer enemy
      marauderAttackChoice enemy (display $ cdName $ getCardDef a) False True you
      pure e
    _ -> MutantSlayersMinion <$> runMessage msg a
