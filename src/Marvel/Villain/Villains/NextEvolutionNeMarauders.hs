module Marvel.Villain.Villains.NextEvolutionNeMarauders where

import Marvel.Prelude
import Marvel.Question (runAbility)
import Marvel.Card
import Marvel.Entity
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Data.Text qualified as T
import Marvel.Ability
import Marvel.Cost
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Name (display)
import Marvel.NextEvolution.Marauders
import Marvel.Window qualified as W
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype NeMaraudersVillain = NeMaraudersVillain (Attrs Villain)
  deriving anyclass (IsVillain, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

ne40070a :: VillainCard NeMaraudersVillain
ne40070a = villainWith NeMaraudersVillain Cards.ne40070a (Sch 1) (Atk 1) (HP $ PerPlayer 10) (stageL .~ 1)

ne40070b :: VillainCard NeMaraudersVillain
ne40070b = villainWith NeMaraudersVillain Cards.ne40070b (Sch 2) (Atk 2) (HP $ PerPlayer 13) (stageL .~ 1)

ne40071a :: VillainCard NeMaraudersVillain
ne40071a = villainWith NeMaraudersVillain Cards.ne40071a (Sch 0) (Atk 2) (HP $ PerPlayer 11) (stageL .~ 1)

ne40071b :: VillainCard NeMaraudersVillain
ne40071b = villainWith NeMaraudersVillain Cards.ne40071b (Sch 1) (Atk 3) (HP $ PerPlayer 14) (stageL .~ 1)

ne40072a :: VillainCard NeMaraudersVillain
ne40072a = villainWith NeMaraudersVillain Cards.ne40072a (Sch 1) (Atk 1) (HP $ PerPlayer 11) (stageL .~ 1)

ne40072b :: VillainCard NeMaraudersVillain
ne40072b = villainWith NeMaraudersVillain Cards.ne40072b (Sch 2) (Atk 2) (HP $ PerPlayer 14) (stageL .~ 1)

ne40073a :: VillainCard NeMaraudersVillain
ne40073a = villainWith NeMaraudersVillain Cards.ne40073a (Sch 1) (Atk 1) (HP $ PerPlayer 9) (stageL .~ 1)

ne40073b :: VillainCard NeMaraudersVillain
ne40073b = villainWith NeMaraudersVillain Cards.ne40073b (Sch 2) (Atk 2) (HP $ PerPlayer 12) (stageL .~ 1)

ne40074a :: VillainCard NeMaraudersVillain
ne40074a = villainWith NeMaraudersVillain Cards.ne40074a (Sch 0) (Atk 2) (HP $ PerPlayer 10) (stageL .~ 1)

ne40074b :: VillainCard NeMaraudersVillain
ne40074b = villainWith NeMaraudersVillain Cards.ne40074b (Sch 1) (Atk 3) (HP $ PerPlayer 13) (stageL .~ 1)

ne40075a :: VillainCard NeMaraudersVillain
ne40075a = villainWith NeMaraudersVillain Cards.ne40075a (Sch 1) (Atk 1) (HP $ PerPlayer 9) (stageL .~ 1)

ne40075b :: VillainCard NeMaraudersVillain
ne40075b = villainWith NeMaraudersVillain Cards.ne40075b (Sch 2) (Atk 2) (HP $ PerPlayer 12) (stageL .~ 1)

ne40076a :: VillainCard NeMaraudersVillain
ne40076a = villainWith NeMaraudersVillain Cards.ne40076a (Sch 2) (Atk 0) (HP $ PerPlayer 9) (stageL .~ 1)

ne40076b :: VillainCard NeMaraudersVillain
ne40076b = villainWith NeMaraudersVillain Cards.ne40076b (Sch 3) (Atk 1) (HP $ PerPlayer 12) (stageL .~ 1)

instance HasAbilities NeMaraudersVillain where
  getAbilities (NeMaraudersVillain a) =
    [windowAbility a 1 (W.EnemyAttacksPlayerOrAlly W.When (EnemyWithId $ toEnemyId a) AnyIdentity)
      ForcedInterrupt NoCost (runAbility a 1)]

instance RunMessage NeMaraudersVillain where
  runMessage msg e@(NeMaraudersVillain a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      let enemy = toEnemyId a
      you <- getBoostedPlayer enemy
      marauderAttackChoice enemy (display $ cdName $ getCardDef a) ("b" `T.isSuffixOf` unCardCode (toCardCode (getCardDef a))) False you
      pure e
    _ -> NeMaraudersVillain <$> runMessage msg a
