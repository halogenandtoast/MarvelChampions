module Marvel.Ally.Allies.WarMachineJamesRhodes
  ( warMachineJamesRhodes
  , WarMachineJamesRhodes(..)
  )
where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Attrs
import Marvel.Ally.Cards qualified as Cards
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Hp
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Queue
import Marvel.Query
import Marvel.Question
import Marvel.Source
import Marvel.Stats
import Marvel.Target
import Marvel.Window

warMachineJamesRhodes :: AllyCard WarMachineJamesRhodes
warMachineJamesRhodes = ally
  WarMachineJamesRhodes
  Cards.warMachineJamesRhodes
  (Thw 1, 1)
  (Atk 2, 1)
  (HP 4)

newtype WarMachineJamesRhodes = WarMachineJamesRhodes AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities WarMachineJamesRhodes where
  getAbilities (WarMachineJamesRhodes a) = [ability a 1 Action OwnsThis (DamageThisCost 2 <> ExhaustCost) $ RunAbility (toTarget a) 1]

instance RunMessage WarMachineJamesRhodes where
  runMessage msg a@(WarMachineJamesRhodes attrs) = case msg of
    RanAbility target 1 _ | isTarget attrs target -> do
      enemies <- selectList DamageableEnemy
      msgs <- concatMapM (\e -> choiceMessages (allyController attrs) (DamageEnemy (EnemyTarget e) (toSource attrs) FromAbility 1)) enemies
      a <$ pushAll msgs
    _ -> WarMachineJamesRhodes <$> runMessage msg attrs
