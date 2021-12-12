module Marvel.Ally.Allies.WarMachineJamesRhodes (
  warMachineJamesRhodes,
  WarMachineJamesRhodes (..),
) where

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
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Stats
import Marvel.Target
import Marvel.Window

warMachineJamesRhodes :: AllyCard WarMachineJamesRhodes
warMachineJamesRhodes =
  ally
    WarMachineJamesRhodes
    Cards.warMachineJamesRhodes
    (Thw 1, 1)
    (Atk 2, 1)
    (HP 4)

newtype WarMachineJamesRhodes = WarMachineJamesRhodes AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities WarMachineJamesRhodes where
  getAbilities (WarMachineJamesRhodes a) =
    [ ability a 1 Action OwnsThis (DamageThisCost 2 <> ExhaustCost) $
        DamageAllEnemies AnyEnemy (toSource a) FromAbility 1
    ]

instance RunMessage WarMachineJamesRhodes where
  runMessage msg a@(WarMachineJamesRhodes attrs) =
    WarMachineJamesRhodes <$> runMessage msg attrs
