module Marvel.Ally.Allies.WarMachineJamesRhodes (
  warMachineJamesRhodes,
  WarMachineJamesRhodes (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Attrs
import Marvel.Ally.Cards qualified as Cards
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers

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
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, IsSource, IsTarget)

instance HasAbilities WarMachineJamesRhodes where
  getAbilities a =
    [ ability a 1 Action OwnsThis (DamageThisCost 2 <> ExhaustCost) $
        DamageAllEnemies AnyEnemy (toSource a) (toDamage 1 FromAbility)
    ]

instance RunMessage WarMachineJamesRhodes where
  runMessage msg a = WarMachineJamesRhodes <$> runMessage msg (toAttrs a)
