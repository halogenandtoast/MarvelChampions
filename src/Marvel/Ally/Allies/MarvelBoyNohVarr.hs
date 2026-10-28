module Marvel.Ally.Allies.MarvelBoyNohVarr (
  marvelBoyNohVarr,
  MarvelBoyNohVarr (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers hiding (ExhaustedAlly)
import Marvel.Resource
import Marvel.Window qualified as W

marvelBoyNohVarr :: AllyCard MarvelBoyNohVarr
marvelBoyNohVarr = ally MarvelBoyNohVarr Cards.marvelBoyNohVarr (Thw 0, 1) (Atk 2, 1) (HP 2)

newtype MarvelBoyNohVarr = MarvelBoyNohVarr (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Interrupt: When Marvel Boy attacks, spend a [physical] resource → this attack gains
-- piercing and ranged (MarvelCDB's "[physics]" read as [physical]).
instance HasAbilities MarvelBoyNohVarr where
  getAbilities a@(MarvelBoyNohVarr attrs) =
    [ limitedWindowAbility
        a
        1
        (W.AllyAttacked W.When (AllyWithId $ allyId attrs) AnyEnemy)
        Interrupt
        OwnsThis
        (ResourceCost (Just Physical))
        (runAbility a 1)
    ]

instance RunMessage MarvelBoyNohVarr where
  runMessage msg a@(MarvelBoyNohVarr attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      -- the attack about to land, as raiseAllyAttack changes it
      replaceMatchingMessage gain \case
        AllyMessage aid (AllyAttackLands _ _) -> aid == allyId attrs
        _ -> False
      pure a
    _ -> MarvelBoyNohVarr <$> runMessage msg attrs
   where
    gain = \case
      AllyMessage aid (AllyAttackLands enemy dmg) -> [AllyMessage aid $ AllyAttackLands enemy (withRanged $ withPiercing dmg)]
      m -> [m]
