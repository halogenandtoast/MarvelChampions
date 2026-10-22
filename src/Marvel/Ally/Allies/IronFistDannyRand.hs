module Marvel.Ally.Allies.IronFistDannyRand (
  ironFistDannyRand,
  IronFistDannyRand (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers
import Marvel.Window qualified as W

-- | He enters play with 2 mystic counters on him.
ironFistDannyRand :: AllyCard IronFistDannyRand
ironFistDannyRand =
  allyWith IronFistDannyRand Cards.ironFistDannyRand (Thw 1, 1) (Atk 2, 1) (HP 3) (countersL .~ 2)

newtype IronFistDannyRand = IronFistDannyRand (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | As errata'd, an interrupt: when he attacks an enemy, remove a mystic counter from
-- him → stun that enemy and deal 1 damage to it.
instance HasAbilities IronFistDannyRand where
  getAbilities (IronFistDannyRand a) =
    [ limitedWindowAbility a 1 (W.AllyAttacked W.When (AllyWithId $ allyId a) AnyEnemy) Interrupt OwnsThis UseCost $
        runAbility a 1
    ]

instance RunMessage IronFistDannyRand where
  runMessage msg a@(IronFistDannyRand attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      for_ [enemy | W.AllyAttack _ enemy <- windows] \enemy -> do
        stun enemy attrs
        dealDamage enemy attrs (toDamage 1 FromAbility)
      pure a
    _ -> IronFistDannyRand <$> runMessage msg attrs
