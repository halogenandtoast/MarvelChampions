module Marvel.Ally.Allies.JackFlagJackHarrison (
  jackFlagJackHarrison,
  JackFlagJackHarrison (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers
import Marvel.Modifier ()
import Marvel.Ref ()
import Marvel.Window qualified as W

jackFlagJackHarrison :: AllyCard JackFlagJackHarrison
jackFlagJackHarrison = ally JackFlagJackHarrison Cards.jackFlagJackHarrison (Thw 2, 1) (Atk 1, 1) (HP 3)

newtype JackFlagJackHarrison = JackFlagJackHarrison (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Response: after Jack Flag thwarts, place 1 ammo counter on him. Hero Action: exhaust
-- him and remove 1 ammo counter from him → deal 2 damage to an enemy.
instance HasAbilities JackFlagJackHarrison where
  getAbilities (JackFlagJackHarrison a) =
    [ limitedWindowAbility a 1 (W.AllyThwarted W.After (AllyWithId $ allyId a) AnyScheme) Response OwnsThis NoCost $
        runAbility a 1
    , ability a 2 HeroAction (OwnsThis <> EnemyExists DamageableEnemy) (ExhaustCost <> UseCost) $
        runAbility a 2
    ]

instance RunMessage JackFlagJackHarrison where
  runMessage msg j@(JackFlagJackHarrison attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget j -> True) 1 _ _ -> do
      placeCounters attrs 1
      pure j
    RanAbility ident (isTarget j -> True) 2 _ _ -> do
      chooseDamage ident attrs (toDamage 2 FromAbility) DamageableEnemy
      pure j
    _ -> JackFlagJackHarrison <$> runMessage msg attrs
