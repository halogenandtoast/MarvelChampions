module Marvel.Ally.Allies.GiantMan (
  giantMan,
  GiantMan (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Modifier ()
import Marvel.Ref ()

giantMan :: AllyCard GiantMan
giantMan = ally GiantMan Cards.giantMan (Thw 2, 1) (Atk 2, 1) (HP 4)

newtype GiantMan = GiantMan (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Giant-Man gets +2 ATK while he has 3 or more remaining hit points.
instance HasModifiersFor GiantMan where
  getModifiersFor _ (AllyRef aid) (GiantMan a) | aid == allyId a = do
    hp <- getModifiedHitPoints a
    pure [AttackModifier 2 | subtractNatural (allyDamage a) hp >= 3]
  getModifiersFor _ _ _ = pure []

instance RunMessage GiantMan where
  runMessage msg (GiantMan attrs) = GiantMan <$> runMessage msg attrs
