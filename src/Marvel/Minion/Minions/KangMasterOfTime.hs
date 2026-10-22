module Marvel.Minion.Minions.KangMasterOfTime (
  kangMasterOfTime,
  KangMasterOfTime (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Game.Source
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

kangMasterOfTime :: MinionCard KangMasterOfTime
kangMasterOfTime = minion KangMasterOfTime Cards.kangMasterOfTime (Sch 1) (Atk 1) (HP 6)

newtype KangMasterOfTime = KangMasterOfTime (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Toughness. Villainous. +1 SCH and +1 ATK for each obligation in the play area of the
-- player he is engaged with.
instance HasModifiersFor KangMasterOfTime where
  getModifiersFor _ (MinionRef mid) (KangMasterOfTime a) | mid == minionId a = do
    n <- countObligationsOf (minionEngagedIdentity a)
    pure [m n | n > 0, m <- [SchemeModifier, AttackModifier]]
  getModifiersFor _ _ _ = pure []

instance RunMessage KangMasterOfTime where
  runMessage msg (KangMasterOfTime attrs) = KangMasterOfTime <$> runMessage msg attrs
