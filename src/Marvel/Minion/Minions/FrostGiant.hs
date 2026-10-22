module Marvel.Minion.Minions.FrostGiant (
  frostGiant,
  FrostGiant (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

frostGiant :: MinionCard FrostGiant
frostGiant = minion FrostGiant Cards.frostGiant (Sch 1) (Atk 3) (HP 4)

newtype FrostGiant = FrostGiant (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | [star] Boost: if the villain is attacking and this attack deals damage to a
-- character, stun that character.
instance RunMessage FrostGiant where
  runMessage msg g@(FrostGiant attrs) = case msg of
    Boost (attackDamages -> Just (EnemyVillainId _, character)) -> do
      push $ GiveStatus (characterRef character) StunnedStatus
      pure g
    _ -> FrostGiant <$> runMessage msg attrs
