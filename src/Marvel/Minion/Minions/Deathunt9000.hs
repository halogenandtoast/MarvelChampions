module Marvel.Minion.Minions.Deathunt9000 (
  deathunt9000,
  Deathunt9000 (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

deathunt9000 :: MinionCard Deathunt9000
deathunt9000 = minion Deathunt9000 Cards.deathunt9000 (Sch 1) (Atk 1) (HP 6)

newtype Deathunt9000 = Deathunt9000 (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Toughness. Villainous. [star] Boost: this enemy is given a tough status card and
-- another boost card.
instance RunMessage Deathunt9000 where
  runMessage msg e@(Deathunt9000 attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      enemyBecomesTough enemy
      dealBoost enemy
      pure e
    _ -> Deathunt9000 <$> runMessage msg attrs
