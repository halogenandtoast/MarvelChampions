module Marvel.Minion.Minions.SirRaston (
  sirRaston,
  SirRaston (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

sirRaston :: MinionCard SirRaston
sirRaston = minion SirRaston Cards.sirRaston (Sch 1) (Atk 2) (HP 6)

newtype SirRaston = SirRaston (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Guard. Retaliate 1. [star] Boost: you take 1 damage, and this enemy is given another
-- boost card.
instance RunMessage SirRaston where
  runMessage msg e@(SirRaston attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      damageIdentity you attrs (toDamage 1 FromAbility)
      dealBoost enemy
      pure e
    _ -> SirRaston <$> runMessage msg attrs
