module Marvel.Minion.Minions.Macrobots (
  macrobots,
  Macrobots (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Matchers (VillainMatcher (ActiveVillain))
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

macrobots :: MinionCard Macrobots
macrobots = minion Macrobots Cards.macrobots (Sch 1) (Atk 2) (HP 4)

newtype Macrobots = Macrobots (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Guard. Retaliate 1. [star] Boost: give Kang a tough status card.
instance RunMessage Macrobots where
  runMessage msg e@(Macrobots attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      traverse_ enemyBecomesTough =<< selectOne ActiveVillain
      pure e
    _ -> Macrobots <$> runMessage msg attrs
