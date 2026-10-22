module Marvel.Minion.Minions.TimeDisplacedSoldier (
  timeDisplacedSoldier,
  TimeDisplacedSoldier (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Game.Source
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

timeDisplacedSoldier :: MinionCard TimeDisplacedSoldier
timeDisplacedSoldier = minion TimeDisplacedSoldier Cards.timeDisplacedSoldier (Sch 1) (Atk 2) (HP 3)

newtype TimeDisplacedSoldier = TimeDisplacedSoldier (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Incite 1. Surge. [star] Boost: deal yourself 1 facedown encounter card.
instance RunMessage TimeDisplacedSoldier where
  runMessage msg e@(TimeDisplacedSoldier attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      dealEncounterCard you
      pure e
    _ -> TimeDisplacedSoldier <$> runMessage msg attrs
