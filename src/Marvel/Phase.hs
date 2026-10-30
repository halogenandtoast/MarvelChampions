module Marvel.Phase where

import Marvel.Prelude

data Phase = PlayerPhase | VillainPhase
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

-- | A step of the villain phase that is several messages, one for each player, told in
-- the log once as it begins.
data VillainPhaseStep = StepEnemiesActivate | StepDealEncounterCards | StepRevealEncounterCards
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
