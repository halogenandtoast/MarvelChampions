module Marvel.Phase where

import Marvel.Prelude

data Phase = PlayerPhase | VillainPhase
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
