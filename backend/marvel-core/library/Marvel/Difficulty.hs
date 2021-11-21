module Marvel.Difficulty where

import Marvel.Prelude

data Difficulty = Normal | Expert
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
