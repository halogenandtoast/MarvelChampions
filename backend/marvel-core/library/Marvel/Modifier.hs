module Marvel.Modifier where

import Marvel.Prelude

newtype Modifier = ResourceCostReduction Natural
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
