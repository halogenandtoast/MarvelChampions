module Marvel.Resource where

import Marvel.Prelude

data Resource = Physical | Mental | Energy | Wild
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
