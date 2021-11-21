module Marvel.Aspect where

import Marvel.Prelude

data Aspect = Leadership | Aggression | Protection | Justice | Neutral
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
