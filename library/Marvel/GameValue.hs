module Marvel.GameValue where

import Marvel.Prelude

data GameValue = Static Int | PerPlayer Int | PerPlayerWithStatic Int Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
