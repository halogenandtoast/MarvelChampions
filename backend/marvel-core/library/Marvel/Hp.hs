module Marvel.Hp where

import Marvel.Prelude

import Marvel.GameValue

newtype HP a = HP { unHp :: a }
  deriving newtype (Show, Eq, ToJSON, FromJSON)

class HasStartingHP a where
  startingHP :: a -> HP GameValue
