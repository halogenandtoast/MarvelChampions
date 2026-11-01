module Marvel.Aspect where

import Marvel.Prelude

import Data.List (nub)

data Aspect = Leadership | Aggression | Protection | Justice | Pool | Neutral
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

-- | How many different aspects are among the cards:
-- "for each different aspect discarded this way" (Adam Warlock's cards).
-- A basic card or a hero's own is none of them.
distinctAspects :: [Maybe Aspect] -> Natural
distinctAspects = fromIntegral . length . nub . filter (/= Neutral) . catMaybes
