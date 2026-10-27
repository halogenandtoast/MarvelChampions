module Marvel.GameValue
  ( module Marvel.GameValue
  , module Marvel.GameValue.Types
  ) where

import Marvel.Prelude

import Marvel.Game.Source
import Marvel.GameValue.Types

-- | A per player value counts the players who started the scenario: an eliminated
-- player still counts (RR Per Player Icon).
fromGameValue :: HasGame m => GameValue -> m Int
fromGameValue gv = gameValue gv <$> getStartingPlayerCount

gameValue :: GameValue -> Int -> Int
gameValue (Static n) _ = n
gameValue (PerPlayer n) playerCount = n * playerCount
gameValue (PerPlayerWithStatic n z) playerCount = n * playerCount + z
