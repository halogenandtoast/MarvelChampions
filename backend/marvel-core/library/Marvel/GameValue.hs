module Marvel.GameValue
  ( module Marvel.GameValue
  , module Marvel.GameValue.Types
  ) where

import Marvel.Prelude

import Marvel.Game.Source
import Marvel.GameValue.Types

fromGameValue :: MonadGame env m => GameValue -> m Int
fromGameValue gv = gameValue gv <$> getPlayerCount

gameValue :: GameValue -> Int -> Int
gameValue (Static n) _ = n
gameValue (PerPlayer n) playerCount = n * playerCount
gameValue (PerPlayerWithStatic n z) playerCount = n * playerCount + z
