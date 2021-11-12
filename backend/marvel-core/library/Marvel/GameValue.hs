module Marvel.GameValue where

import Marvel.Prelude

import Marvel.Game.Source

data GameValue = Static Int | PerPlayer Int | PerPlayerWithStatic Int Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

fromGameValue :: MonadGame env m => GameValue -> m Int
fromGameValue gv = gameValue gv <$> getPlayerCount

gameValue :: GameValue -> Int -> Int
gameValue (Static n) _ = n
gameValue (PerPlayer n) playerCount = n * playerCount
gameValue (PerPlayerWithStatic n z) playerCount = n * playerCount + z
