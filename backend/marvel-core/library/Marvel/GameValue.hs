module Marvel.GameValue where

import Marvel.Prelude

import Marvel.Game.Source

data GameValue = Static Int | PerPlayer Int | PerPlayerWithStatic Int Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

fromGameValue :: MonadGame env m => GameValue -> m Int
fromGameValue (Static n) = pure n
fromGameValue (PerPlayer n) = do
  playerCount <- getPlayerCount
  pure $ n * playerCount
fromGameValue (PerPlayerWithStatic n z) = do
  playerCount <- getPlayerCount
  pure $ n * playerCount + z
