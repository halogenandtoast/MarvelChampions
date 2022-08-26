module Api.Handler.Marvel.Heroes
  ( getApiV1MarvelHeroesR
  ) where

import Import

import Data.UUID (nil)
import Marvel.Hero
import Marvel.Hero.Types
import Marvel.Card.Builder

getApiV1MarvelHeroesR :: Handler [Hero]
getApiV1MarvelHeroesR = pure $ flip map (toList allHeroes) $ \case
  SomeHeroCard a -> Hero $ cbCardBuilder a (coerce nil)
