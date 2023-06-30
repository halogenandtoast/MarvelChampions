module Api.Handler.Marvel.Heroes (
  getApiV1MarvelHeroesR,
) where

import Import

import Data.Coerce (coerce)
import Data.HashMap.Strict (elems)
import Data.UUID (nil)
import Marvel.Card.Builder
import Marvel.Hero
import Marvel.Hero.Types

getApiV1MarvelHeroesR :: Handler [Hero]
getApiV1MarvelHeroesR = pure $ flip map (elems allHeroes) $ \case
  SomeHeroCard a -> Hero $ cbCardBuilder a (coerce nil)
