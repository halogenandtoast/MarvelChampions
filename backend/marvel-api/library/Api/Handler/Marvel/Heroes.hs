module Api.Handler.Marvel.Heroes
  ( getApiV1MarvelHeroesR
  ) where

import Import

import Data.UUID (nil)
import Marvel.Hero
import Marvel.Id

getApiV1MarvelHeroesR :: Handler [Hero]
getApiV1MarvelHeroesR = pure $ map ($ coerce nil) $ toList allHeroes
