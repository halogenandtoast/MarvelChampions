module Api.Handler.Marvel.Cards
  ( getApiV1MarvelCardsR
  ) where

import Import

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.PlayerCard

getApiV1MarvelCardsR :: Handler [CardDef]
getApiV1MarvelCardsR =
  pure $ filter ((/= "01000") . toCardCode) $ toList allPlayerCards
