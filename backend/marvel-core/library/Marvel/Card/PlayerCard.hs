{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Card.PlayerCard
  ( module Marvel.Card.PlayerCard.Types
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.PlayerCard.Types
import Marvel.Resource

instance HasCardCode PlayerCard where
  toCardCode = toCardCode . getCardDef

instance HasCardDef PlayerCard where
  getCardDef = pcCardDef

instance HasResources PlayerCard where
  resourcesFor x mc = pure $ do
    guard $ Just x /= mc
    map snd $ filter isValidResource $ cdResources $ getCardDef x
   where
    isValidResource (restriction, _) = case restriction of
      PrintedResource -> True
      ResourceForCardsMatching matcher -> maybe False (cardMatch matcher) mc
