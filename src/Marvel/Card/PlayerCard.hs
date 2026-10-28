{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Card.PlayerCard
  ( module Marvel.Card.PlayerCard.Types
  ) where

import Marvel.Prelude

import Marvel.Card.Def
import Data.HashSet qualified as HashSet
import Marvel.Card.PlayerCard.Types
import Marvel.Game.Source
import Marvel.Matchers.Types
import Marvel.Resource

instance HasResources PlayerCard where
  resourcesFor x mc
    | Just x == mc = pure []
    | otherwise = concatMapM resource $ cdResources $ getCardDef x
   where
    resource (restriction, r) = case restriction of
      PrintedResource -> pure [r]
      ResourceForCardsMatching matcher -> pure [r | maybe False (cardMatch matcher) mc]
      ForEachAllyControlled n -> case pcOwner x of
        Nothing -> pure []
        Just owner -> do
          allies <- HashSet.size <$> gameSelectAlly (AllyControlledBy $ IdentityWithId owner)
          pure $ replicate (min (fromIntegral n) allies) r
