{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Card.PlayerCard
  ( module Marvel.Card.PlayerCard.Types
  , printedResourcesFor
  ) where

import Marvel.Prelude

import Marvel.Card.Def
import Data.HashSet qualified as HashSet
import Marvel.Card.PlayerCard.Types
import Marvel.Game.Source
import Marvel.Matchers.Types
import Marvel.Modifier.Types (Modifier (PrintedResourcesAs))
import Marvel.Resource

instance HasResources PlayerCard where
  resourcesFor x mc
    | Just x == mc = pure []
    | otherwise = do
        modifiers <- getModifiers x
        let replace r = fromMaybe r (headMay [r' | PrintedResourcesAs r' <- modifiers])
        concatMapM (\(restriction, r) -> resource (restriction, if restriction == PrintedResource then replace r else r)) $ cdResources $ getCardDef x
   where
    resource (restriction, r) = case restriction of
      PrintedResource -> pure [r]
      ResourceForCardsMatching matcher -> pure [r | maybe False (cardMatch matcher) mc]
      ForEachAllyControlled n -> case pcOwner x of
        Nothing -> pure []
        Just owner -> do
          allies <- HashSet.size <$> gameSelectAlly (AllyControlledBy $ IdentityWithId owner)
          pure $ replicate (min (fromIntegral n) allies) r

-- | Printed icons as they are treated while the card is in its current zone.
printedResourcesFor :: (HasGame m) => PlayerCard -> m [Resource]
printedResourcesFor card = do
  modifiers <- getModifiers card
  let symbols = printedResources (getCardDef card)
  pure $ case [r | PrintedResourcesAs r <- modifiers] of
    r : _ -> map (const r) symbols
    [] -> symbols
