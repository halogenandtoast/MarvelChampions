{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Card.PlayerCard
  ( module Marvel.Card.PlayerCard.Types
  , printedResourcesFor
  ) where

import Marvel.Prelude

import Marvel.Card.Def
import Marvel.Area (areaId)
import Data.HashSet qualified as HashSet
import Marvel.Card.PlayerCard.Types
import Marvel.Game.Source
import Marvel.Matchers.Types
import Marvel.Modifier.Types (Modifier (PrintedResourcesAs))
import Marvel.Resource
import Marvel.Ref (toSource)
import Marvel.Id (CharacterId (IdentityCharacter))

instance HasResources PlayerCard where
  resourcesFor x mc
    | Just x == mc = pure []
    | canonicalCardCode (getCardDef x) == "50014" = do
        more <- maybe (pure []) organizationalSupportPotential (pcController x <|> pcOwner x)
        pure $ printedResources (getCardDef x) <> more
    | otherwise = do
        modifiers <- getModifiers x
        holder <- getSourceController (toSource x)
        let controller = holder <|> pcController x <|> pcOwner x
        let replace r = fromMaybe r (headMay [r' | PrintedResourcesAs r' <- modifiers])
        -- The Self resources double/triple the printed resource they generate,
        -- including a replacement of that printed resource (for example Haywire).
        let generated restriction r = case restriction of
              PrintedResource -> replace r
              ResourceIfIdentityDamageLessThan _ -> replace r
              _ -> r
        concatMapM (\(restriction, r) -> resource controller (restriction, generated restriction r)) $ cdResources $ getCardDef x
   where
    resource controller (restriction, r) = case restriction of
      PrintedResource -> pure [r]
      ResourceForCardsMatching matcher -> pure [r | maybe False (cardMatch matcher) mc]
      ForEachAllyControlled n -> case controller of
        Nothing -> pure []
        Just owner -> do
          allies <- HashSet.size <$> gameSelectAlly (AllyControlledBy $ IdentityWithId owner)
          pure $ replicate (min (fromIntegral n) allies) r
      ForEachMainSchemeAccelerationToken n -> case controller of
        Nothing -> pure []
        Just owner -> do
          area <- getAreaOf owner
          tokens <- getMainSchemeTokenCountIn (areaId area)
          pure $ replicate (fromIntegral $ min n tokens) r
      ResourceIfIdentityDamageLessThan n -> case controller of
        Nothing -> pure []
        Just owner -> do
          damage <- characterStoredDamage (IdentityCharacter owner)
          pure [r | damage < n]

-- | Printed icons as they are treated while the card is in its current zone.
printedResourcesFor :: (HasGame m) => PlayerCard -> m [Resource]
printedResourcesFor card = do
  modifiers <- getModifiers card
  let symbols = printedResources (getCardDef card)
  pure $ case [r | PrintedResourcesAs r <- modifiers] of
    r : _ -> map (const r) symbols
    [] -> symbols
