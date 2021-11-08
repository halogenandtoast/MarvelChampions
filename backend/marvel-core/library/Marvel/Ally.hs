{-# LANGUAGE TemplateHaskell #-}
module Marvel.Ally where

import Marvel.Prelude

import Marvel.Ally.Allies
import Marvel.Ally.Attrs
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Id
import Marvel.TH

$(buildEntity "Ally")

allAllies :: HashMap CardCode (IdentityId -> AllyId -> Ally)
allAllies = fromList $ map
  (toCardCode &&& (curry . cbCardBuilder))
  $(buildEntityLookupList "Ally")

lookupAlly :: CardCode -> (IdentityId -> AllyId -> Ally)
lookupAlly cardCode = case lookup cardCode allAllies of
  Just f -> f
  Nothing -> error "Invalid card code"

instance Entity Ally where
  type EntityId Ally = AllyId
  type EntityAttrs Ally = AllyAttrs
  toId = toId . toAttrs
  toAttrs = genericToAttrs
