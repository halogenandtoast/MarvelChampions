{-# LANGUAGE TemplateHaskell #-}
module Marvel.Villain where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Entity
import Marvel.TH
import Marvel.Villain.Attrs
import Marvel.Villain.Villains.Klaw
import Marvel.Villain.Villains.Rhino

$(buildEntity "Villain")

instance Entity Villain where
  type EntityId Villain = VillainId
  type EntityAttrs Villain = VillainAttrs
  toId = toId . toAttrs
  toAttrs = genericToAttrs

lookupVillain :: CardCode -> VillainId -> Maybe Villain
lookupVillain cardCode villainId =
  lookup cardCode allVillains <*> pure villainId

allVillains :: HashMap CardCode (VillainId -> Villain)
allVillains = fromList
  $ map (toCardCode &&& cbCardBuilder) $(buildEntityLookupList "Villain")