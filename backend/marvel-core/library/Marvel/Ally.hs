{-# LANGUAGE TemplateHaskell #-}
module Marvel.Ally where

import Marvel.Prelude

import Marvel.Ally.Allies
import Marvel.Ally.Attrs
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.PlayerCard
import Marvel.Id
import Marvel.TH

$(buildEntity "Ally")

allAllies :: HashMap CardCode (IdentityId -> AllyId -> Ally)
allAllies = fromList $ map
  (toCardCode &&& (curry . cbCardBuilder))
  $(buildEntityLookupList "Ally")
