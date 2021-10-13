{-# LANGUAGE TemplateHaskell #-}
module Marvel.AlterEgo where

import Marvel.Prelude

import Marvel.AlterEgo.AlterEgos
import Marvel.AlterEgo.Attrs
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.TH

$(buildEntity "AlterEgo")

allAlterEgos :: HashMap CardCode (IdentityId -> AlterEgo)
allAlterEgos = fromList
  $ map (toCardCode &&& cbCardBuilder) $(buildEntityLookupList "AlterEgo")

instance HasStartingHP AlterEgo where
  startingHP = defaultHasStartingHP

instance HasIdentityAttrs AlterEgo where
  toIdentityAttrs = genericToIdentityAttrs
