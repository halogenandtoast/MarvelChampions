{-# LANGUAGE TemplateHaskell #-}
module Marvel.AlterEgo where

import Marvel.Prelude

import Marvel.AlterEgo.AlterEgos
import Marvel.AlterEgo.Attrs
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Message
import Marvel.TH

$(buildEntity "AlterEgo")

instance RunMessage AlterEgo where
  runMessage = genericRunMessage

allAlterEgos :: HashMap CardCode (IdentityId -> AlterEgo)
allAlterEgos = fromList
  $ map (toCardCode &&& cbCardBuilder) $(buildEntityLookupList "AlterEgo")

instance HasStartingHP AlterEgo where
  startingHP = defaultHasStartingHP

instance HasIdentityAttrs AlterEgo where
  toIdentityAttrs = genericToIdentityAttrs
