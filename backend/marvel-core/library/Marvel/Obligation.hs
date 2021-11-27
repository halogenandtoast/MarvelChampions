{-# LANGUAGE TemplateHaskell #-}
module Marvel.Obligation where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Obligation.Attrs
import Marvel.Obligation.Obligations
import Marvel.Source
import Marvel.TH

$(buildEntity "Obligation")

allObligations :: HashMap CardCode (ObligationId -> Obligation)
allObligations = fromList $ map
  (toCardCode &&& cbCardBuilder)
  $(buildEntityLookupList "Obligation")

lookupObligation :: CardCode -> (ObligationId -> Obligation)
lookupObligation cardCode = case lookup cardCode allObligations of
  Just f -> f
  Nothing -> error $ "Invalid card code for obligation " <> show cardCode

instance Entity Obligation where
  type EntityId Obligation = ObligationId
  type EntityAttrs Obligation = ObligationAttrs
  toId = toId . toAttrs
  toAttrs = genericToAttrs

instance RunMessage Obligation where
  runMessage = genericRunMessage

instance IsSource Obligation where
  toSource = ObligationSource . toId

instance HasCardDef Obligation where
  getCardDef = getCardDef . toAttrs
