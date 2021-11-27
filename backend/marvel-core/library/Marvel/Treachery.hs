{-# LANGUAGE TemplateHaskell #-}
module Marvel.Treachery where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Source
import Marvel.TH
import Marvel.Target
import Marvel.Treachery.Attrs
import Marvel.Treachery.Treacheries

$(buildEntity "Treachery")

allTreacheries :: HashMap CardCode (TreacheryId -> Treachery)
allTreacheries = fromList $ map
  (toCardCode &&& cbCardBuilder)
  $(buildEntityLookupList "Treachery")

lookupTreachery :: CardCode -> (TreacheryId -> Treachery)
lookupTreachery cardCode = case lookup cardCode allTreacheries of
  Just f -> f
  Nothing -> error $ "Invalid card code for treachery " <> show cardCode

instance Entity Treachery where
  type EntityId Treachery = TreacheryId
  type EntityAttrs Treachery = TreacheryAttrs
  toId = toId . toAttrs
  toAttrs = genericToAttrs

instance RunMessage Treachery where
  runMessage = genericRunMessage

instance IsSource Treachery where
  toSource = TreacherySource . toId

instance IsTarget Treachery where
  toTarget = TreacheryTarget . toId

instance HasCardDef Treachery where
  getCardDef = getCardDef . toAttrs
