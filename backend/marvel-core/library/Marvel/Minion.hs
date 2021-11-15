{-# LANGUAGE TemplateHaskell #-}
module Marvel.Minion where

import Marvel.Prelude

import Marvel.Minion.Minions
import Marvel.Minion.Attrs
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Source
import Marvel.TH

$(buildEntity "Minion")

allMinions :: HashMap CardCode (MinionId -> Minion)
allMinions = fromList $ map
  (toCardCode &&& cbCardBuilder)
  $(buildEntityLookupList "Minion")

lookupMinion :: CardCode -> (MinionId -> Minion)
lookupMinion cardCode = case lookup cardCode allMinions of
  Just f -> f
  Nothing -> error "Invalid card code"

instance Entity Minion where
  type EntityId Minion = MinionId
  type EntityAttrs Minion = MinionAttrs
  toId = toId . toAttrs
  toAttrs = genericToAttrs

instance RunMessage Minion where
  runMessage = genericRunMessage

instance IsSource Minion where
  toSource = MinionSource . toId
