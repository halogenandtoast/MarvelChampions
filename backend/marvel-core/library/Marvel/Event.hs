{-# LANGUAGE TemplateHaskell #-}
module Marvel.Event where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Attrs
import Marvel.Event.Events
import Marvel.Id
import Marvel.Message
import Marvel.TH

$(buildEntity "Event")

allEvents :: HashMap CardCode (IdentityId -> EventId -> Event)
allEvents = fromList $ map
  (toCardCode &&& (curry . cbCardBuilder))
  $(buildEntityLookupList "Event")

lookupEvent :: CardCode -> (IdentityId -> EventId -> Event)
lookupEvent cardCode = case lookup cardCode allEvents of
  Just f -> f
  Nothing -> error "Invalid card code"

instance Entity Event where
  type EntityId Event = EventId
  type EntityAttrs Event = EventAttrs
  toId = toId . toAttrs
  toAttrs = genericToAttrs

instance RunMessage Event where
  runMessage = genericRunMessage
