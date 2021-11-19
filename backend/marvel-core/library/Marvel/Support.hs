{-# LANGUAGE TemplateHaskell #-}
module Marvel.Support where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Source
import Marvel.Support.Attrs
import Marvel.Support.Supports
import Marvel.TH

$(buildEntity "Support")

allSupports :: HashMap CardCode (IdentityId -> SupportId -> Support)
allSupports = fromList $ map
  (toCardCode &&& (curry . cbCardBuilder))
  $(buildEntityLookupList "Support")

lookupSupport :: CardCode -> (IdentityId -> SupportId -> Support)
lookupSupport cardCode = case lookup cardCode allSupports of
  Just f -> f
  Nothing -> error $ "Invalid card code for support " <> show cardCode

instance Entity Support where
  type EntityId Support = SupportId
  type EntityAttrs Support = SupportAttrs
  toId = toId . toAttrs
  toAttrs = genericToAttrs

instance RunMessage Support where
  runMessage = genericRunMessage

instance Exhaustable Support where
  isExhausted = supportExhausted . toAttrs

instance IsSource Support where
  toSource = SupportSource . toId

instance HasAbilities Support where
  getAbilities = genericGetAbilities

getSupportController :: Support -> IdentityId
getSupportController = supportController . toAttrs
