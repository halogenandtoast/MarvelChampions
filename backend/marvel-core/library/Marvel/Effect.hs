module Marvel.Effect where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Effect.Attrs
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Support.Supports

newtype Effect = HelicarrierEffect' HelicarrierEffect
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance RunMessage Effect where
  runMessage = genericRunMessage

instance Entity Effect where
  type EntityId Effect = EffectId
  type EntityAttrs Effect = EffectAttrs
  toId = toId . toAttrs
  toAttrs = genericToAttrs

allEffects :: HashMap CardCode (EffectId -> Effect)
allEffects = fromList [("01092", HelicarrierEffect' . helicarrierEffect)]

lookupEffect :: CardCode -> (EffectId -> Effect)
lookupEffect cardCode = case lookup cardCode allEffects of
  Just f -> f
  Nothing -> error "Invalid card code"
