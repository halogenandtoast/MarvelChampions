module Marvel.Effect where

import Marvel.Prelude

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
