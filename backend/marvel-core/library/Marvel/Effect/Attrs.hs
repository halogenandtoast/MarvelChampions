module Marvel.Effect.Attrs where

import Marvel.Prelude

import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Modifier
import Marvel.Source
import Marvel.Target

class IsEffect a

data EffectAttrs = EffectAttrs
  { effectId :: EffectId
  , effectSource :: Source
  , effectTarget :: Target
  , effectModifiers :: [Modifier]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance IsTarget EffectAttrs where
  toTarget = EffectTarget . effectId

instance IsSource EffectAttrs where
  toSource = EffectSource . effectId

instance Entity EffectAttrs where
  type EntityId EffectAttrs = EffectId
  type EntityAttrs EffectAttrs = EffectAttrs
  toId = effectId
  toAttrs = id

instance RunMessage EffectAttrs where
  runMessage _ = pure
