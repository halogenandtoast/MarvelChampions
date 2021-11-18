module Marvel.Effect.Attrs where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Modifier
import Marvel.Source
import Marvel.Target

class IsEffect a

type CardEffect a = CardBuilder (Source, Target, EffectId) a

data EffectAttrs = EffectAttrs
  { effectId :: EffectId
  , effectSource :: Source
  , effectTarget :: Target
  , effectModifiers :: [Modifier]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

modifiersL :: Lens' EffectAttrs [Modifier]
modifiersL = lens effectModifiers $ \m x -> m { effectModifiers = x }

instance IsTarget EffectAttrs where
  toTarget = EffectTarget . effectId

instance IsSource EffectAttrs where
  toSource = EffectSource . effectId

effectWith
  :: (EffectAttrs -> a)
  -> CardDef
  -> (EffectAttrs -> EffectAttrs)
  -> CardBuilder (Source, Target, EffectId) a
effectWith f cardDef g = effect (f . g) cardDef

effect
  :: (EffectAttrs -> a) -> CardDef -> CardBuilder (Source, Target, EffectId) a
effect f cardDef = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \(source, target, eid) -> f $ EffectAttrs
    { effectId = eid
    , effectSource = source
    , effectTarget = target
    , effectModifiers = mempty
    }
  }

instance Entity EffectAttrs where
  type EntityId EffectAttrs = EffectId
  type EntityAttrs EffectAttrs = EffectAttrs
  toId = effectId
  toAttrs = id

instance RunMessage EffectAttrs where
  runMessage _ = pure
