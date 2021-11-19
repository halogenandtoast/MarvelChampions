module Marvel.Effect.Attrs where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Queue
import Marvel.Source
import Marvel.Target

class IsEffect a

type CardEffect a = CardBuilder (Source, EntityMatcher, EffectId) a

data EffectAttrs = EffectAttrs
  { effectId :: EffectId
  , effectSource :: Source
  , effectMatcher :: EntityMatcher
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
  -> CardBuilder (Source, EntityMatcher, EffectId) a
effectWith f cardDef g = effect (f . g) cardDef

effect
  :: (EffectAttrs -> a)
  -> CardDef
  -> CardBuilder (Source, EntityMatcher, EffectId) a
effect f cardDef = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \(source, matcher, eid) -> f $ EffectAttrs
    { effectId = eid
    , effectSource = source
    , effectMatcher = matcher
    , effectModifiers = mempty
    }
  }

effectValidFor :: MonadGame env m => EffectAttrs -> Target -> m Bool
effectValidFor e target = case (target, effectMatcher e) of
  (IdentityTarget ident, IdentityEntity matcher) ->
    member ident <$> select matcher
  (AllyTarget ident, AllyEntity matcher) -> member ident <$> select matcher
  _ -> pure False

instance Entity EffectAttrs where
  type EntityId EffectAttrs = EffectId
  type EntityAttrs EffectAttrs = EffectAttrs
  toId = effectId
  toAttrs = id

instance RunMessage EffectAttrs where
  runMessage msg a = case msg of
    EffectMessage ident msg' | ident == toId a -> case msg' of
      DisableEffect -> a <$ push (DisabledEffect $ toId a)
    _ -> pure a
