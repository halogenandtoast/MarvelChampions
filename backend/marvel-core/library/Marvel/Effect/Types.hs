module Marvel.Effect.Types where

import Marvel.Prelude

import Data.Typeable
import Marvel.Card.Builder
import Marvel.Card.Code
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
import Text.Show qualified

data Effect = forall a . IsEffect a => Effect a

instance Show Effect where
  show (Effect a) = show a

instance Eq Effect where
  (Effect (a :: a)) == (Effect (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance ToJSON Effect where
  toJSON (Effect a) = toJSON a

instance RunMessage Effect where
  runMessage msg (Effect a) = Effect <$> runMessage msg a

instance Entity Effect where
  type EntityId Effect = EffectId
  type EntityAttrs Effect = EffectAttrs
  toId = toId . toAttrs
  toAttrs (Effect a) = toAttrs a

data SomeCardEffect = forall a . IsEffect a => SomeCardEffect (CardEffect a)

liftCardEffect :: (forall a . CardEffect a -> b) -> SomeCardEffect -> b
liftCardEffect f (SomeCardEffect a) = f a

someCardEffectCardCode :: SomeCardEffect -> CardCode
someCardEffectCardCode = liftCardEffect cbCardCode

instance HasModifiersFor Effect where
  getModifiersFor _ target e = do
    valid <- effectValidFor attrs target
    pure $ if valid then effectModifiers attrs else []
    where attrs = toAttrs e

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, Entity a, EntityAttrs a ~ EffectAttrs, EntityId a ~ EffectId, RunMessage a) => IsEffect a

type CardEffect a = CardBuilder (Source, EntityMatcher, EffectId) a

data EffectTiming = DisableAtEndOfPhase | DisableAtEndOfRound
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EffectAttrs = EffectAttrs
  { effectId :: EffectId
  , effectCardCode :: CardCode
  , effectSource :: Source
  , effectMatcher :: EntityMatcher
  , effectModifiers :: [Modifier]
  , effectEnds :: Maybe EffectTiming
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

modifiersL :: Lens' EffectAttrs [Modifier]
modifiersL = lens effectModifiers $ \m x -> m { effectModifiers = x }

endsL :: Lens' EffectAttrs (Maybe EffectTiming)
endsL = lens effectEnds $ \m x -> m { effectEnds = x }

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
    , effectCardCode = cdCardCode cardDef
    , effectSource = source
    , effectMatcher = matcher
    , effectModifiers = mempty
    , effectEnds = Nothing
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
    EndPhase _ -> a <$ when
      (effectEnds a == Just DisableAtEndOfPhase)
      (push $ EffectMessage (toId a) DisableEffect)
    EndRound -> a <$ when
      (effectEnds a == Just DisableAtEndOfRound)
      (push $ EffectMessage (toId a) DisableEffect)
    _ -> pure a
