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
import Marvel.Source (Source, IsSource(..))
import Marvel.Source qualified as Source
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
  type Id Effect = EffectId
  data Attrs Effect = EffectAttrs
    { effectId :: EffectId
    , effectCardCode :: CardCode
    , effectSource :: Source
    , effectMatcher :: EntityMatcher
    , effectModifiers :: [Modifier]
    , effectEnds :: Maybe EffectTiming
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
  data Field Effect :: Type -> Type where
    EffectId :: Field Effect EffectId
    EffectCardCode :: Field Effect CardCode
    EffectSource :: Field Effect Source
    EffectMatcher :: Field Effect EntityMatcher
    EffectModifiers :: Field Effect [Modifier]
    EffectEnds :: Field Effect (Maybe EffectTiming)
  toId = effectId . toAttrs
  toAttrs (Effect a) = toEffectAttrs a

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

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, Entity a, Id a ~ EffectId, RunMessage a) => IsEffect a where
  toEffectAttrs :: a -> Attrs Effect
  default toEffectAttrs :: Coercible a (Attrs Effect) => a -> Attrs Effect
  toEffectAttrs = coerce

type CardEffect a = CardBuilder (Source, EntityMatcher, EffectId) a

data EffectTiming = DisableAtEndOfPhase | DisableAtEndOfRound
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

modifiersL :: Lens' (Attrs Effect) [Modifier]
modifiersL = lens effectModifiers $ \m x -> m { effectModifiers = x }

endsL :: Lens' (Attrs Effect) (Maybe EffectTiming)
endsL = lens effectEnds $ \m x -> m { effectEnds = x }

instance IsTarget (Attrs Effect) where
  toTarget = EffectTarget . effectId

instance IsSource (Attrs Effect) where
  toSource = Source.EffectSource . effectId

effectWith
  :: (Attrs Effect -> a)
  -> CardDef
  -> (Attrs Effect -> Attrs Effect)
  -> CardBuilder (Source, EntityMatcher, EffectId) a
effectWith f cardDef g = effect (f . g) cardDef

effect
  :: (Attrs Effect -> a)
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

effectValidFor :: MonadGame env m => Attrs Effect -> Target -> m Bool
effectValidFor e target = case (target, effectMatcher e) of
  (IdentityTarget ident, IdentityEntity matcher) ->
    member ident <$> select matcher
  (AllyTarget ident, AllyEntity matcher) -> member ident <$> select matcher
  _ -> pure False

instance RunMessage (Attrs Effect) where
  runMessage msg a = case msg of
    EffectMessage ident msg' | ident == effectId a -> case msg' of
      DisableEffect -> a <$ push (DisabledEffect $ effectId a)
      _ -> pure a
    EndPhase _ -> a <$ when
      (effectEnds a == Just DisableAtEndOfPhase)
      (push $ EffectMessage (effectId a) DisableEffect)
    EndRound -> a <$ when
      (effectEnds a == Just DisableAtEndOfRound)
      (push $ EffectMessage (effectId a) DisableEffect)
    _ -> pure a
