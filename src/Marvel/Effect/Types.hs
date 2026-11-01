module Marvel.Effect.Types where

import Marvel.Prelude

import Data.Typeable
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id as X (EffectId, EnemyId (..))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

data Effect = forall a. (IsEffect a) => Effect a

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
  field fld e =
    let EffectAttrs {..} = toAttrs e
     in case fld of
          EffectId -> effectId
          EffectCardCode -> effectCardCode
          EffectSource -> effectSource
          EffectMatcher -> effectMatcher
          EffectModifiers -> effectModifiers
          EffectEnds -> effectEnds
  toId = effectId . toAttrs
  toAttrs (Effect a) = toEffectAttrs a

data SomeCardEffect = forall a. (IsEffect a) => SomeCardEffect (CardEffect a)

liftCardEffect :: (forall a. CardEffect a -> b) -> SomeCardEffect -> b
liftCardEffect f (SomeCardEffect a) = f a

someCardEffectCardCode :: SomeCardEffect -> CardCode
someCardEffectCardCode = liftCardEffect toCardCode

instance HasModifiersFor Effect where
  getBlankingModifiersFor source target e
    | Blank `elem` effectModifiers (toAttrs e) = filter (== Blank) <$> getModifiersFor source target e
    | otherwise = pure []
  getModifiersFor _ target e@(Effect inner) = do
    valid <- effectValidFor attrs target
    if valid then getEffectModifiers inner else pure []
   where
    attrs = toAttrs e

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, RunMessage a) => IsEffect a where
  toEffectAttrs :: a -> Attrs Effect
  default toEffectAttrs :: (Coercible a (Attrs Effect)) => a -> Attrs Effect
  toEffectAttrs = coerce

  -- | Lasting effects can depend on the current board, such as an ally remaining in play.
  getEffectModifiers :: (HasGame m) => a -> m [Modifier]
  getEffectModifiers = pure . effectModifiers . toEffectAttrs

type CardEffect a = CardBuilder (Source, EntityMatcher, EffectId) a

data EffectTiming
  = DisableAtEndOfPhase
  | DisableAtEndOfRound
  | -- | at the end of the turn of the identity it is on: "until the end of this turn", or
    -- from the villain phase "until your next turn ends"
    DisableAtEndOfTurn
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

modifiersL :: Lens' (Attrs Effect) [Modifier]
modifiersL = lens effectModifiers $ \m x -> m {effectModifiers = x}

endsL :: Lens' (Attrs Effect) (Maybe EffectTiming)
endsL = lens effectEnds $ \m x -> m {effectEnds = x}

instance IsRef (Attrs Effect) where
  toRef = EffectRef . effectId

effectWith ::
  (Attrs Effect -> a) ->
  CardDef ->
  (Attrs Effect -> Attrs Effect) ->
  CardBuilder (Source, EntityMatcher, EffectId) a
effectWith f cardDef g = effect (f . g) cardDef

effect ::
  (Attrs Effect -> a) ->
  CardDef ->
  CardBuilder (Source, EntityMatcher, EffectId) a
effect f cardDef =
  CardBuilder
    { cbCardDef = cardDef
    , cbCardBuilder = \cardDef (source, matcher, eid) ->
        f $
          EffectAttrs
            { effectId = eid
            , effectCardCode = cdCardCode cardDef
            , effectSource = source
            , effectMatcher = matcher
            , effectModifiers = mempty
            , effectEnds = Nothing
            }
    }

effectValidFor :: (HasGame m) => Attrs Effect -> Target -> m Bool
effectValidFor e target = case (target, effectMatcher e) of
  (_, SourceEntity source) -> pure $ refocus target == source
  -- the target first: matching every identity or ally runs their modifiers, which
  -- each run this again
  (IdentityRef ident, IdentityEntity matcher) -> selectAny (IdentityWithId ident <> matcher)
  (VillainRef vid, EnemyEntity matcher) -> selectAny (EnemyWithId (EnemyVillainId vid) <> matcher)
  (MinionRef mid, EnemyEntity matcher) -> selectAny (EnemyWithId (EnemyMinionId mid) <> matcher)
  (VillainRef _, VillainsAndEngagedMinions _) -> pure True
  (MinionRef mid, VillainsAndEngagedMinions you) -> selectAny (MinionWithId mid <> MinionEngagedWith (IdentityWithId you))
  (MinionRef ident, MinionEntity matcher) -> selectAny (MinionWithId ident <> matcher)
  (AllyRef ident, AllyEntity matcher) -> selectAny (AllyWithId ident <> matcher)
  (UpgradeRef ident, UpgradeEntity matcher) -> selectAny (UpgradeWithId ident <> matcher)
  (EventRef ident, EventEntity ident') -> pure $ ident == ident'
  (EventRef eid, EventsMatching matcher) -> maybe False (cardMatch matcher) <$> getsGame (`abilityCardDef` EventRef eid)
  (MainSchemeRef ident, MainSchemeEntity ident') -> pure $ ident == ident'
  (AttachmentRef ident, AttachmentEntity matcher) -> selectAny (AttachmentWithId ident <> matcher)
  (SideSchemeRef ident, SideSchemeEntity matcher) -> elem ident <$> selectList matcher
  _ -> pure False

instance RunMessage (Attrs Effect) where
  runMessage msg a = runQueueT $ case msg of
    EffectMessage ident msg' | ident == effectId a -> case msg' of
      DisableEffect -> a <$ effectDisabled (effectId a)
      _ -> pure a
    -- an effect on an event lasts while the event resolves
    EventMessage eid ResolvedEvent
      | effectMatcher a == EventEntity eid ->
          a <$ disableEffect (effectId a)
    EndPhase _ -> a <$ when (effectEnds a == Just DisableAtEndOfPhase) (disableEffect $ effectId a)
    EndRound -> a <$ when (effectEnds a == Just DisableAtEndOfRound) (disableEffect $ effectId a)
    IdentityEndedTurn ident | effectEnds a == Just DisableAtEndOfTurn -> do
      ends <- effectValidFor a (IdentityRef ident)
      a <$ when ends (disableEffect $ effectId a)
    _ -> pure a
