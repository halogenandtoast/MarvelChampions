module Marvel.Support.Types where

import Marvel.Prelude

import Data.Aeson.KeyMap qualified as KeyMap
import Data.Typeable
import Marvel.Ability.Types
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.Types
import Marvel.Entity
import Marvel.Game.Source (getModifiers)
import Marvel.Matchers.Types (IdentityMatcher (FirstPlayer))
import Marvel.Query (selectOne)
import Marvel.Id hiding (SupportId)
import Marvel.Id as X (SupportId)
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

data Support = forall a. (IsSupport a) => Support a

instance Show Support where
  show (Support a) = show a

instance ToJSON Support where
  toJSON (Support a) = toJSON a

instance Eq Support where
  Support (a :: a) == Support (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

data SomeSupportCard
  = forall a.
    (IsSupport a) =>
    SomeSupportCard
      (SupportCard a)

liftSupportCard :: (forall a. SupportCard a -> b) -> SomeSupportCard -> b
liftSupportCard f (SomeSupportCard a) = f a

someSupportCardCode :: SomeSupportCard -> CardCode
someSupportCardCode = liftSupportCard toCardCode

instance Entity Support where
  type Id Support = SupportId
  data Attrs Support = SupportAttrs
    { supportId :: SupportId
    , supportCardDef :: CardDef
    , supportController :: IdentityId
    , supportOwner :: IdentityId
    -- ^ whose deck it came from, and whose discard pile it goes to
    , supportExhausted :: Bool
    , supportUses :: Natural
    , supportDiscardIfNoUses :: Bool
    , supportHeldCards :: [PlayerCard]
    -- ^ cards attached here facedown
    , supportCanPlayHeldEvents :: Bool
    -- ^ a printed permission; Bruno's stored cards have no such permission
    , supportThreat :: Natural
    -- ^ threat moved here, as Beat Cop's
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON)
  data Field Support :: Type -> Type where
    SupportId :: Field Support SupportId
    SupportCardDef :: Field Support CardDef
    SupportController :: Field Support IdentityId
    SupportExhausted :: Field Support Bool
    SupportUses :: Field Support Natural
    SupportDiscardIfNoUses :: Field Support Bool
    SupportHeldCards :: Field Support [PlayerCard]
  field fld s =
    let SupportAttrs {..} = toAttrs s
     in case fld of
          SupportId -> supportId
          SupportCardDef -> supportCardDef
          SupportController -> supportController
          SupportExhausted -> supportExhausted
          SupportUses -> supportUses
          SupportDiscardIfNoUses -> supportDiscardIfNoUses
          SupportHeldCards -> supportHeldCards
  toId = supportId . toAttrs
  toAttrs (Support a) = toSupportAttrs a

instance FromJSON (Attrs Support) where
  parseJSON = withObject "SupportAttrs" $ \o ->
    genericParseJSON defaultOptions $ Object $
      if KeyMap.member "supportCanPlayHeldEvents" o then o
      else KeyMap.insert "supportCanPlayHeldEvents" (Bool False) o

instance RunMessage Support where
  runMessage msg (Support a) = Support <$> runMessage msg a

instance Exhaustable Support where
  isExhausted = supportExhausted . toAttrs

instance IsRef Support where
  toRef = SupportRef . toId

instance HasAbilities Support where
  getAbilities (Support a) = getAbilities a

instance HasModifiersFor Support where
  getModifiersFor source target (Support a) = getModifiersFor source target a

getSupportController :: Support -> IdentityId
getSupportController = supportController . toAttrs

getSupportUses :: Support -> Natural
getSupportUses = supportUses . toAttrs

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, HasModifiersFor a, HasAbilities a, RunMessage a) => IsSupport a where
  toSupportAttrs :: a -> Attrs Support
  default toSupportAttrs :: (Coercible a (Attrs Support)) => a -> Attrs Support
  toSupportAttrs = coerce

type SupportCard a = CardBuilder (IdentityId, SupportId) a

exhaustedL :: Lens' (Attrs Support) Bool
exhaustedL = lens supportExhausted $ \m x -> m {supportExhausted = x}

usesL :: Lens' (Attrs Support) Natural
usesL = lens supportUses $ \m x -> m {supportUses = x}

heldCardsL :: Lens' (Attrs Support) [PlayerCard]
heldCardsL = lens supportHeldCards $ \m x -> m {supportHeldCards = x}

discardIfNoUsesL :: Lens' (Attrs Support) Bool
discardIfNoUsesL =
  lens supportDiscardIfNoUses $ \m x -> m {supportDiscardIfNoUses = x}

instance HasCardCode (Attrs Support) where
  toCardCode = toCardCode . supportCardDef

instance IsCard (Attrs Support) where
  toCard a =
    PlayerCard $
      MkPlayerCard
        { pcCardId = CardId . unSupportId $ supportId a
        , pcCardDef = supportCardDef a
        , pcOwner = Just $ supportOwner a
        , pcController = Just $ supportController a
        }

supportWith ::
  (Attrs Support -> a) ->
  CardDef ->
  (Attrs Support -> Attrs Support) ->
  CardBuilder (IdentityId, SupportId) a
supportWith f cardDef g = support (f . g) cardDef

support ::
  (Attrs Support -> a) -> CardDef -> CardBuilder (IdentityId, SupportId) a
support f cardDef =
  CardBuilder
    { cbCardDef = cardDef
    , cbCardBuilder = \cardDef (ident, mid) ->
        f $
          SupportAttrs
            { supportId = mid
            , supportCardDef = cardDef
            , supportController = ident
            , supportOwner = ident
            , supportExhausted = False
            , supportUses = 0
            , supportDiscardIfNoUses = False
            , supportHeldCards = []
            , supportCanPlayHeldEvents = False
            , supportThreat = 0
            }
    }

instance IsRef (Attrs Support) where
  toRef = SupportRef . supportId

instance RunMessage (Attrs Support) where
  runMessage msg a = runQueueT $ case msg of
    CardSpecific (isTarget a -> True) "holdFacedownCard" (fromJSON -> Success card) ->
      pure a {supportCanPlayHeldEvents = False, supportHeldCards = supportHeldCards a <> [card]}
    CardSpecific (isTarget a -> True) "releaseHeldCard" (fromJSON -> Success card) ->
      pure a {supportHeldCards = filter (/= card) (supportHeldCards a)}
    RemovePlayerCardFromHeldZones cid -> pure a {supportHeldCards = filter ((/= cid) . pcCardId) (supportHeldCards a)}
    PassFirstPlayer -> do
      follows <- elem ControlledByFirstPlayer <$> getModifiers a
      when follows $ cardSpecific a "followFirstPlayer" ()
      pure a
    SetPlayerOrder _ -> do
      follows <- elem ControlledByFirstPlayer <$> getModifiers a
      when follows $ cardSpecific a "followFirstPlayer" ()
      pure a
    RemoveFromPlay (IdentityRef _) -> do
      follows <- elem ControlledByFirstPlayer <$> getModifiers a
      when follows $ cardSpecific a "followFirstPlayer" ()
      pure a
    CardSpecific (isTarget a -> True) "followFirstPlayer" _ -> do
      first <- selectOne FirstPlayer
      for_ first $ \new -> when (new /= supportController a) $ supportStep (supportId a) (SupportChangedController new)
      pure a
    RemoveCounters target n | target == toRef a -> do
      when (supportUses a > 0 && n >= supportUses a && supportDiscardIfNoUses a) $ removeFromPlay a
      pure $ a & usesL %~ subtractNatural n
    SupportMessage ident msg' | ident == supportId a -> case msg' of
      AttachPlayableEvent card -> pure a
        { supportHeldCards = supportHeldCards a <> [card | card `notElem` supportHeldCards a]
        , supportCanPlayHeldEvents = True
        }
      ReserveHeldCard card -> pure a {supportHeldCards = filter (/= card) (supportHeldCards a)}
      ReadiedSupport -> do
        readies <- notElem CannotReady <$> getModifiers a
        pure $ a & exhaustedL %~ (&& not readies)
      ExhaustedSupport -> do
        pure $ a & exhaustedL .~ True
      SpendSupportUse -> do
        when (supportUses a == 1 && supportDiscardIfNoUses a) $ removeFromPlay a
        pure $ a & usesL -~ 1
      AddSupportUses n -> pure $ a & usesL +~ n
      SupportOwnedBy owner -> pure a {supportOwner = owner}
      SupportChangedController new -> do
        when (new /= supportController a) $ do
          identityStep (supportController a) (SupportRemoved $ supportId a)
          identityStep new (SupportCreated $ supportId a)
        pure a {supportController = new}
    _ -> pure a
