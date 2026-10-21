module Marvel.MainScheme.Types where

import Marvel.Prelude

import Data.Typeable
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id hiding (MainSchemeId)
import Marvel.Id as X (MainSchemeId)
import Marvel.Message
import Marvel.Queue
import Marvel.Ref
import Marvel.Window qualified as W

data MainScheme = forall a. (IsMainScheme a) => MainScheme a

instance Show MainScheme where
  show (MainScheme a) = show a

instance ToJSON MainScheme where
  toJSON (MainScheme a) = toJSON a

instance Eq MainScheme where
  MainScheme (a :: a) == MainScheme (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

data SomeMainSchemeCard
  = forall a.
    (IsMainScheme a) =>
    SomeMainSchemeCard
      (MainSchemeCard a)

liftMainSchemeCard ::
  (forall a. MainSchemeCard a -> b) -> SomeMainSchemeCard -> b
liftMainSchemeCard f (SomeMainSchemeCard a) = f a

someMainSchemeCardCode :: SomeMainSchemeCard -> CardCode
someMainSchemeCardCode = liftMainSchemeCard cbCardCode

instance Entity MainScheme where
  type Id MainScheme = MainSchemeId
  data Attrs MainScheme = MainSchemeAttrs
    { mainSchemeId :: MainSchemeId
    , mainSchemeCardDef :: CardDef
    , mainSchemeThreat :: Natural
    , mainSchemeInitialThreat :: GameValue
    , mainSchemeAcceleration :: GameValue
    , mainSchemeThreshold :: GameValue
    , mainSchemeCrisis :: Bool
    , mainSchemeHeldCards :: [PlayerCard]
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
  data Field MainScheme :: Type -> Type where
    MainSchemeId :: Field MainScheme MainSchemeId
    MainSchemeCardDef :: Field MainScheme CardDef
    MainSchemeThreat :: Field MainScheme Natural
    MainSchemeInitialThreat :: Field MainScheme GameValue
    MainSchemeAcceleration :: Field MainScheme GameValue
    MainSchemeThreshold :: Field MainScheme GameValue
    MainSchemeCrisis :: Field MainScheme Bool
    MainSchemeHeldCards :: Field MainScheme [PlayerCard]
  field fld m =
    let MainSchemeAttrs {..} = toAttrs m
     in case fld of
          MainSchemeId -> mainSchemeId
          MainSchemeCardDef -> mainSchemeCardDef
          MainSchemeThreat -> mainSchemeThreat
          MainSchemeInitialThreat -> mainSchemeInitialThreat
          MainSchemeAcceleration -> mainSchemeAcceleration
          MainSchemeThreshold -> mainSchemeThreshold
          MainSchemeCrisis -> mainSchemeCrisis
          MainSchemeHeldCards -> mainSchemeHeldCards
  toId = mainSchemeId . toAttrs
  toAttrs (MainScheme a) = toMainSchemeAttrs a

instance RunMessage MainScheme where
  runMessage msg (MainScheme a) = MainScheme <$> runMessage msg a

instance IsRef MainScheme where
  toRef = MainSchemeRef . toId

instance IsCard MainScheme where
  toCard = toCard . toAttrs

instance HasCardDef MainScheme where
  getCardDef = getCardDef . toAttrs

getMainSchemeThreat :: MainScheme -> Natural
getMainSchemeThreat = mainSchemeThreat . toAttrs

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, RunMessage a) => IsMainScheme a where
  toMainSchemeAttrs :: a -> Attrs MainScheme
  default toMainSchemeAttrs :: (Coercible a (Attrs MainScheme)) => a -> Attrs MainScheme
  toMainSchemeAttrs = coerce

type MainSchemeCard a = CardBuilder MainSchemeId a

threatL :: Lens' (Attrs MainScheme) Natural
threatL = lens mainSchemeThreat $ \m x -> m {mainSchemeThreat = x}

instance HasCardCode (Attrs MainScheme) where
  toCardCode = toCardCode . mainSchemeCardDef

mainSchemeWith ::
  (Attrs MainScheme -> a) ->
  CardDef ->
  GameValue ->
  GameValue ->
  GameValue ->
  (Attrs MainScheme -> Attrs MainScheme) ->
  CardBuilder MainSchemeId a
mainSchemeWith f cardDef threshold initialThreat acceleration g =
  mainScheme (f . g) cardDef threshold initialThreat acceleration

mainScheme ::
  (Attrs MainScheme -> a) ->
  CardDef ->
  GameValue ->
  GameValue ->
  GameValue ->
  CardBuilder MainSchemeId a
mainScheme f cardDef threshold initialThreat acceleration =
  CardBuilder
    { cbCardCode = cdCardCode cardDef
    , cbCardBuilder = \mid ->
        f $
          MainSchemeAttrs
            { mainSchemeId = mid
            , mainSchemeCardDef = cardDef
            , mainSchemeThreshold = threshold
            , mainSchemeInitialThreat = initialThreat
            , mainSchemeAcceleration = acceleration
            , mainSchemeThreat = 0
            , mainSchemeCrisis = False
            , mainSchemeHeldCards = []
            }
    }

instance IsRef (Attrs MainScheme) where
  toRef = MainSchemeRef . mainSchemeId

instance HasCardDef (Attrs MainScheme) where
  getCardDef = mainSchemeCardDef

instance IsCard (Attrs MainScheme) where
  toCard a =
    EncounterCard $
      MkEncounterCard
        { ecCardId = CardId $ unMainSchemeId $ mainSchemeId a
        , ecCardDef = getCardDef a
        }

instance RunMessage (Attrs MainScheme) where
  runMessage msg attrs = case msg of
    MainSchemeMessage ident msg' | ident == mainSchemeId attrs ->
      case msg' of
        MainSchemePlaceInitialThreat -> do
          n <- fromIntegral <$> fromGameValue (mainSchemeInitialThreat attrs)
          pure $ attrs & threatL .~ n
        MainSchemePlaceThreat n -> do
          threshold <-
            fromIntegral
              <$> fromGameValue (mainSchemeThreshold attrs)
          when
            (mainSchemeThreat attrs + n >= threshold)
            (push AdvanceMainScheme)
          pure $ attrs & threatL +~ n
        MainSchemeThwarted _ n -> pure $ attrs & threatL %~ subtractNatural n
        RevealMainScheme -> pure attrs
    AccelerateMainScheme -> do
      acceleration <- getAccelerationCount
      additionalThreat <-
        fromIntegral
          <$> fromGameValue (mainSchemeAcceleration attrs)
      pushAll
        [ CheckWindows
            [ W.Window W.Would
                $ W.ThreatPlaced
                  W.ThreatFromAcceleration
                  (SchemeMainSchemeId $ mainSchemeId attrs)
                $ additionalThreat
                  + acceleration
            ]
        , MainSchemeMessage
            (mainSchemeId attrs)
            (MainSchemePlaceThreat $ additionalThreat + acceleration)
        ]
      pure attrs
    _ -> pure attrs
