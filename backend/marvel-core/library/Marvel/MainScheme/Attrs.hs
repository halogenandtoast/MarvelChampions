module Marvel.MainScheme.Attrs where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Message
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Window qualified as W

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, Entity a, EntityAttrs a ~ MainSchemeAttrs, EntityId a ~ MainSchemeId, RunMessage a) => IsMainScheme a

type MainSchemeCard a = CardBuilder MainSchemeId a

data MainSchemeAttrs = MainSchemeAttrs
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

threatL :: Lens' MainSchemeAttrs Natural
threatL = lens mainSchemeThreat $ \m x -> m { mainSchemeThreat = x }

instance HasCardCode MainSchemeAttrs where
  toCardCode = toCardCode . mainSchemeCardDef

mainSchemeWith ::
  (MainSchemeAttrs -> a) ->
  CardDef ->
  GameValue ->
  GameValue ->
  GameValue ->
  (MainSchemeAttrs -> MainSchemeAttrs) ->
  CardBuilder MainSchemeId a
mainSchemeWith f cardDef threshold initialThreat acceleration g =
  mainScheme (f . g) cardDef threshold initialThreat acceleration

mainScheme ::
  (MainSchemeAttrs -> a) ->
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

instance Entity MainSchemeAttrs where
  type EntityId MainSchemeAttrs = MainSchemeId
  type EntityAttrs MainSchemeAttrs = MainSchemeAttrs
  toId = mainSchemeId
  toAttrs = id

instance IsSource MainSchemeAttrs where
  toSource = MainSchemeSource . toId

instance IsTarget MainSchemeAttrs where
  toTarget = MainSchemeTarget . toId

instance HasCardDef MainSchemeAttrs where
  getCardDef = mainSchemeCardDef

instance IsCard MainSchemeAttrs where
  toCard a =
    EncounterCard $
      MkEncounterCard
        { ecCardId = CardId $ unMainSchemeId $ mainSchemeId a
        , ecCardDef = getCardDef a
        }

instance RunMessage MainSchemeAttrs where
  runMessage msg attrs = case msg of
    MainSchemeMessage mainSchemeId msg' | mainSchemeId == toId attrs ->
      case msg' of
        MainSchemePlaceInitialThreat -> do
          n <- fromIntegral <$> fromGameValue (mainSchemeInitialThreat attrs)
          pure $ attrs & threatL .~ n
        MainSchemePlaceThreat n -> do
          threshold <- fromIntegral <$> fromGameValue (mainSchemeThreshold attrs)
          when (mainSchemeThreat attrs + n >= threshold) (push AdvanceMainScheme)
          pure $ attrs & threatL +~ n
        MainSchemeThwarted _ n -> pure $ attrs & threatL %~ subtractNatural n
        RevealMainScheme -> pure attrs
    AccelerateMainScheme -> do
      acceleration <- getAccelerationCount
      additionalThreat <- fromIntegral <$> fromGameValue (mainSchemeAcceleration attrs)
      pushAll
        [ CheckWindows
            [ W.Window W.Would $
                W.ThreatPlaced
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
