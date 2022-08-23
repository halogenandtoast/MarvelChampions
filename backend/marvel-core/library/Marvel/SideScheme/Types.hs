module Marvel.SideScheme.Types where

import Marvel.Prelude

import Data.Typeable
import Marvel.Card
import Marvel.Entity
import Marvel.GameValue
import Marvel.Id
import Marvel.Message
import Marvel.Modifier
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Window
import Text.Show qualified

data SideScheme = forall a . IsSideScheme a => SideScheme a

instance ToJSON SideScheme where
  toJSON (SideScheme a) = toJSON a

instance Show SideScheme where
  show (SideScheme a) = show a

instance Eq SideScheme where
  SideScheme (a :: a) == SideScheme (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

data SomeSideSchemeCard = forall a . IsSideScheme a => SomeSideSchemeCard
  (SideSchemeCard a)

liftSideSchemeCard
  :: (forall a . SideSchemeCard a -> b) -> SomeSideSchemeCard -> b
liftSideSchemeCard f (SomeSideSchemeCard a) = f a

someSideSchemeCardCode :: SomeSideSchemeCard -> CardCode
someSideSchemeCardCode = liftSideSchemeCard cbCardCode

instance Entity SideScheme where
  type EntityId SideScheme = SideSchemeId
  type EntityAttrs SideScheme = SideSchemeAttrs
  toId = toId . toAttrs
  toAttrs (SideScheme a) = toAttrs a

instance RunMessage SideScheme where
  runMessage msg (SideScheme a) = SideScheme <$> runMessage msg a

instance HasModifiersFor SideScheme where
  getModifiersFor source target (SideScheme a) =
    getModifiersFor source target a

instance IsSource SideScheme where
  toSource = SideSchemeSource . toId

instance IsTarget SideScheme where
  toTarget = SideSchemeTarget . toId

instance IsCard SideScheme where
  toCard = toCard . toAttrs

instance HasCardDef SideScheme where
  getCardDef = getCardDef . toAttrs

isCrisis :: SideScheme -> Bool
isCrisis = sideSchemeCrisis . toAttrs

getSideSchemeThreat :: SideScheme -> Natural
getSideSchemeThreat = sideSchemeThreat . toAttrs

class (HasModifiersFor a, Show a, Eq a, ToJSON a, FromJSON a, Typeable a, RunMessage a, Entity a, EntityAttrs a ~ SideSchemeAttrs, EntityId a ~ SideSchemeId) => IsSideScheme a

type SideSchemeCard a = CardBuilder SideSchemeId a

data SideSchemeAttrs = SideSchemeAttrs
  { sideSchemeId :: SideSchemeId
  , sideSchemeCardDef :: CardDef
  , sideSchemeThreat :: Natural
  , sideSchemeInitialThreat :: GameValue
  , sideSchemeCrisis :: Bool
  , sideSchemeHeldCards :: [PlayerCard]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

threatL :: Lens' SideSchemeAttrs Natural
threatL = lens sideSchemeThreat $ \m x -> m { sideSchemeThreat = x }

crisisL :: Lens' SideSchemeAttrs Bool
crisisL = lens sideSchemeCrisis $ \m x -> m { sideSchemeCrisis = x }

heldCardsL :: Lens' SideSchemeAttrs [PlayerCard]
heldCardsL = lens sideSchemeHeldCards $ \m x -> m { sideSchemeHeldCards = x }

instance HasCardCode SideSchemeAttrs where
  toCardCode = toCardCode . sideSchemeCardDef

sideSchemeWith
  :: (SideSchemeAttrs -> a)
  -> CardDef
  -> GameValue
  -> (SideSchemeAttrs -> SideSchemeAttrs)
  -> CardBuilder SideSchemeId a
sideSchemeWith f cardDef initialThreat g =
  sideScheme (f . g) cardDef initialThreat

sideScheme
  :: (SideSchemeAttrs -> a)
  -> CardDef
  -> GameValue
  -> CardBuilder SideSchemeId a
sideScheme f cardDef initialThreat = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \mid -> f $ SideSchemeAttrs
    { sideSchemeId = mid
    , sideSchemeCardDef = cardDef
    , sideSchemeInitialThreat = initialThreat
    , sideSchemeThreat = 0
    , sideSchemeCrisis = False
    , sideSchemeHeldCards = []
    }
  }

instance Entity SideSchemeAttrs where
  type EntityId SideSchemeAttrs = SideSchemeId
  type EntityAttrs SideSchemeAttrs = SideSchemeAttrs
  toId = sideSchemeId
  toAttrs = id

instance IsSource SideSchemeAttrs where
  toSource = SideSchemeSource . toId

instance IsTarget SideSchemeAttrs where
  toTarget = SideSchemeTarget . toId

instance HasCardDef SideSchemeAttrs where
  getCardDef = sideSchemeCardDef

instance IsCard SideSchemeAttrs where
  toCard a = EncounterCard $ MkEncounterCard
    { ecCardId = CardId $ unSideSchemeId $ sideSchemeId a
    , ecCardDef = getCardDef a
    }

instance RunMessage SideSchemeAttrs where
  runMessage msg attrs = case msg of
    SideSchemeMessage sideSchemeId msg' | sideSchemeId == toId attrs ->
      case msg' of
        SideSchemePlaceInitialThreat -> do
          n <- fromIntegral <$> fromGameValue (sideSchemeInitialThreat attrs)
          pure $ attrs & threatL .~ n
        SideSchemePlaceThreat n -> pure $ attrs & threatL +~ n
        SideSchemeThwarted _ n -> do
          when
            (subtractNatural n (sideSchemeThreat attrs) == 0)
            (pushAll
              [ CheckWindows [Window When $ DefeatedSideScheme (toId attrs)]
              , SideSchemeMessage (toId attrs) DefeatSideScheme
              , RemoveFromPlay (toTarget attrs)
              ]
            )
          pure $ attrs & threatL %~ subtractNatural n
        _ -> pure attrs
    _ -> pure attrs
