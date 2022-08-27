module Marvel.SideScheme.Types where

import Marvel.Prelude

import Data.Typeable
import Marvel.Card
import Marvel.Entity
import Marvel.GameValue
import Marvel.Id hiding (SideSchemeId)
import Marvel.Id as X (SideSchemeId)
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
  type Id SideScheme = SideSchemeId
  data Attrs SideScheme = SideSchemeAttrs
    { sideSchemeId :: SideSchemeId
    , sideSchemeCardDef :: CardDef
    , sideSchemeThreat :: Natural
    , sideSchemeInitialThreat :: GameValue
    , sideSchemeCrisis :: Bool
    , sideSchemeHeldCards :: [PlayerCard]
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
  data Field SideScheme :: Type -> Type where
    SideSchemeId :: Field SideScheme SideSchemeId
    SideSchemeCardDef :: Field SideScheme CardDef
    SideSchemeThreat :: Field SideScheme Natural
    SideSchemeInitialThreat :: Field SideScheme GameValue
    SideSchemeCrisis :: Field SideScheme Bool
    SideSchemeHeldCards :: Field SideScheme [PlayerCard]
  field fld s = let SideSchemeAttrs {..} = toAttrs s in case fld of
    SideSchemeId -> sideSchemeId
    SideSchemeCardDef -> sideSchemeCardDef
    SideSchemeThreat -> sideSchemeThreat
    SideSchemeInitialThreat -> sideSchemeInitialThreat
    SideSchemeCrisis -> sideSchemeCrisis
    SideSchemeHeldCards -> sideSchemeHeldCards
  toId = sideSchemeId . toAttrs
  toAttrs (SideScheme a) = toSideSchemeAttrs a

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

class (HasModifiersFor a, Show a, Eq a, ToJSON a, FromJSON a, Typeable a, RunMessage a) => IsSideScheme a where
  toSideSchemeAttrs :: a -> Attrs SideScheme
  default toSideSchemeAttrs :: Coercible a (Attrs SideScheme) => a -> Attrs SideScheme
  toSideSchemeAttrs = coerce

type SideSchemeCard a = CardBuilder SideSchemeId a

threatL :: Lens' (Attrs SideScheme) Natural
threatL = lens sideSchemeThreat $ \m x -> m { sideSchemeThreat = x }

crisisL :: Lens' (Attrs SideScheme) Bool
crisisL = lens sideSchemeCrisis $ \m x -> m { sideSchemeCrisis = x }

heldCardsL :: Lens' (Attrs SideScheme) [PlayerCard]
heldCardsL = lens sideSchemeHeldCards $ \m x -> m { sideSchemeHeldCards = x }

instance HasCardCode (Attrs SideScheme) where
  toCardCode = toCardCode . sideSchemeCardDef

sideSchemeWith
  :: (Attrs SideScheme -> a)
  -> CardDef
  -> GameValue
  -> (Attrs SideScheme -> Attrs SideScheme)
  -> CardBuilder SideSchemeId a
sideSchemeWith f cardDef initialThreat g =
  sideScheme (f . g) cardDef initialThreat

sideScheme
  :: (Attrs SideScheme -> a)
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

instance IsSource (Attrs SideScheme) where
  toSource = SideSchemeSource . sideSchemeId

instance IsTarget (Attrs SideScheme) where
  toTarget = SideSchemeTarget . sideSchemeId

instance HasCardDef (Attrs SideScheme) where
  getCardDef = sideSchemeCardDef

instance IsCard (Attrs SideScheme) where
  toCard a = EncounterCard $ MkEncounterCard
    { ecCardId = CardId $ unSideSchemeId $ sideSchemeId a
    , ecCardDef = getCardDef a
    }

instance RunMessage (Attrs SideScheme) where
  runMessage msg attrs = case msg of
    SideSchemeMessage ident msg' | ident == sideSchemeId attrs ->
      case msg' of
        SideSchemePlaceInitialThreat -> do
          n <- fromIntegral <$> fromGameValue (sideSchemeInitialThreat attrs)
          pure $ attrs & threatL .~ n
        SideSchemePlaceThreat n -> pure $ attrs & threatL +~ n
        SideSchemeThwarted _ n -> do
          when
            (subtractNatural n (sideSchemeThreat attrs) == 0)
            (pushAll
              [ CheckWindows [Window When $ DefeatedSideScheme (sideSchemeId attrs)]
              , SideSchemeMessage (sideSchemeId attrs) DefeatSideScheme
              , RemoveFromPlay (toTarget attrs)
              ]
            )
          pure $ attrs & threatL %~ subtractNatural n
        _ -> pure attrs
    _ -> pure attrs
