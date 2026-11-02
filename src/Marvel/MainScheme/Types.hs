module Marvel.MainScheme.Types where

import Marvel.Prelude
import Data.Aeson.KeyMap qualified as KeyMap

import Data.Typeable
import Marvel.Ability.Types (Ability, HasAbilities (..))
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id hiding (MainSchemeId)
import Marvel.Id as X (MainSchemeId)
import Marvel.Matchers.Types ()
import Marvel.Message.Lifted
import Marvel.Query ()
import Marvel.Modifier
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
someMainSchemeCardCode = liftMainSchemeCard toCardCode

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
    , mainSchemeHeldEncounterCards :: [EncounterCard]
    , mainSchemeHeldCardsFacedown :: Bool
    , mainSchemeCounters :: Natural
    , mainSchemeHasGlider :: Bool
    , mainSchemeAccelerationTokens :: Natural
    , mainSchemeVillain :: Maybe Text
    -- ^ the title of the villain whose own scheme it is ("Proxima Midnight's Scheme"),
    -- who schemes onto it alone (Tower Defense)
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON)
  data Field MainScheme :: Type -> Type where
    MainSchemeId :: Field MainScheme MainSchemeId
    MainSchemeCardDef :: Field MainScheme CardDef
    MainSchemeThreat :: Field MainScheme Natural
    MainSchemeInitialThreat :: Field MainScheme GameValue
    MainSchemeAcceleration :: Field MainScheme GameValue
    MainSchemeThreshold :: Field MainScheme GameValue
    MainSchemeCrisis :: Field MainScheme Bool
    MainSchemeHeldCards :: Field MainScheme [PlayerCard]
    MainSchemeCounters :: Field MainScheme Natural
    MainSchemeHasGlider :: Field MainScheme Bool
    MainSchemeAccelerationTokens :: Field MainScheme Natural
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
          MainSchemeCounters -> mainSchemeCounters
          MainSchemeHasGlider -> mainSchemeHasGlider
          MainSchemeAccelerationTokens -> mainSchemeAccelerationTokens
  toId = mainSchemeId . toAttrs
  toAttrs (MainScheme a) = toMainSchemeAttrs a

instance FromJSON (Attrs MainScheme) where
  parseJSON = withObject "MainSchemeAttrs" \o -> genericParseJSON defaultOptions
    (Object $ KeyMap.union o $ KeyMap.fromList
      [("mainSchemeHasGlider", Bool False), ("mainSchemeAccelerationTokens", Number 0), ("mainSchemeHeldEncounterCards", toJSON ([] :: [EncounterCard])), ("mainSchemeHeldCardsFacedown", Bool False)])

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
  mainSchemeModifiersFor :: (HasGame m) => Source -> Target -> a -> m [Modifier]
  mainSchemeModifiersFor _ _ _ = pure []
  -- | its abilities, as a "First Player Action" (The Galaxy's Most Wanted): anyone's
  mainSchemeAbilities :: a -> [Ability]
  mainSchemeAbilities _ = []

instance HasAbilities MainScheme where
  getAbilities (MainScheme a) = mainSchemeAbilities a

instance HasModifiersFor MainScheme where
  getModifiersFor source target (MainScheme a) = mainSchemeModifiersFor source target a

type MainSchemeCard a = CardBuilder MainSchemeId a

threatL :: Lens' (Attrs MainScheme) Natural
threatL = lens mainSchemeThreat $ \m x -> m {mainSchemeThreat = x}

countersL :: Lens' (Attrs MainScheme) Natural
countersL = lens mainSchemeCounters $ \m x -> m {mainSchemeCounters = x}

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
    { cbCardDef = cardDef
    , cbCardBuilder = \cardDef mid ->
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
            , mainSchemeHeldEncounterCards = []
            , mainSchemeHeldCardsFacedown = False
            , mainSchemeCounters = 0
            , mainSchemeHasGlider = False
            , mainSchemeAccelerationTokens = 0
            , mainSchemeVillain = Nothing
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
  runMessage msg attrs = runQueueT $ case msg of
    CardSpecific target "resolveThreatPlacement" (fromJSON -> Success n) | target == toRef attrs -> do
      threshold <- getThreshold attrs
      when (mainSchemeThreat attrs + n >= threshold) $ advanceMainScheme (mainSchemeId attrs)
      pure $ attrs & threatL +~ n
    CardSpecific target "moveThreatOut" (fromJSON -> Success n) | target == toRef attrs -> pure $ attrs & threatL %~ subtractNatural n
    CardSpecific target "moveThreatIn" (fromJSON -> Success n) | target == toRef attrs -> pure $ attrs & threatL +~ n
    CardSpecific target "mgHoldCard" (fromJSON -> Success card) | target == toRef attrs ->
      pure attrs {mainSchemeHeldCards = card : filter ((/= pcCardId card) . pcCardId) (mainSchemeHeldCards attrs)}
    CardSpecific target "holdEncounterCard" (fromJSON -> Success card) | target == toRef attrs ->
      pure attrs {mainSchemeHeldEncounterCards = card : filter ((/= ecCardId card) . ecCardId) (mainSchemeHeldEncounterCards attrs)}
    CardSpecific target "mgClearHeld" _ | target == toRef attrs ->
      pure attrs {mainSchemeHeldCards = [],mainSchemeHeldEncounterCards = []}
    CardSpecific target "mgRemoveHeld" (fromJSON -> Success cid) | target == toRef attrs ->
      pure attrs {mainSchemeHeldCards = filter ((/= cid) . pcCardId) (mainSchemeHeldCards attrs),mainSchemeHeldEncounterCards = filter ((/= cid) . ecCardId) (mainSchemeHeldEncounterCards attrs)}
    RemovePlayerCardFromHeldZones cid -> pure attrs {mainSchemeHeldCards = filter ((/= cid) . pcCardId) (mainSchemeHeldCards attrs),mainSchemeHeldEncounterCards = filter ((/= cid) . ecCardId) (mainSchemeHeldEncounterCards attrs)}
    MainSchemeMessage ident msg' | ident == mainSchemeId attrs -> do
      locked <- elem ThreatCannotBeRemoved <$> getModifiers attrs
      case msg' of
        SetMainSchemeGlider value -> pure attrs {mainSchemeHasGlider = value}
        SetMainSchemeAcceleration value -> pure attrs {mainSchemeAccelerationTokens = value}
        AddMainSchemeAcceleration value -> pure attrs {mainSchemeAccelerationTokens = mainSchemeAccelerationTokens attrs + value}
        MainSchemePlaceInitialThreat -> do
          n <- fromIntegral <$> fromGameValue (mainSchemeInitialThreat attrs)
          pure $ attrs & threatL .~ n
        MainSchemePlaceThreat n -> do
          threshold <- getThreshold attrs
          when (mainSchemeThreat attrs + n >= threshold) $ checkWindows [W.Window W.Would $ W.MainSchemeCompleting ident]
          cardSpecific attrs "resolveThreatPlacement" n
          pure attrs
        -- a scheme threat cannot be removed from ("Take What Is Mine")
        MainSchemeBasicThwarted _ _ | locked -> pure attrs
        MainSchemeThwarted _ _ | locked -> pure attrs
        MainSchemeThreatRemoved _ _ | locked -> pure attrs
        -- a player a patrol minion is engaged with cannot thwart it with the cards they
        -- control (RR Patrol); removing threat otherwise is not a thwart
        MainSchemeBasicThwarted source n -> unlessPatrolled True source do
          interruptThwart source n
        MainSchemeThwarted source n -> unlessPatrolled False source do
          interruptThwart source n
        -- "player cards cannot remove threat from the main scheme" while a crisis icon is
        -- in play (RR Crisis Icon); a thwart is already kept off it
        MainSchemeThreatRemoved source n -> do
          allowed <- canRemoveThreatFrom source (SchemeMainSchemeId $ mainSchemeId attrs)
          crisis <- hasCrisisInYourArea
          let playerCard = case source of
                IdentityRef _ -> True
                AllyRef _ -> True
                EventRef _ -> True
                SupportRef _ -> True
                UpgradeRef _ -> True
                _ -> False
          ownIgnores <- elem IgnoresCrisis <$> getModifiers source
          controller <- getSourceController source
          controllerIgnores <- case source of
            AllyRef _ -> pure False
            _ -> maybe (pure False) (fmap (elem IgnoresCrisis) . getModifiers . IdentityRef) controller
          restricted <- isRestrictedMainScheme (mainSchemeId attrs)
          if not allowed || (crisis && restricted && playerCard && not (ownIgnores || controllerIgnores))
            then pure attrs else do
              heroRemovedLastThreat source (SchemeMainSchemeId ident) (mainSchemeThreat attrs) n
              pure $ attrs & threatL %~ subtractNatural n
        RevealMainScheme -> pure attrs
    CardSpecific target "resolveThwart" (fromJSON -> Success (source, n :: Natural)) | target == toRef attrs -> do
      let scheme = SchemeMainSchemeId $ mainSchemeId attrs
      thwartRemovesThreat W.After source scheme (mainSchemeThreat attrs) n
      heroThwartedAllThreat source scheme (mainSchemeThreat attrs) n
      heroRemovedLastThreat source scheme (mainSchemeThreat attrs) n
      pure $ attrs & threatL %~ subtractNatural n
    PlaceCounters target n | target == toRef attrs -> pure $ attrs & countersL +~ n
    RemoveCounters target n | target == toRef attrs -> pure $ attrs & countersL %~ subtractNatural n
    AccelerateMainScheme -> do
      -- the tokens, and the icons in its own area
      acceleration <- getAccelerationCountIn =<< getAreaOfTarget (mainSchemeId attrs)
      additionalThreat <- fromIntegral <$> fromGameValue (mainSchemeAcceleration attrs)
      gliders <- getGliderSchemes
      localTokens <- getMainSchemeAccelerationTokensIn =<< getAreaOfTarget (mainSchemeId attrs)
      let extra = if null gliders then acceleration
            else mainSchemeAccelerationTokens attrs + if mainSchemeHasGlider attrs then subtractNatural localTokens acceleration else 0
      placeThreatFrom W.ThreatFromAcceleration (mainSchemeId attrs) (additionalThreat + extra)
      pure attrs
    _ -> pure attrs
   where
    interruptThwart source n = do
      thwartRemovesThreat W.When source (SchemeMainSchemeId $ mainSchemeId attrs) (mainSchemeThreat attrs) n
      cardSpecific attrs "resolveThwart" (source, n)
      pure attrs
    unlessPatrolled basic source body = do
      allowed <- canRemoveThreatFrom source (SchemeMainSchemeId $ mainSchemeId attrs)
      patrolled <- maybe (pure False) isPatrolled =<< getSourceController source
      modifiers <- getModifiers source
      let ignores = IgnoresPatrol `elem` modifiers || (basic && BasicThwartIgnoresPatrol `elem` modifiers)
      restricted <- isRestrictedMainScheme (mainSchemeId attrs)
      if not allowed || (patrolled && restricted && not ignores) then pure attrs else body

-- | The threat that advances it: printed, and as modified (Under Surveillance).
getThreshold :: (HasGame m) => Attrs MainScheme -> m Natural
getThreshold attrs = do
  printed <- fromIntegral <$> fromGameValue (mainSchemeThreshold attrs)
  modifiers <- getModifiers attrs
  pure $ subtractNatural (sum [x | TargetThreatReduction x <- modifiers])
    (printed + sum [x | TargetThreatModifier x <- modifiers])
