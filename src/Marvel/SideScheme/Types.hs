module Marvel.SideScheme.Types where

import Marvel.Prelude

import Data.Aeson.KeyMap qualified as KeyMap
import Data.Typeable
import Marvel.Ability.Types (Ability, HasAbilities (..))
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Keyword (Keyword (Hinder))
import Marvel.Id hiding (SideSchemeId)
import Marvel.Id as X (SideSchemeId)
import Marvel.Matchers.Types (SideSchemeMatcher (SignatureSchemeOf), VillainMatcher (AnyVillain))
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query (match)
import Marvel.Ref
import Marvel.Window

data SideScheme = forall a. (IsSideScheme a) => SideScheme a

instance ToJSON SideScheme where
  toJSON (SideScheme a) = toJSON a

instance Show SideScheme where
  show (SideScheme a) = show a

instance Eq SideScheme where
  SideScheme (a :: a) == SideScheme (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

data SomeSideSchemeCard
  = forall a.
    (IsSideScheme a) =>
    SomeSideSchemeCard
      (SideSchemeCard a)

liftSideSchemeCard ::
  (forall a. SideSchemeCard a -> b) -> SomeSideSchemeCard -> b
liftSideSchemeCard f (SomeSideSchemeCard a) = f a

someSideSchemeCardCode :: SomeSideSchemeCard -> CardCode
someSideSchemeCardCode = liftSideSchemeCard toCardCode

instance Entity SideScheme where
  type Id SideScheme = SideSchemeId
  data Attrs SideScheme = SideSchemeAttrs
    { sideSchemeId :: SideSchemeId
    , sideSchemeCardDef :: CardDef
    , sideSchemeThreat :: Natural
    , sideSchemeInitialThreat :: GameValue
    , sideSchemeCrisis :: Bool
    , sideSchemeHeldCards :: [PlayerCard]
    , sideSchemeDefeatedBy :: Maybe IdentityId
    -- ^ the player whose thwarting removed its last threat: "the player who defeated it"
    , sideSchemeCounters :: Natural
    -- ^ counters on it (Defensive Protocols' crash counters)
    , sideSchemeAccelerationTokens :: Natural
    , sideSchemePlayerCard :: Maybe PlayerCard
    , sideSchemeExhausted :: Bool
    -- ^ acceleration tokens on this scheme; they leave play with it
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON)
  data Field SideScheme :: Type -> Type where
    SideSchemeId :: Field SideScheme SideSchemeId
    SideSchemeCardDef :: Field SideScheme CardDef
    SideSchemeThreat :: Field SideScheme Natural
    SideSchemeInitialThreat :: Field SideScheme GameValue
    SideSchemeCrisis :: Field SideScheme Bool
    SideSchemeHeldCards :: Field SideScheme [PlayerCard]
    SideSchemeCounters :: Field SideScheme Natural
    SideSchemeAccelerationTokens :: Field SideScheme Natural
  field fld s =
    let SideSchemeAttrs {..} = toAttrs s
     in case fld of
          SideSchemeId -> sideSchemeId
          SideSchemeCardDef -> sideSchemeCardDef
          SideSchemeThreat -> sideSchemeThreat
          SideSchemeInitialThreat -> sideSchemeInitialThreat
          SideSchemeCrisis -> sideSchemeCrisis
          SideSchemeHeldCards -> sideSchemeHeldCards
          SideSchemeCounters -> sideSchemeCounters
          SideSchemeAccelerationTokens -> sideSchemeAccelerationTokens
  toId = sideSchemeId . toAttrs
  toAttrs (SideScheme a) = toSideSchemeAttrs a

instance RunMessage SideScheme where
  runMessage msg (SideScheme a) = SideScheme <$> runMessage msg a

instance HasModifiersFor SideScheme where
  getBlankingModifiersFor source target (SideScheme a) = getBlankingModifiersFor source target a
  getModifiersFor source target (SideScheme a) =
    getModifiersFor source target a

instance IsRef SideScheme where
  toRef = SideSchemeRef . toId

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
  default toSideSchemeAttrs :: (Coercible a (Attrs SideScheme)) => a -> Attrs SideScheme
  toSideSchemeAttrs = coerce
  -- | its abilities, as a "First Player Action" (The Galaxy's Most Wanted): anyone's
  sideSchemeAbilities :: a -> [Ability]
  sideSchemeAbilities _ = []

instance HasAbilities SideScheme where
  getAbilities (SideScheme a) = sideSchemeAbilities a

type SideSchemeCard a = CardBuilder SideSchemeId a

threatL :: Lens' (Attrs SideScheme) Natural
threatL = lens sideSchemeThreat $ \m x -> m {sideSchemeThreat = x}

crisisL :: Lens' (Attrs SideScheme) Bool
crisisL = lens sideSchemeCrisis $ \m x -> m {sideSchemeCrisis = x}

heldCardsL :: Lens' (Attrs SideScheme) [PlayerCard]
heldCardsL = lens sideSchemeHeldCards $ \m x -> m {sideSchemeHeldCards = x}

instance HasCardCode (Attrs SideScheme) where
  toCardCode = toCardCode . sideSchemeCardDef

sideSchemeWith ::
  (Attrs SideScheme -> a) ->
  CardDef ->
  GameValue ->
  (Attrs SideScheme -> Attrs SideScheme) ->
  CardBuilder SideSchemeId a
sideSchemeWith f cardDef initialThreat g =
  sideScheme (f . g) cardDef initialThreat

sideScheme ::
  (Attrs SideScheme -> a) ->
  CardDef ->
  GameValue ->
  CardBuilder SideSchemeId a
sideScheme f cardDef initialThreat =
  CardBuilder
    { cbCardDef = cardDef
    , cbCardBuilder = \cardDef mid ->
        f $
          SideSchemeAttrs
            { sideSchemeId = mid
            , sideSchemeCardDef = cardDef
            , sideSchemeInitialThreat = initialThreat
            , sideSchemeThreat = 0
            , sideSchemeCrisis = False
            , sideSchemeHeldCards = []
            , sideSchemeDefeatedBy = Nothing
            , sideSchemeCounters = 0
            , sideSchemeAccelerationTokens = 0
            , sideSchemePlayerCard = Nothing
            , sideSchemeExhausted = False
            }
    }

-- | The player who defeated the side scheme: whose thwarting removed its last threat, or
-- else "you", for a scheme defeated some other way.
getDefeatedBy :: (HasGame m) => Attrs SideScheme -> m IdentityId
getDefeatedBy attrs = maybe getYou pure (sideSchemeDefeatedBy attrs)

instance IsRef (Attrs SideScheme) where
  toRef = SideSchemeRef . sideSchemeId

instance HasCardDef (Attrs SideScheme) where
  getCardDef = sideSchemeCardDef

instance IsCard (Attrs SideScheme) where
  toCard a =
    maybe (EncounterCard $
      MkEncounterCard
        { ecCardId = CardId $ unSideSchemeId $ sideSchemeId a
        , ecCardDef = getCardDef a
        }) PlayerCard (sideSchemePlayerCard a)

-- | A side scheme saved before it could hold counters has none.
instance FromJSON (Attrs SideScheme) where
  parseJSON = withObject "SideSchemeAttrs" \o ->
    genericParseJSON defaultOptions (Object (KeyMap.union o (KeyMap.fromList [("sideSchemeCounters", Number 0), ("sideSchemeAccelerationTokens", Number 0), ("sideSchemePlayerCard", Null), ("sideSchemeExhausted", Bool False)])))

countersL :: Lens' (Attrs SideScheme) Natural
countersL = lens sideSchemeCounters $ \m x -> m {sideSchemeCounters = x}

instance RunMessage (Attrs SideScheme) where
  runMessage msg attrs = runQueueT $ case msg of
    CardSpecific (isTarget attrs -> True) "removeAccelerationToken" _ -> pure attrs {sideSchemeAccelerationTokens = subtractNatural 1 $ sideSchemeAccelerationTokens attrs}
    CardSpecific target "playerSideSchemeCard" (fromJSON -> Success card) | target == toRef attrs -> pure attrs {sideSchemePlayerCard = Just card}
    CardSpecific target "moveThreatOut" (fromJSON -> Success n) | target == toRef attrs -> do
      let remaining = subtractNatural n (sideSchemeThreat attrs)
      when (sideSchemeThreat attrs > 0 && remaining == 0) $ defeatSideScheme (sideSchemeId attrs)
      pure $ attrs & threatL .~ remaining
    CardSpecific target "moveThreatIn" (fromJSON -> Success n) | target == toRef attrs -> pure $ attrs & threatL +~ n
    RemovePlayerCardFromHeldZones cid -> pure attrs {sideSchemeHeldCards = filter ((/= cid) . pcCardId) (sideSchemeHeldCards attrs)}
    CardSpecific target "resolveThwart" (fromJSON -> Success (source, n :: Natural)) | target == toRef attrs -> do
      let scheme = SchemeSideSchemeId $ sideSchemeId attrs
      thwartRemovesThreat After source scheme (sideSchemeThreat attrs) n
      attrs' <- removed True source n
      heroThwartedAllThreat source scheme (sideSchemeThreat attrs) n
      pure attrs'
    PlaceCounters target n | target == toRef attrs -> pure $ attrs & countersL +~ n
    RemoveCounters target n | target == toRef attrs -> pure $ attrs & countersL %~ subtractNatural n
    SideSchemeMessage ident msg' | ident == sideSchemeId attrs -> do
      modifiers <- getModifiers attrs
      case msg' of
        ExhaustedSideScheme -> pure attrs {sideSchemeExhausted = True}
        ReadiedSideScheme -> do
          canReady <- notElem CannotReady <$> getModifiers attrs
          pure attrs {sideSchemeExhausted = sideSchemeExhausted attrs && not canReady}
        SideSchemePlaceInitialThreat -> do
          n <- fromIntegral <$> fromGameValue (sideSchemeInitialThreat attrs)
          hinder <- sum <$> traverse fromGameValue [x | Hinder x <- toList $ cdKeywords $ getCardDef attrs]
          pure $ attrs & threatL .~ n + fromIntegral hinder
        SideSchemePlaceThreat n -> pure $ attrs & threatL +~ n
        SideSchemePlaceAccelerationToken -> pure attrs {sideSchemeAccelerationTokens = sideSchemeAccelerationTokens attrs + 1}
        SideSchemeThwarted _ _ | any (`elem` modifiers) [ThreatCannotBeRemoved, ThreatCannotBeRemovedByThwarting] -> pure attrs
        SideSchemeThreatRemoved _ _ | ThreatCannotBeRemoved `elem` modifiers -> pure attrs
        SideSchemeThwarted source n -> unlessBarred source do
          controller <- getSourceController source
          forbidden <- maybe (pure False) (fmap (elem CannotThwartSideSchemes) . getModifiers . IdentityRef) controller
          if forbidden then pure attrs else do
            thwartRemovesThreat When source (SchemeSideSchemeId ident) (sideSchemeThreat attrs) n
            cardSpecific attrs "resolveThwart" (source, n)
            pure attrs
        -- not thwarting, so "threat cannot be removed by thwarting" does not stop it
        SideSchemeThreatRemoved source n -> unlessBarred source (removed False source n)
        _ -> pure attrs
    _ -> pure attrs
   where
    -- a player's card cannot remove threat it is kept to another player's ('ThreatRemovableOnlyBy')
    unlessBarred source body = do
      onlyBy <- (\ms -> [p | ThreatRemovableOnlyBy p <- ms]) <$> getModifiers attrs
      controller <- getSourceController source
      allowed <- canRemoveThreatFrom source (SchemeSideSchemeId $ sideSchemeId attrs)
      if not allowed || any (\p -> maybe False (/= p) controller) onlyBy then pure attrs else body
    removed thwart source n = do
          heroRemovedLastThreat source (SchemeSideSchemeId $ sideSchemeId attrs) (sideSchemeThreat attrs) n
          modifiers <- getModifiers attrs
          -- a villain's own side scheme is not discarded with no threat on it
          signature <- match (sideSchemeId attrs) (SignatureSchemeOf AnyVillain)
          let defeated = subtractNatural n (sideSchemeThreat attrs) == 0 && not signature && CannotLeavePlay `notElem` modifiers
          when defeated $ if thwart then defeatSideSchemeByThwart (sideSchemeId attrs) source else defeatSideSchemeBy (sideSchemeId attrs) source
          when (defeated && thwart && n == sideSchemeThreat attrs && n > 0) $ checkWindows [Window After $ ExactlyDefeatedSideSchemeBy (sideSchemeId attrs) source]
          defeater <- if defeated then getSourceController source else pure Nothing
          pure $ attrs & threatL %~ subtractNatural n & \a -> a {sideSchemeDefeatedBy = defeater <|> sideSchemeDefeatedBy a}
