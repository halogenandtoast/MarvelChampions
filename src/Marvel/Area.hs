-- | Game areas: where players play against their own villains and schemes, as The Once
-- and Future Kang splits them. Every other scenario is one area holding everything.
--
-- An area lists what is in it. What is listed nowhere is in the first area, so a card put
-- into play by some other way than the engine's own is never lost; a minion is where the
-- player it is engaged with is, and a player's cards where that player is.
module Marvel.Area where

import Marvel.Prelude

import Marvel.Card.EncounterCard (EncounterCard)
import Marvel.Id

newtype AreaId = AreaId Int
  deriving newtype (Show, Eq, Ord, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

-- | The area every game starts with.
defaultAreaId :: AreaId
defaultAreaId = AreaId 0

-- | An encounter deck, or a deck kept like one: the encounter deck, a villain's own (The
-- Wrecking Crew), a named deck set next to the main scheme (Experimental Weapons), or
-- the side-scheme deck (The Rise of Red Skull).
data DeckKey
  = EncounterDeckKey
  | VillainDeckKey VillainId
  | NamedDeckKey Text
  | SideSchemeDeckKey
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

-- | The Experimental Weapons deck, set next to the main scheme by Attack on Mount Athena.
experimentalWeaponsDeck :: DeckKey
experimentalWeaponsDeck = NamedDeckKey "experimentalWeapons"

-- | Running out of an encounter deck, the encounter deck or a villain's, costs an
-- acceleration token as its discard pile is shuffled back; the side-scheme deck's is
-- shuffled back without penalty, and a named deck is not shuffled back at all.
isEncounterDeck :: DeckKey -> Bool
isEncounterDeck = \case
  EncounterDeckKey -> True
  VillainDeckKey _ -> True
  _ -> False

-- | Where a card of the deck goes when it is discarded: a card from the Experimental
-- Weapons deck is part of the encounter deck once it is in play.
discardsTo :: DeckKey -> DeckKey
discardsTo = \case
  NamedDeckKey _ -> EncounterDeckKey
  k -> k

-- | A deck, its top card first, and its discard pile, its top card first.
data Pile = Pile {pileDeck :: [EncounterCard], pileDiscard :: [EncounterCard]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

emptyPile :: Pile
emptyPile = Pile [] []

data Area = Area
  { areaId :: AreaId
  , areaPlayers :: [IdentityId]
  , areaVillains :: [VillainId]
  , areaActiveVillain :: Maybe VillainId
  -- ^ the villain that activates and that "the villain" means here
  , areaMainSchemes :: [MainSchemeId]
  , areaSideSchemes :: [SideSchemeId]
  , areaEnvironments :: [EnvironmentId]
  , areaAccelerationTokens :: Natural
  -- ^ next to its main scheme: they stay as the main scheme leaves play (RR,
  -- Acceleration Token)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newArea :: AreaId -> [IdentityId] -> Area
newArea aid players = Area aid players [] Nothing [] [] [] 0

-- | The area listing it, or else the first.
areaListing :: (Area -> Bool) -> [Area] -> Maybe Area
areaListing listed areas = find listed areas <|> headMay areas
 where
  find p = headMay . filter p

-- | Each player leaves where they were, for the area given.
movePlayers :: [IdentityId] -> AreaId -> [Area] -> [Area]
movePlayers players aid = map \a ->
  let rest = filter (`notElem` players) (areaPlayers a)
   in a {areaPlayers = if areaId a == aid then rest <> players else rest}

-- | What is in the first area moves to the second, which keeps its own active villain
-- (or takes the first's, having none); the first area is gone.
combine :: AreaId -> AreaId -> [Area] -> [Area]
combine from into areas = case filter ((== from) . areaId) areas of
  [] -> areas
  src : _ | from /= into -> map (merge src) (filter ((/= from) . areaId) areas)
  _ -> areas
 where
  merge src a
    | areaId a /= into = a
    | otherwise =
        a
          { areaPlayers = areaPlayers a <> areaPlayers src
          , areaVillains = areaVillains a <> areaVillains src
          , areaActiveVillain = areaActiveVillain a <|> areaActiveVillain src
          , areaMainSchemes = areaMainSchemes a <> areaMainSchemes src
          , areaSideSchemes = areaSideSchemes a <> areaSideSchemes src
          , areaEnvironments = areaEnvironments a <> areaEnvironments src
          , areaAccelerationTokens = areaAccelerationTokens a + areaAccelerationTokens src
          }
