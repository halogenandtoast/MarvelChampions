module Marvel.MainScheme.MainSchemes.TheArtOfEvasion (
  theArtOfEvasion,
  TheArtOfEvasion (..),
  warpDriveInitiated,
  WarpDriveInitiated (..),
  accelerateByEvasion,
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.EncounterCard
import Marvel.Entity
import Marvel.Environment.Cards qualified as Environments
import Marvel.Environment.Environments.NebulasShip (getEvasionCounters, placeEvasionCounters)
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Trait

-- | "+X[per player]" acceleration, X the evasion counters on Nebula's Ship, counted as
-- the scheme accelerates.
accelerateByEvasion :: (ReverseQueue m) => Message -> Attrs MainScheme -> m (Attrs MainScheme)
accelerateByEvasion msg attrs = case msg of
  AccelerateMainScheme -> do
    x <- getEvasionCounters
    _ <- runMessage msg attrs {mainSchemeAcceleration = PerPlayer (fromIntegral x)}
    pure attrs
  _ -> runMessage msg attrs

theArtOfEvasion :: MainSchemeCard TheArtOfEvasion
theArtOfEvasion = mainScheme TheArtOfEvasion Cards.theArtOfEvasion (PerPlayer 6) (PerPlayer 2) (PerPlayer 0)

newtype TheArtOfEvasion = TheArtOfEvasion (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | 1A Setup: put Nebula's Ship and the Milano into play, attach the Power Stone to
-- Nebula (the Milano and the Power Stone are there already, by their setup keyword),
-- then discard the top 2[per player] encounter cards and attach each Technique
-- attachment discarded to Nebula.
instance RunMessage TheArtOfEvasion where
  runMessage msg ms@(TheArtOfEvasion attrs) = runQueueT $ case msg of
    SetupMainScheme -> do
      firstPlayer <- selectJust FirstPlayer
      ship <- genEncounterCard Environments.nebulasShip
      putEncounterCardIntoPlay firstPlayer ship
      n <- fromIntegral <$> fromGameValue (PerPlayer 2)
      villain <- selectOne ActiveVillain
      discardTopOfEncounterDeckThen n \cards ->
        for_ villain \v -> for_ (filter (cardMatch $ CardWithType AttachmentType <> CardWithTrait Technique) cards) (`attachEncounterCard` v)
      pure ms
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) -> ms <$ nextMainScheme (mainSchemeId attrs)
    _ -> TheArtOfEvasion <$> accelerateByEvasion msg attrs

warpDriveInitiated :: MainSchemeCard WarpDriveInitiated
warpDriveInitiated = mainScheme WarpDriveInitiated Cards.warpDriveInitiated (PerPlayer 9) (PerPlayer 3) (PerPlayer 0)

newtype WarpDriveInitiated = WarpDriveInitiated (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | 2A When Revealed: place 2 evasion counters on Nebula's Ship; for each evasion counter
-- there, discard the top 2 cards of each player deck and the encounter deck. 2B
-- completed: the players lose.
instance RunMessage WarpDriveInitiated where
  runMessage msg ms@(WarpDriveInitiated attrs) = runQueueT $ case msg of
    MainSchemeMessage ident RevealMainScheme | ident == mainSchemeId attrs -> do
      placeEvasionCounters 2
      cardSpecific (MainSchemeRef ident) "warpDriveDiscards" ()
      pure ms
    CardSpecific (isTarget attrs -> True) "warpDriveDiscards" _ -> do
      n <- (* 2) <$> getEvasionCounters
      when (n > 0) do
        players <- getPlayers
        for_ players (`discardTopOfDeck` n)
        discardTopOfEncounterDeck n
      pure ms
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) -> ms <$ gameOver Lost
    _ -> WarpDriveInitiated <$> accelerateByEvasion msg attrs
