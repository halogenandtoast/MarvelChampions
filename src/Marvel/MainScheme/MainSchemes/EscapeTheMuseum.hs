-- | Escape the Museum's main schemes (The Galaxy's Most Wanted): The Missing Milano,
-- Lost in the Museum and The Great Escape. The players move on as each is cleared of
-- threat, and lose as any is completed.
module Marvel.MainScheme.MainSchemes.EscapeTheMuseum where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.EncounterCard (genEncounterCard)
import Marvel.EncounterSet (EncounterSet (ShipCommand))
import Marvel.Entity
import Marvel.Environment.Cards qualified as Environments
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Card.Id ()
import Marvel.Id (SupportId (..))
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Support.Cards qualified as Supports

theMissingMilano :: MainSchemeCard TheMissingMilano
theMissingMilano = mainScheme TheMissingMilano Cards.theMissingMilano (PerPlayer 11) (PerPlayer 7) (PerPlayer 1)

lostInTheMuseum :: MainSchemeCard LostInTheMuseum
lostInTheMuseum = mainScheme LostInTheMuseum Cards.lostInTheMuseum (PerPlayer 15) (PerPlayer 11) (PerPlayer 1)

theGreatEscape :: MainSchemeCard TheGreatEscape
theGreatEscape = mainScheme TheGreatEscape Cards.theGreatEscape (PerPlayer 12) (PerPlayer 8) (PerPlayer 1)

-- | Each stage's B side: "If this stage is completed, the players lose the game", and
-- what follows as the last threat is removed from it ("Forced Interrupt: when the last
-- threat is removed from this scheme", "If there is no threat here").
escapeStage :: (ReverseQueue m) => m () -> Message -> Attrs MainScheme -> m (Attrs MainScheme)
escapeStage cleared msg attrs = case msg of
  AdvanceMainScheme mid | mid == mainSchemeId attrs -> attrs <$ gameOver Lost
  CardSpecific target "resolveThwart" _ | target == toRef attrs -> do
    attrs' <- runMessage msg attrs
    when (mainSchemeThreat attrs > 0 && mainSchemeThreat attrs' == 0) cleared
    pure attrs'
  MainSchemeMessage mid m | mid == mainSchemeId attrs, removes m -> do
    attrs' <- runMessage msg attrs
    when (mainSchemeThreat attrs > 0 && mainSchemeThreat attrs' == 0) cleared
    pure attrs'
  _ -> runMessage msg attrs
 where
  removes = \case
    MainSchemeBasicThwarted {} -> True
    MainSchemeThwarted {} -> True
    MainSchemeThreatRemoved {} -> True
    _ -> False

newtype TheMissingMilano = TheMissingMilano (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | 1A Setup: put the Library Labyrinth environment into play; set aside the Ship
-- Command modular set: its cards in the encounter deck, and the Milano, put into play by
-- its setup keyword just before (RR Appendix II, step 11), out of play again. 1B: cleared
-- of threat, advance to stage 2A (the players win by advancing).
instance RunMessage TheMissingMilano where
  runMessage msg ms@(TheMissingMilano attrs) = runQueueT $ case msg of
    SetupMainScheme -> do
      first <- selectJust FirstPlayer
      putEncounterCardIntoPlay first =<< genEncounterCard Environments.libraryLabyrinth
      milano <- getMilano
      deck <- getEncounterDeck
      setAside $
        [EncounterCard (MkEncounterCard (CardId $ unSupportId sid) Supports.milano) | Just sid <- [milano]]
          <> [EncounterCard c | c <- deck, cdEncounterSet (getCardDef c) == Just ShipCommand]
      pure ms
    _ -> TheMissingMilano <$> escapeStage (nextMainScheme (mainSchemeId attrs)) msg attrs

newtype LostInTheMuseum = LostInTheMuseum (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | 2A When Revealed: put the set-aside Milano into play under the first player's
-- control. 2B: cleared of threat, advance to stage 3A.
instance RunMessage LostInTheMuseum where
  runMessage msg ms@(LostInTheMuseum attrs) = runQueueT $ case msg of
    MainSchemeMessage ident RevealMainScheme | ident == mainSchemeId attrs -> do
      first <- selectJust FirstPlayer
      setAsideCards <- getSetAsideEncounterCards
      for_ [c | c <- setAsideCards, getCardDef c == Supports.milano] (putEncounterCardIntoPlay first)
      pure ms
    _ -> LostInTheMuseum <$> escapeStage (nextMainScheme (mainSchemeId attrs)) msg attrs

newtype TheGreatEscape = TheGreatEscape (Attrs MainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | 3B First Player Action: exhaust the Milano → remove 3 threat from here.
instance IsMainScheme TheGreatEscape where
  mainSchemeAbilities (TheGreatEscape a) =
    [ability a 1 Action (SelfMatches FirstPlayer) ExhaustMilanoCost $ runAbility a 1]

-- | 3A When Revealed: flip Library Labyrinth; place 1 acceleration token on the main
-- scheme; shuffle the remaining cards of the set-aside Ship Command set into the
-- encounter deck. 3B: with no threat here, the players win.
instance RunMessage TheGreatEscape where
  runMessage msg ms@(TheGreatEscape attrs) = runQueueT $ case msg of
    MainSchemeMessage ident RevealMainScheme | ident == mainSchemeId attrs -> do
      traverse_ flipEnvironment =<< selectOne (EnvironmentIs Environments.libraryLabyrinth)
      placeAccelerationToken (mainSchemeId attrs)
      setAsideCards <- getSetAsideEncounterCards
      shuffleIntoEncounterDeck [c | c <- setAsideCards, cdEncounterSet (getCardDef c) == Just ShipCommand]
      pure ms
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> ms <$ thwartScheme (mainSchemeId attrs) attrs 3
    _ -> TheGreatEscape <$> escapeStage (gameOver Won) msg attrs
