module Marvel.MainScheme.MainSchemes.TheIslandOfDrZola where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Cards
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards

theIslandOfDrZola :: MainSchemeCard TheIslandOfDrZola
theIslandOfDrZola =
  mainScheme TheIslandOfDrZola Cards.theIslandOfDrZola (PerPlayer 6) (Static 0) (PerPlayer 1)

newtype TheIslandOfDrZola = TheIslandOfDrZola (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Its setup: Hydra Prison revealed, and an Ultimate Bio-Servant put into play engaged
-- with each player, the encounter deck shuffled (each search shuffles it).
instance RunMessage TheIslandOfDrZola where
  runMessage msg ms@(TheIslandOfDrZola attrs) = runQueueT $ case msg of
    SetupMainScheme -> do
      searchForAndRevealScheme Cards.hydraPrison
      players <- getPlayers
      for_ players \p -> searchAndPutIntoPlayEncounterCard p (CardIs Cards.ultimateBioServant)
      pure ms
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) -> ms <$ nextMainScheme (mainSchemeId attrs)
    _ -> TheIslandOfDrZola <$> testCounters msg attrs

-- | [star] Forced Response: after resolving step one of the villain phase, place 1 test
-- counter here. Then, if there are 3 or more test counters here, discard cards from the
-- top of the encounter deck until a minion is discarded, put it into play engaged with
-- the first player, and remove 3 test counters. Both Zola main schemes have it.
testCounters :: (ReverseQueue m) => Message -> Attrs MainScheme -> m (Attrs MainScheme)
testCounters msg attrs = case msg of
  AccelerateMainScheme -> do
    attrs' <- runMessage msg attrs
    placeCounters attrs 1
    cardSpecific attrs "testCounters" ()
    pure attrs'
  CardSpecific (isTarget attrs -> True) "testCounters" _ -> do
    when (mainSchemeCounters attrs >= 3) do
      discardUntil FromEncounterDeck (CardWithType MinionType) attrs
      removeCounters attrs 3
    pure attrs
  WithDiscardedMatch (isTarget attrs -> True) _ (EncounterCard card) -> do
    firstPlayer <- selectJust FirstPlayer
    putEncounterCardIntoPlay firstPlayer card
    pure attrs
  _ -> runMessage msg attrs
