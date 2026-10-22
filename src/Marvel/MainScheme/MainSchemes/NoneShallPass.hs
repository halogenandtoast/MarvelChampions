module Marvel.MainScheme.MainSchemes.NoneShallPass where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query

noneShallPass :: MainSchemeCard NoneShallPass
noneShallPass =
  mainScheme NoneShallPass Cards.noneShallPass (PerPlayer 12) (PerPlayer 2) (PerPlayer 1)

newtype NoneShallPass = NoneShallPass (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage NoneShallPass where
  runMessage msg ms@(NoneShallPass attrs) = runQueueT $ case msg of
    -- 1A: cards are discarded from the encounter deck until an environment is; it is put
    -- into play (no surge: rulebook p.7 Q&A), and the discard pile shuffled back in
    SetupMainScheme -> do
      discardUntil FromEncounterDeck (CardWithType EnvironmentType) attrs
      pure ms
    WithDiscardedMatch (isTarget attrs -> True) FromEncounterDeck (EncounterCard card) -> do
      you <- selectJust FirstPlayer
      putEncounterCardIntoPlay you card
      shuffleIntoEncounterDeck =<< getEncounterDiscard
      pure ms
    -- [star] Forced Response: after step one of the villain phase, 1 delay counter here
    AccelerateMainScheme -> do
      ms' <- NoneShallPass <$> runMessage msg attrs
      placeCounters attrs 1
      pure ms'
    -- Forced Interrupt: an environment entering play discards each other one
    EnvironmentMessage eid EnvironmentEntered -> do
      others <- filter (/= eid) <$> selectList AnyEnvironment
      for_ others removeFromPlay
      pure ms
    AdvanceMainScheme -> ms <$ gameOver Lost
    _ -> NoneShallPass <$> runMessage msg attrs
