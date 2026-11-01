module Marvel.MainScheme.MainSchemes.AgeOfApocalypseEnSabahNur where

import Marvel.Prelude
import Marvel.Phase
import Marvel.Game.Source
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Query
import Marvel.Trait qualified as Trait
import Marvel.Card
import Marvel.Entity
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Ability ()
import Marvel.Modifier ()
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype AoaEnSabahNurMainScheme = AoaEnSabahNurMainScheme (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

enSabahNursPyramid :: MainSchemeCard AoaEnSabahNurMainScheme
enSabahNursPyramid = mainScheme AoaEnSabahNurMainScheme Cards.enSabahNursPyramid (PerPlayer 8) (PerPlayer 1) (PerPlayer 1)

theRiseOfApocalypse :: MainSchemeCard AoaEnSabahNurMainScheme
theRiseOfApocalypse = mainScheme AoaEnSabahNurMainScheme Cards.theRiseOfApocalypse (PerPlayer 10) (PerPlayer 1) (PerPlayer 1)


instance RunMessage AoaEnSabahNurMainScheme where
  runMessage msg a@(AoaEnSabahNurMainScheme attrs) = runQueueT $ case msg of
    SetupMainScheme | toCardCode attrs == "45147" -> do
      getPlayers >>= traverse_ dealEncounterCard
      pure a
    MainSchemeMessage mid RevealMainScheme | mid == mainSchemeId attrs, toCardCode attrs == "45148" -> do
      discardUntil FromEncounterDeck (CardWithTrait Trait.Superpower) attrs
      pure a
    VillainPhaseStep StepEnemiesActivate -> do
      placeCounters attrs 1
      cardSpecific attrs "checkPowerCounters" ()
      pure a
    CardSpecific (isTarget attrs -> True) "checkPowerCounters" _ -> do
      when (mainSchemeCounters attrs >= 4) $ do
        removeCounters attrs 4
        discardUntil FromEncounterDeck (CardWithTrait Trait.Superpower) attrs
      pure a
    WithDiscardedMatch (isTarget attrs -> True) FromEncounterDeck (EncounterCard card) -> do
      first <- selectJust FirstPlayer
      revealEncounterCard first card
      pure a
    _ -> AoaEnSabahNurMainScheme <$> runMessage msg attrs
