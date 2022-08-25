module Marvel.MainScheme.MainSchemes.UndergroundDistribution where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.GameValue
import Marvel.MainScheme.Types
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Queue
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.Source
import Marvel.Target

undergroundDistribution :: MainSchemeCard UndergroundDistribution
undergroundDistribution =
  mainScheme UndergroundDistribution Cards.undergroundDistribution (PerPlayer 6) (Static 0) (PerPlayer 1)

newtype UndergroundDistribution = UndergroundDistribution (Attrs MainScheme)
  deriving anyclass IsMainScheme
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance RunMessage UndergroundDistribution where
  runMessage msg ms@(UndergroundDistribution attrs) = case msg of
    MainSchemeMessage ident msg' | ident == mainSchemeId attrs ->
      case msg' of
        RevealMainScheme -> ms <$ push (DiscardUntil FromEncounterDeck (CardWithType MinionType) (toTarget attrs))
        _ -> UndergroundDistribution <$> runMessage msg attrs
    SetupMainScheme -> ms <$ push (SearchForAndRevealScheme Cards.defenseNetwork)
    AdvanceMainScheme -> ms <$ push NextMainScheme
    WithDiscardedMatch (isTarget attrs -> True) _ (EncounterCard card) -> do
      firstPlayer <- selectJust FirstPlayer
      ms <$ push (RevealEncounterCard firstPlayer card)
    _ -> UndergroundDistribution <$> runMessage msg attrs
