module Marvel.MainScheme.MainSchemes.UndergroundDistribution where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards

undergroundDistribution :: MainSchemeCard UndergroundDistribution
undergroundDistribution =
  mainScheme UndergroundDistribution Cards.undergroundDistribution (PerPlayer 6) (Static 0) (PerPlayer 1)

newtype UndergroundDistribution = UndergroundDistribution (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage UndergroundDistribution where
  runMessage msg ms@(UndergroundDistribution attrs) = runQueueT $ case msg of
    MainSchemeMessage ident msg' | ident == mainSchemeId attrs ->
      case msg' of
        RevealMainScheme -> ms <$ discardUntil FromEncounterDeck (CardWithType MinionType) attrs
        _ -> UndergroundDistribution <$> runMessage msg attrs
    SetupMainScheme -> ms <$ searchForAndRevealScheme Cards.defenseNetwork
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) -> ms <$ nextMainScheme (mainSchemeId attrs)
    WithDiscardedMatch (isTarget attrs -> True) _ (EncounterCard card) -> do
      firstPlayer <- selectJust FirstPlayer
      ms <$ putEncounterCardIntoPlay firstPlayer card
    _ -> UndergroundDistribution <$> runMessage msg attrs
