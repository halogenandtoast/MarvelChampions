module Marvel.SideScheme.SideSchemes.TheAnachronauts (
  theAnachronauts,
  TheAnachronauts (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Trait (Trait (Temporal))

theAnachronauts :: SideSchemeCard TheAnachronauts
theAnachronauts = sideScheme TheAnachronauts Cards.theAnachronauts (PerPlayer 3)

newtype TheAnachronauts = TheAnachronauts (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Defeated: each Temporal card in the encounter discard pile is shuffled into the
-- encounter deck.
instance RunMessage TheAnachronauts where
  runMessage msg s@(TheAnachronauts attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      temporal <- filter (cardMatch $ CardWithTrait Temporal) <$> getEncounterDiscard
      unless (null temporal) $ shuffleIntoEncounterDeck temporal
      pure s
    _ -> TheAnachronauts <$> runMessage msg attrs
