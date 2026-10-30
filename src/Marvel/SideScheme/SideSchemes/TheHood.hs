module Marvel.SideScheme.SideSchemes.TheHood where

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Phase
import Marvel.Prelude
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Villain.Villains.TheHood

newtype UnbridledAmbition = UnbridledAmbition (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

unbridledAmbition :: SideSchemeCard UnbridledAmbition
unbridledAmbition = sideScheme UnbridledAmbition Cards.unbridledAmbition (Static 2)

instance RunMessage UnbridledAmbition where
  runMessage msg e@(UnbridledAmbition attrs) = runQueueT $ case msg of
    BeginPhase VillainPhase -> e <$ (traverse_ foulPlay =<< getPlayers)
    _ -> UnbridledAmbition <$> runMessage msg attrs
