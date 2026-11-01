module Marvel.SideScheme.SideSchemes.AgeOfApocalypseUnus where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.GameValue
import Marvel.Modifier (HasModifiersFor)
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.SideScheme.SideSchemes.AgeOfApocalypseInfinites (placeGenePoolThreat)

newtype AoaUnusSideScheme = AoaUnusSideScheme (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

endlessRanks :: SideSchemeCard AoaUnusSideScheme
endlessRanks = sideScheme AoaUnusSideScheme Cards.endlessRanks (Static 4)

instance RunMessage AoaUnusSideScheme where
  runMessage msg e@(AoaUnusSideScheme a) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId a -> e <$ placeGenePoolThreat 3
    _ -> AoaUnusSideScheme <$> runMessage msg a
