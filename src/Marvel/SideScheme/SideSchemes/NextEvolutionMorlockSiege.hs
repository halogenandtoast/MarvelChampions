module Marvel.SideScheme.SideSchemes.NextEvolutionMorlockSiege where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Ability (HasAbilities)
import Marvel.Game.Source
import Marvel.NextEvolution.Helpers
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype MorlockSiegeSideScheme = MorlockSiegeSideScheme (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40084 :: SideSchemeCard MorlockSiegeSideScheme
ne40084 = sideScheme MorlockSiegeSideScheme Cards.ne40084 (Static 3)

ne40085 :: SideSchemeCard MorlockSiegeSideScheme
ne40085 = sideScheme MorlockSiegeSideScheme Cards.ne40085 (Static 3)

ne40086 :: SideSchemeCard MorlockSiegeSideScheme
ne40086 = sideScheme MorlockSiegeSideScheme Cards.ne40086 (Static 3)

ne40087 :: SideSchemeCard MorlockSiegeSideScheme
ne40087 = sideScheme (\a -> MorlockSiegeSideScheme a {sideSchemeCrisis = True}) Cards.ne40087 (Static 4)

instance RunMessage MorlockSiegeSideScheme where
  runMessage msg e@(MorlockSiegeSideScheme a) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId a -> do
      count <- routedCount
      players <- fromIntegral <$> getStartingPlayerCount
      placeThreat sid (count * players)
      pure e
    _ -> MorlockSiegeSideScheme <$> runMessage msg a
