module Marvel.SideScheme.SideSchemes.AgeOfApocalypseClanAkkaba where

import Marvel.Prelude
import Marvel.Game.Source
import Marvel.Scenario.AgeOfApocalypseEnSabahNur ()
import Marvel.Card
import Marvel.Entity
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Ability ()
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype AoaClanAkkabaSideScheme = AoaClanAkkabaSideScheme (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ancientRitual :: SideSchemeCard AoaClanAkkabaSideScheme
ancientRitual = sideScheme AoaClanAkkabaSideScheme Cards.ancientRitual (Static 5)


instance RunMessage AoaClanAkkabaSideScheme where
  runMessage msg (AoaClanAkkabaSideScheme attrs) = runQueueT $ case msg of
    SideSchemeMessage sid (SideSchemePlaceThreat n) | sid == sideSchemeId attrs -> do
      attrs' <- runMessage msg attrs
      when (sideSchemeThreat attrs' >= 10 && n > 0) $ do
        removeSideSchemeThreat sid 5
        getPlayers >>= traverse_ dealEncounterCard
      pure $ AoaClanAkkabaSideScheme attrs'
    _ -> AoaClanAkkabaSideScheme <$> runMessage msg attrs
