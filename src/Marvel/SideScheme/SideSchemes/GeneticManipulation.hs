module Marvel.SideScheme.SideSchemes.GeneticManipulation where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue
import Marvel.Id ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Ref
import Marvel.Stats ()
import Marvel.Window qualified as W ()
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Attachment.Cards qualified as Attachments

geneticManipulation :: SideSchemeCard GeneticManipulation
geneticManipulation = sideScheme GeneticManipulation Cards.geneticManipulation (PerPlayer 2)

newtype GeneticManipulation = GeneticManipulation (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage GeneticManipulation where
  runMessage msg e@(GeneticManipulation attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      you <- getDefeatedBy attrs
      searchAndRevealEncounterCard you (CardIs Attachments.geneTherapy)
      pure e
    _ -> GeneticManipulation <$> runMessage msg attrs
