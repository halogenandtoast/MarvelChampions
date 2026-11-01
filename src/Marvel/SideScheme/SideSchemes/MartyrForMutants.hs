module Marvel.SideScheme.SideSchemes.MartyrForMutants where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource.Types ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window ()
import Marvel.Identity.Types ()

import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

martyrForMutants :: SideSchemeCard MartyrForMutants
martyrForMutants = sideScheme MartyrForMutants Cards.martyrForMutants (PerPlayer 3)

newtype MartyrForMutants = MartyrForMutants (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage MartyrForMutants where
  runMessage msg e@(MartyrForMutants attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      you <- getDefeatedBy attrs
      discardTopOfDeck you 9
      pure e
    _ -> MartyrForMutants <$> runMessage msg attrs
