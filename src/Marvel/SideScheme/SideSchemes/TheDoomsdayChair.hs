module Marvel.SideScheme.SideSchemes.TheDoomsdayChair (
  theDoomsdayChair,
  TheDoomsdayChair (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message
import Marvel.Minion.Cards qualified as Cards
import Marvel.Modifier
import Marvel.Query
import Marvel.Queue
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

theDoomsdayChair :: SideSchemeCard TheDoomsdayChair
theDoomsdayChair = sideScheme TheDoomsdayChair Cards.theDoomsdayChair (Static 8)

newtype TheDoomsdayChair = TheDoomsdayChair (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage TheDoomsdayChair where
  runMessage msg s@(TheDoomsdayChair attrs) = case msg of
    -- if M.O.D.O.K. is not in play he is found and engages you
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      inPlay <- selectAny (MinionIs Cards.modok)
      unless inPlay $ push $ SearchForAndRevealScheme Cards.modok
      pure s
    _ -> TheDoomsdayChair <$> runMessage msg attrs
