module Marvel.SideScheme.SideSchemes.BringTheWar where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.GameValue
import Marvel.Hand ()
import Marvel.Stats ()

bringTheWar :: SideSchemeCard BringTheWar
bringTheWar = sideScheme BringTheWar Cards.bringTheWar (Static 2)

newtype BringTheWar = BringTheWar (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage BringTheWar where
  runMessage msg (BringTheWar attrs) = BringTheWar <$> runMessage msg attrs
