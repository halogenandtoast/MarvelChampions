module Marvel.SideScheme.SideSchemes.GrandLarceny where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.GameValue
import Marvel.Trait
import Marvel.Query

grandLarceny :: SideSchemeCard GrandLarceny
grandLarceny = sideSchemeWith GrandLarceny Cards.grandLarceny (Static 4) (crisisL .~ True)

newtype GrandLarceny = GrandLarceny (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor GrandLarceny where
  getModifiersFor _ (SideSchemeRef sid) (GrandLarceny attrs) | sid == sideSchemeId attrs = do
    criminal <- selectAny $ MinionWithTrait Criminal
    pure [ThreatCannotBeRemoved | criminal]
  getModifiersFor _ _ _ = pure []

instance RunMessage GrandLarceny where
  runMessage msg e@(GrandLarceny attrs) = runQueueT $ case msg of
    _ -> GrandLarceny <$> runMessage msg attrs
