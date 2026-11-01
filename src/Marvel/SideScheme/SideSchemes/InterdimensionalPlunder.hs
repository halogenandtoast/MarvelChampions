module Marvel.SideScheme.SideSchemes.InterdimensionalPlunder where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

interdimensionalPlunder :: SideSchemeCard InterdimensionalPlunder
interdimensionalPlunder = sideSchemeWith InterdimensionalPlunder Cards.interdimensionalPlunder (Static 2) (crisisL .~ True)

newtype InterdimensionalPlunder = InterdimensionalPlunder (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage InterdimensionalPlunder where
  runMessage msg a@(InterdimensionalPlunder attrs) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId attrs -> do
      n <- selectListCount (UpgradeMatches [])
      placeThreat (sideSchemeId attrs) n
      pure a
    _ -> InterdimensionalPlunder <$> runMessage msg attrs
