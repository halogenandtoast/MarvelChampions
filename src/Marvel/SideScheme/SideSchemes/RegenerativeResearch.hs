module Marvel.SideScheme.SideSchemes.RegenerativeResearch where

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
import Marvel.Matchers
import Marvel.Phase (Phase (VillainPhase))
import Marvel.Query

regenerativeResearch :: SideSchemeCard RegenerativeResearch
regenerativeResearch = sideScheme RegenerativeResearch Cards.regenerativeResearch (Static 5)

newtype RegenerativeResearch = RegenerativeResearch (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage RegenerativeResearch where
  runMessage msg s@(RegenerativeResearch attrs) = runQueueT $ case msg of
    BeginPhase VillainPhase -> do
      enemies <- selectList AnyEnemy
      for_ enemies (`heal` 1)
      pure s
    _ -> RegenerativeResearch <$> runMessage msg attrs
