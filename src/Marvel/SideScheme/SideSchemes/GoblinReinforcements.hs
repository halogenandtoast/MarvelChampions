module Marvel.SideScheme.SideSchemes.GoblinReinforcements (
  goblinReinforcements,
  GoblinReinforcements (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Matchers
import Marvel.Query
import Marvel.Trait (Trait (Goblin))

goblinReinforcements :: SideSchemeCard GoblinReinforcements
goblinReinforcements = sideScheme GoblinReinforcements Cards.goblinReinforcements (Static 2)

newtype GoblinReinforcements = GoblinReinforcements (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage GoblinReinforcements where
  runMessage msg s@(GoblinReinforcements attrs) = runQueueT $ case msg of
    -- When Revealed: 1 additional threat here for each Goblin minion in play
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      placeThreat ident =<< selectListCount (MinionWithTrait Goblin)
      pure s
    _ -> GoblinReinforcements <$> runMessage msg attrs
