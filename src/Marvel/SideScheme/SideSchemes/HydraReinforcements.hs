module Marvel.SideScheme.SideSchemes.HydraReinforcements (
  hydraReinforcements,
  HydraReinforcements (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Trait

hydraReinforcements :: SideSchemeCard HydraReinforcements
hydraReinforcements = sideScheme HydraReinforcements Cards.hydraReinforcements (PerPlayer 2)

newtype HydraReinforcements = HydraReinforcements (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Defeated: the player who defeated this scheme discards a non-Elite minion.
instance RunMessage HydraReinforcements where
  runMessage msg s@(HydraReinforcements attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      you <- getDefeatedBy attrs
      minions <- selectList (NotMinion $ MinionWithTrait Elite)
      unless (null minions) $ chooseOne you [targeting m (removeFromPlay m) | m <- minions]
      pure s
    _ -> HydraReinforcements <$> runMessage msg attrs
