module Marvel.SideScheme.SideSchemes.RunningInterference (
  runningInterference,
  RunningInterference (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Cost
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Resource
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

runningInterference :: SideSchemeCard RunningInterference
runningInterference =
  sideSchemeWith RunningInterference Cards.runningInterference (PerPlayer 1) (crisisL .~ True)

newtype RunningInterference = RunningInterference (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Crisis. When Revealed: each player spends [mental] [physical] resources or places 2
-- threat here.
instance RunMessage RunningInterference where
  runMessage msg s@(RunningInterference attrs) = runQueueT $ case msg of
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      players <- getPlayers
      let cost = MultiResourceCost [Just Mental, Just Physical]
      for_ players \p -> do
        canPay <- canAffordCost p (toSource attrs) cost
        chooseOrRunOne p $
          labeled "$label.placeTwoThreatOnRunningInterference" (placeThreat ident 2)
            : [labeled "$label.spendMentalPhysicalResources" (payForTreachery p cost) | canPay]
      pure s
    _ -> RunningInterference <$> runMessage msg attrs
