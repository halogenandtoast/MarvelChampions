module Marvel.SideScheme.SideSchemes.TimePortal (
  timePortal,
  TimePortal (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

timePortal :: SideSchemeCard TimePortal
timePortal = sideScheme TimePortal Cards.timePortal (PerPlayer 2)

newtype TimePortal = TimePortal (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Forced Interrupt: when this scheme is defeated, it is shuffled into the encounter
-- deck instead of being discarded (it leaves play, then goes from the discard pile).
instance RunMessage TimePortal where
  runMessage msg s@(TimePortal attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      for_ [ec | EncounterCard ec <- [toCard attrs]] \ec ->
        afterMatching (== DefeatedLeavesPlay (toTarget attrs)) (shuffleIntoEncounterDeck [ec])
      pure s
    _ -> TimePortal <$> runMessage msg attrs
