module Marvel.SideScheme.SideSchemes.MassChaos (
  massChaos,
  MassChaos (..),
) where

import Marvel.Prelude

import Data.List (nub)
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

massChaos :: SideSchemeCard MassChaos
massChaos = sideSchemeWith MassChaos Cards.massChaos (Static 0) (crisisL .~ True)

newtype MassChaos = MassChaos (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Revealed: each player discards the top 5 cards of their deck and places 1
-- threat here for each different type of resource icon they discarded this way.
instance RunMessage MassChaos where
  runMessage msg s@(MassChaos attrs) = runQueueT $ case msg of
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      players <- getPlayers
      for_ players \p -> discardFromDeck p 5 attrs
      pure s
    WithDiscarded (isTarget attrs -> True) _ (onlyPlayerCards -> cards) -> do
      let types = nub [r | c <- cards, (_, r) <- cdResources (getCardDef c)]
      placeThreat (sideSchemeId attrs) (fromIntegral $ length types)
      pure s
    _ -> MassChaos <$> runMessage msg attrs
