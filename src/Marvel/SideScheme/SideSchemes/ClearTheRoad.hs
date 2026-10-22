module Marvel.SideScheme.SideSchemes.ClearTheRoad (
  clearTheRoad,
  ClearTheRoad (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Status

clearTheRoad :: SideSchemeCard ClearTheRoad
clearTheRoad = sideScheme ClearTheRoad Cards.clearTheRoad (Static 4)

newtype ClearTheRoad = ClearTheRoad (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Bulldozer's side scheme: it cannot leave play while he is in play, and leaves the game
-- with him (his signature scheme, set up by Breakout).
--
-- Charge! — Forced Response: after threat is placed here, if there is 10 or more threat here,
-- each player must discard the top 10 cards of their deck. Remove all but 3 threat from this scheme.
instance RunMessage ClearTheRoad where
  runMessage msg s@(ClearTheRoad attrs) = runQueueT $ case msg of
    SideSchemeMessage ident (SideSchemePlaceThreat n) | ident == sideSchemeId attrs -> do
      ClearTheRoad attrs' <- ClearTheRoad <$> runMessage msg attrs
      when (n > 0 && sideSchemeThreat attrs' >= 10) do
        players <- getPlayers
        for_ players \p -> discardTopOfDeck p 10
        cardSpecific attrs "allButThree" ()
      pure (ClearTheRoad attrs')
    CardSpecific (isTarget attrs -> True) "allButThree" _ -> do
      removeSideSchemeThreat (sideSchemeId attrs) (subtractNatural 3 (sideSchemeThreat attrs))
      pure s
    _ -> ClearTheRoad <$> runMessage msg attrs
