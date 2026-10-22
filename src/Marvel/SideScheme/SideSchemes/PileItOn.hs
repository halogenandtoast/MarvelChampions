module Marvel.SideScheme.SideSchemes.PileItOn (
  pileItOn,
  PileItOn (..),
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

pileItOn :: SideSchemeCard PileItOn
pileItOn = sideScheme PileItOn Cards.pileItOn (Static 3)

newtype PileItOn = PileItOn (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Piledriver's side scheme: it cannot leave play while he is in play, and leaves the game
-- with him (his signature scheme, set up by Breakout).
--
-- Pile Drive — Forced Response: after threat is placed here, if there is 10 or more threat here,
-- each player discards the upgrade or support they
-- control with the highest cost. Remove all but 3 threat from this scheme.
instance RunMessage PileItOn where
  runMessage msg s@(PileItOn attrs) = runQueueT $ case msg of
    SideSchemeMessage ident (SideSchemePlaceThreat n) | ident == sideSchemeId attrs -> do
      PileItOn attrs' <- PileItOn <$> runMessage msg attrs
      when (n > 0 && sideSchemeThreat attrs' >= 10) do
        players <- getPlayers
        for_ players \p -> do
          cards <- highestCostControlled p True True
          unless (null cards) $ chooseOrRunOne p [targeting c (discardControlled c) | c <- cards]
        cardSpecific attrs "allButThree" ()
      pure (PileItOn attrs')
    CardSpecific (isTarget attrs -> True) "allButThree" _ -> do
      removeSideSchemeThreat (sideSchemeId attrs) (subtractNatural 3 (sideSchemeThreat attrs))
      pure s
    _ -> PileItOn <$> runMessage msg attrs
