module Marvel.SideScheme.SideSchemes.BlazingInferno (
  blazingInferno,
  BlazingInferno (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Phase (Phase (VillainPhase))
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

-- | Base threat 3 per player.
blazingInferno :: SideSchemeCard BlazingInferno
blazingInferno = sideScheme BlazingInferno Cards.blazingInferno (PerPlayer 3)

newtype BlazingInferno = BlazingInferno (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Forced Response: after the villain phase begins, deal 2 indirect damage to each
-- player.
instance RunMessage BlazingInferno where
  runMessage msg s@(BlazingInferno attrs) = runQueueT $ case msg of
    BeginPhase VillainPhase -> do
      players <- getPlayers
      for_ players \p -> dealIndirectDamage p attrs 2
      pure s
    _ -> BlazingInferno <$> runMessage msg attrs
