module Marvel.SideScheme.SideSchemes.TheVipersAmbition (
  theVipersAmbition,
  TheVipersAmbition (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

theVipersAmbition :: SideSchemeCard TheVipersAmbition
theVipersAmbition = sideScheme TheVipersAmbition Cards.theVipersAmbition (Static 2)

newtype TheVipersAmbition = TheVipersAmbition (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Revealed: Place an additional 1 [per_hero] threat here.
instance RunMessage TheVipersAmbition where
  runMessage msg s@(TheVipersAmbition attrs) = runQueueT $ case msg of
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      n <- fromIntegral <$> getPlayerCount
      placeThreat (SchemeSideSchemeId ident) n
      pure s
    _ -> TheVipersAmbition <$> runMessage msg attrs
