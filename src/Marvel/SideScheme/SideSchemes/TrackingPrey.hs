module Marvel.SideScheme.SideSchemes.TrackingPrey where

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
import Marvel.Game.Source
import Marvel.Matchers

trackingPrey :: SideSchemeCard TrackingPrey
trackingPrey = sideScheme TrackingPrey Cards.trackingPrey (Static 4)

newtype TrackingPrey = TrackingPrey (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage TrackingPrey where
  runMessage msg s@(TrackingPrey attrs) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId attrs -> do
      you <- getYou
      alterEgo <- identityMatches AlterEgoIdentity you
      when alterEgo $ placeSideSchemeAccelerationToken sid
      pure s
    _ -> TrackingPrey <$> runMessage msg attrs
