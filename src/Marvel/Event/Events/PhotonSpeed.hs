module Marvel.Event.Events.PhotonSpeed (
  photonSpeed,
  PhotonSpeed (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Upgrades

photonSpeed :: EventCard PhotonSpeed
photonSpeed = event PhotonSpeed Cards.photonSpeed

newtype PhotonSpeed = PhotonSpeed (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Change to Photon energy form and remove 4 threat from a scheme. If you were already
-- in Photon energy form, ignore crisis icons for this thwart.
instance RunMessage PhotonSpeed where
  runMessage msg e@(PhotonSpeed attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      already <- isInEnergyForm you Upgrades.photon
      changeToEnergyForm you Upgrades.photon
      removeThreat you attrs 4 (if already then ThwartableSchemeIgnoringCrisis else ThwartableScheme)
      pure e
    _ -> PhotonSpeed <$> runMessage msg attrs
