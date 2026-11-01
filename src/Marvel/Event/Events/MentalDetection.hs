module Marvel.Event.Events.MentalDetection where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Card
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

mentalDetection :: EventCard MentalDetection
mentalDetection = event MentalDetection Cards.mentalDetection

newtype MentalDetection = MentalDetection (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage MentalDetection where
  runMessage msg a@(MentalDetection attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      knives <- selectListCount $ UpgradeControlledBy (IdentityWithId you) <> UpgradeIs Upgrades.psiKnife
      whenM (selectAny SchemeYouCanThwart) $ removeThreat you attrs (1 + 2 * knives) SchemeYouCanThwart
      cardSpecific attrs "draw" you
      pure a
    CardSpecific (isTarget attrs -> True) "draw" (fromJSON -> Success you) -> do
      katanas <- selectListCount $ UpgradeControlledBy (IdentityWithId you) <> UpgradeIs Upgrades.psiKatana
      drawCards you katanas
      pure a
    _ -> MentalDetection <$> runMessage msg attrs
