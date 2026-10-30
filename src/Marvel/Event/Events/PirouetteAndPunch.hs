module Marvel.Event.Events.PirouetteAndPunch where

import Marvel.Prelude
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Window qualified as W

pirouetteAndPunch :: EventCard PirouetteAndPunch
pirouetteAndPunch = event PirouetteAndPunch Cards.pirouetteAndPunch

newtype PirouetteAndPunch = PirouetteAndPunch (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage PirouetteAndPunch where
  runMessage msg e@(PirouetteAndPunch attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ (Just (W.EncounterCardRevealed _ _ card))) | eid == eventId attrs -> do
      cancelEncounterWhenRevealed card
      countBoostIcons attrs you [card]
      pure e
    WithBoostIcons (isTarget attrs -> True) (fromJSON -> Success you) (sum . map snd -> n) -> do
      withTheVillain you $ \villain -> dealDamage villain attrs (toDamage (n + 1) FromAbility)
      pure e
    _ -> PirouetteAndPunch <$> runMessage msg attrs
