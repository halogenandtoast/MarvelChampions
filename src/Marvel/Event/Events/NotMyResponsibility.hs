module Marvel.Event.Events.NotMyResponsibility where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Window qualified as W

notMyResponsibility :: EventCard NotMyResponsibility
notMyResponsibility = event NotMyResponsibility Cards.notMyResponsibility

newtype NotMyResponsibility = NotMyResponsibility (Attrs Event)
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage NotMyResponsibility where
  runMessage msg e@(NotMyResponsibility attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ (Just (W.ThreatPlaced _ scheme _))) | eid == eventId attrs -> do
      pending <- pendingThreatPlacement scheme
      when (maybe False (> 0) pending) do
        allies <- selectList (AllyControlledBy $ IdentityWithId you)
        chooseOne you [targeting (characterRef character) (cardSpecific attrs "replace" (scheme, character))
          | character <- IdentityCharacter you : map AllyCharacter allies]
      pure e
    CardSpecific (isTarget attrs -> True) "replace" (fromJSON -> Success (scheme, character)) -> do
      replaceThreatWithDamage attrs scheme character
      pure e
    _ -> NotMyResponsibility <$> runMessage msg attrs
