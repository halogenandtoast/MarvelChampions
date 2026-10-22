module Marvel.Event.Events.GreatResponsibility (
  greatResponsibility,
  GreatResponsibility (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Window

greatResponsibility :: EventCard GreatResponsibility
greatResponsibility = event GreatResponsibility Cards.greatResponsibility

newtype GreatResponsibility = GreatResponsibility (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage GreatResponsibility where
  runMessage msg e@(GreatResponsibility attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ (Just (ThreatPlaced _ schemeId n)) -> do
        -- taken as damage from this card: "would take damage" interrupts answer it
        insteadOf (isThreatPlacement schemeId) $ damageHero identityId attrs (toDamage n FromAbility)
        pure e
      _ -> GreatResponsibility <$> runMessage msg attrs
    _ -> GreatResponsibility <$> runMessage msg attrs
