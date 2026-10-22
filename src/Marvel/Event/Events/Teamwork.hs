module Marvel.Event.Events.Teamwork (
  teamwork,
  Teamwork (..),
) where

import Marvel.Prelude

import Marvel.Payment
import Marvel.Card.Code
import Marvel.Count
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers hiding (ExhaustedAlly)
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Window.Types (WindowType (MadeBasicAttack))

teamwork :: EventCard Teamwork
teamwork = event Teamwork Cards.teamwork

newtype Teamwork = Teamwork (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | As the hero uses their basic ATK or THW: exhausting an ally you control is its cost
-- → add its matching power to the hero's for this use.
instance RunMessage Teamwork where
  runMessage msg e@(Teamwork attrs) = case msg of
    EventMessage ident (PlayedEvent identityId payment window) | ident == eventId attrs -> do
      let attacking = case window of
            Just (MadeBasicAttack _) -> True
            _ -> False
      for_ (paymentExhaustedAllies payment) \ally -> do
        power <- selectCount (if attacking then AllyAttackValue else AllyThwartValue) (AllyWithId ally)
        addToBasicPower identityId power
      pure e
    _ -> Teamwork <$> runMessage msg attrs
