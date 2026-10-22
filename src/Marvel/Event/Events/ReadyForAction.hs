module Marvel.Event.Events.ReadyForAction (
  readyForAction,
  ReadyForAction (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Status

readyForAction :: EventCard ReadyForAction
readyForAction = event ReadyForAction Cards.readyForAction

newtype ReadyForAction = ReadyForAction (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Give an ally you control a tough status card.
instance RunMessage ReadyForAction where
  runMessage msg e@(ReadyForAction attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      allies <- selectList $ AllyControlledBy (IdentityWithId identityId)
      chooseOne identityId [targeting (AllyRef a) (giveStatus (AllyCharacter a) ToughStatus) | a <- allies]
      pure e
    _ -> ReadyForAction <$> runMessage msg attrs
