module Marvel.Event.Events.InspiringPresence (
  inspiringPresence,
  InspiringPresence (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Choice
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref

inspiringPresence :: EventCard InspiringPresence
inspiringPresence = event InspiringPresence Cards.inspiringPresence

newtype InspiringPresence = InspiringPresence (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Heal 1 damage from an ally and ready it."
instance RunMessage InspiringPresence where
  runMessage msg e@(InspiringPresence attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        allies <- selectList AnyAlly
        chooseOne identityId
          [ TargetLabel (toRef ally) [Heal (AllyCharacter ally) 1, Run [AllyMessage ally ReadiedAlly]]
          | ally <- allies
          ]
        pure e
      _ -> InspiringPresence <$> runMessage msg attrs
    _ -> InspiringPresence <$> runMessage msg attrs
