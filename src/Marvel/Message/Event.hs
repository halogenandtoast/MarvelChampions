module Marvel.Message.Event where

import Marvel.Prelude

import Marvel.Id
import Marvel.Payment.Types
import Marvel.Window.Types (WindowType)

data EventMessage
  = PlayedEvent IdentityId Payment (Maybe WindowType)
  | ResolvedEvent
  | -- | the event's damage attacked the enemy for the player
    EventAttacked IdentityId EnemyId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
