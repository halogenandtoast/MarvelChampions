module Marvel.Message.Event where

import Marvel.Prelude

import Marvel.Id
import Marvel.Payment
import Marvel.Window (WindowType)

data EventMessage
  = PlayedEvent IdentityId Payment (Maybe WindowType)
  | ResolvedEvent
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
