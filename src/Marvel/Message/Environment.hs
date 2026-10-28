module Marvel.Message.Environment where

import Marvel.Prelude

import Marvel.Id

data EnvironmentMessage
  = AttachedToEnvironment AttachmentId
  | -- | it turns to its other face, keeping its attachments and counters
    FlipEnvironment
  | -- | it has entered play: "enters play with 2 counters"
    EnvironmentEntered
  | -- | it is in the player's play area (Ebony Maw's Spells)
    EnvironmentOwnedBy IdentityId
  | -- | damage placed on it (Tower Defense's Avengers Tower)
    EnvironmentDamaged Natural
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
