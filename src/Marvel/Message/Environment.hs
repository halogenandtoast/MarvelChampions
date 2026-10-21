module Marvel.Message.Environment where

import Marvel.Prelude

import Marvel.Id

newtype EnvironmentMessage
  = AttachedToEnvironment AttachmentId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
