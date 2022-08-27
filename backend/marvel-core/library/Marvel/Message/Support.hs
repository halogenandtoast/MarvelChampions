module Marvel.Message.Support where

import Marvel.Prelude

data SupportMessage
  = ExhaustedSupport
  | ReadiedSupport
  | SpendSupportUse
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

