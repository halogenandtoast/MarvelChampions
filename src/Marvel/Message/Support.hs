module Marvel.Message.Support where

import Marvel.Prelude

import Marvel.Id (IdentityId)

data SupportMessage
  = ExhaustedSupport
  | ReadiedSupport
  | SpendSupportUse
  | -- | whose deck it came from, and whose discard pile it goes to
    SupportOwnedBy IdentityId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

