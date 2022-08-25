module Marvel.Message.Obligation where

import Marvel.Prelude

import Marvel.Id

data ObligationMessage
  = RevealObligation IdentityId
  | ResolveObligation IdentityId
  | ResolvedObligation IdentityId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
