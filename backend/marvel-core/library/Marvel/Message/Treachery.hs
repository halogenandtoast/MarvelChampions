module Marvel.Message.Treachery where

import Marvel.Prelude

import Marvel.Id

data TreacheryMessage
  = RevealTreachery IdentityId
  | CheckTreacheryCondition IdentityId
  | ResolvedTreachery IdentityId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
