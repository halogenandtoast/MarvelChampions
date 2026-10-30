module Marvel.Message.Treachery where

import Marvel.Prelude

import Marvel.Id

data TreacheryMessage
  = RevealTreachery IdentityId
  | -- | its incite keyword, a "When Revealed" ability, so cancelled with the others
    TreacheryIncite
  | CheckTreacheryCondition IdentityId
  | TreacheryMarkResolved
  | ResolvedTreacheryAfterCancel IdentityId
  | ResolvedTreachery IdentityId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
