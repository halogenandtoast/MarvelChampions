module Marvel.Message.Treachery where

import Marvel.Prelude

import Marvel.Id

data TreacheryMessage
  = RevealTreachery IdentityId
  | -- | its incite keyword, a "When Revealed" ability, so cancelled with the others
    TreacheryIncite
  | -- | its surge keyword (printed, or given it as it is revealed), a "When Revealed"
    -- ability resolved before the card's own (RR v1.8 Surge), so cancelled with the others
    TreacheryKeywordSurge IdentityId
  | CheckTreacheryCondition IdentityId
  | TreacheryMarkResolved
  | ResolvedTreacheryAfterCancel IdentityId
  | ResolvedTreachery IdentityId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
