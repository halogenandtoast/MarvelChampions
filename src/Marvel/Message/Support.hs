module Marvel.Message.Support where

import Marvel.Prelude

import Marvel.Id (IdentityId)
import Marvel.Card.PlayerCard.Types (PlayerCard)

data SupportMessage
  = AttachPlayableEvent PlayerCard
  | ReserveHeldCard PlayerCard
  | ExhaustedSupport
  | ReadiedSupport
  | SpendSupportUse
  | AddSupportUses Natural
  | -- | whose deck it came from, and whose discard pile it goes to
    SupportOwnedBy IdentityId
  | -- | another player takes control of it, as the first player does of the Milano
    SupportChangedController IdentityId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

