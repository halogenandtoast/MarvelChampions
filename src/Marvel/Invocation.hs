-- | Doctor Strange's Invocation deck (see the Doctor Strange insert): the five
-- Invocation cards, set aside beside his player deck and never part of it. Its top card
-- is always faceup; resolved Invocation cards go to a discard pile of its own, and once
-- the deck is empty that pile is shuffled to make it again.
--
-- Doctor Strange keeps the deck; everything else asks him for a step with 'invocation'.
module Marvel.Invocation (
  InvocationStep (..),
  invocation,
  invocationResolved,
) where

import Marvel.Prelude

import Marvel.Card.Id
import Marvel.Card.PlayerCard
import Marvel.Event.Types (Attrs (..), Event)
import Marvel.Id
import Marvel.Message
import Marvel.Ref

data InvocationStep
  = SetUpInvocationDeck
  | DiscardTopInvocation
  | -- | resolve the top card's "Special", and put it back on top after if so
    ResolveTopInvocation Bool
  | PlaceInInvocationDiscard PlayerCard
  | -- | the top card facedown under the card targeted, which is told it as
    -- @CardSpecific target "invocationUnder" card@
    PlaceTopInvocationUnder Target
  | ShuffleIntoInvocationDeck PlayerCard
  | ReturnToTopOfInvocationDeck PlayerCard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A step for the Invocation deck of the player given.
invocation :: IdentityId -> InvocationStep -> Message
invocation ident = cardSpecific ident "invocation"

-- | An Invocation card's "Special" is done: the card goes to the Invocation deck's
-- discard pile, not its player's.
invocationResolved :: Attrs Event -> Message
invocationResolved attrs = invocation (eventController attrs) (PlaceInInvocationDiscard card)
 where
  card = MkPlayerCard (CardId . unEventId $ eventId attrs) (eventCardDef attrs) (Just $ eventController attrs) (Just $ eventController attrs)
