module Marvel.Resource.Resources.AvianAnatomy where

import Marvel.Prelude
import Marvel.Card
import Marvel.Effect.Types
import Marvel.Entity ()
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Resource.Cards qualified as Cards
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W ()

-- The response is offered when spent; the return waits for the exact event to finish.
avianAnatomySpent :: ReverseQueue m => IdentityId -> PlayerCard -> PlayerCard -> m ()
avianAnatomySpent you played spent =
  when (getCardDef spent `sameCard` Cards.avianAnatomy && cardMatch (CardWithType EventType <> CardWithTrait Trait.Aerial) played) $
    chooseOne you
      [ labeled "$label.avianAnatomy" $ createEffect Cards.avianAnatomy (EventRef eid) (SourceEntity $ EventRef eid)
      , labeled "$label.noResponse" (pure ())
      ]
 where
  eid = EventId $ unCardId $ pcCardId played

newtype AvianAnatomyEffect = AvianAnatomyEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

avianAnatomyEffect :: CardEffect AvianAnatomyEffect
avianAnatomyEffect = effect AvianAnatomyEffect Cards.avianAnatomy

instance RunMessage AvianAnatomyEffect where
  runMessage msg e@(AvianAnatomyEffect attrs) = runQueueT $ case msg of
    EventMessage eid ResolvedEvent | effectSource attrs == EventRef eid -> do
      -- Event attrs have queued the exact discard before effects are dispatched.
      -- Return immediately after that discard, before the after-play responses.
      afterMatching (discarded eid) $ returnToHand (effectSource attrs)
      disableEffect (effectId attrs)
      pure e
    _ -> AvianAnatomyEffect <$> runMessage msg attrs
   where
    discarded eid (DiscardedCard (PlayerCard card)) = pcCardId card == CardId (unEventId eid)
    discarded _ _ = False
