module Marvel.Resource.Resources.PassionForJustice where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Status
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Effect.Types
import Marvel.Resource.Cards qualified as Cards

passionForJusticeSpent :: ReverseQueue m => IdentityId -> PlayerCard -> PlayerCard -> m ()
passionForJusticeSpent you played spent =
  when (getCardDef spent `sameCard` Cards.passionForJustice && cdCardType (getCardDef played) == EventType && Trait.Thwart `member` cdTraits (getCardDef played)) $
    chooseOne you
      [ labeled "$label.passionForJustice" $ createEffect Cards.passionForJustice (CardIdRef $ pcCardId spent) (EventEntity $ EventId $ unCardId $ pcCardId played)
      , labeled "$label.noResponse" (pure ())]

newtype PassionForJusticeEffect = PassionForJusticeEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

passionForJusticeEffect :: CardEffect PassionForJusticeEffect
passionForJusticeEffect = effectWith PassionForJusticeEffect Cards.passionForJustice (modifiersL .~ [ThreatRemovedModifier 1])

instance RunMessage PassionForJusticeEffect where
  runMessage msg e@(PassionForJusticeEffect attrs) = runQueueT $ case msg of
    EventMessage eid ResolvedEvent | effectMatcher attrs == EventEntity eid -> e <$ disableEffect (effectId attrs)
    _ -> PassionForJusticeEffect <$> runMessage msg attrs
