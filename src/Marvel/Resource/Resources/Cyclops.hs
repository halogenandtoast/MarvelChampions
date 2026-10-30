module Marvel.Resource.Resources.Cyclops where

import Marvel.Prelude
import Marvel.Card
import Marvel.Effect.Types
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Resource.Cards qualified as Cards

-- An optional interrupt, resolved before the ally enters play so its entry
-- responses see the bonus. Each physical resource creates its own lasting effect.
cyclopsResourceSpent :: ReverseQueue m => IdentityId -> PlayerCard -> PlayerCard -> m ()
cyclopsResourceSpent you played spent =
  when (getCardDef spent `sameCard` Cards.effectiveLeadership && cdCardType (getCardDef played) == AllyType) $
    chooseOne you
      [ labeled "$label.effectiveLeadership" $ createEffect Cards.effectiveLeadership
          (CardIdRef $ pcCardId spent) (AllyEntity $ AllyWithId $ AllyId $ unCardId $ pcCardId played)
      , labeled "$label.noResponse" (pure ())
      ]

newtype EffectiveLeadershipEffect = EffectiveLeadershipEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

effectiveLeadershipEffect :: CardEffect EffectiveLeadershipEffect
effectiveLeadershipEffect = effectWith EffectiveLeadershipEffect Cards.effectiveLeadership
  ((modifiersL .~ [ThwartModifier 1, AttackModifier 1]) . (endsL ?~ DisableAtEndOfPhase))

instance RunMessage EffectiveLeadershipEffect where
  runMessage msg (EffectiveLeadershipEffect attrs) = EffectiveLeadershipEffect <$> runMessage msg attrs
