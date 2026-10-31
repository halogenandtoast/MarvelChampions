module Marvel.Resource.Resources.MutantGenesis where

import Marvel.Prelude
import Marvel.Id
import Marvel.Card
import Marvel.Effect.Types
import Marvel.Entity
import Marvel.Event.Types qualified as Event
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Resource.Cards qualified as Cards
import Marvel.Trait qualified as Trait

-- These are interrupts to spending a resource to play a matching event, before its effect.
mutantEnergySpent :: (ReverseQueue m) => IdentityId -> PlayerCard -> PlayerCard -> m ()
mutantEnergySpent you played spent = do
  hero <- identityMatches HeroIdentity you
  let def = getCardDef played
      resource = canonicalCardCode (getCardDef spent)
      matches trait = cdCardType def == EventType && trait `member` cdTraits def
  when hero $ case resource of
    "32018" | matches Trait.Defense -> chooseOne you
      [labeled "$label.defensiveEnergy" (drawCards you 1), labeled "$label.noResponse" (pure ())]
    "32047" | matches Trait.Attack -> chooseOne you
      [ labeled "$label.aggressiveEnergy" (createEffect Cards.aggressiveEnergy (CardIdRef $ pcCardId spent) (EventEntity $ EventId $ unCardId $ pcCardId played))
      , labeled "$label.noResponse" (pure ())]
    _ -> pure ()

newtype AggressiveEnergyEffect = AggressiveEnergyEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

aggressiveEnergyEffect :: CardEffect AggressiveEnergyEffect
aggressiveEnergyEffect = effectWith AggressiveEnergyEffect Cards.aggressiveEnergy (modifiersL .~ [DamageDealtModifier 1])

instance RunMessage AggressiveEnergyEffect where
  runMessage msg e@(AggressiveEnergyEffect attrs) = runQueueT $ case msg of
    EventMessage eid ResolvedEvent | effectMatcher attrs == EventEntity eid -> e <$ disableEffect (effectId attrs)
    _ -> AggressiveEnergyEffect <$> runMessage msg attrs
