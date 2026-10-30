module Marvel.Event.Events.Repurpose where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Card
import Marvel.Effect.Types
import Marvel.Matchers
import Marvel.Payment

repurpose :: EventCard Repurpose
repurpose = event Repurpose Cards.repurpose

newtype Repurpose = Repurpose (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Repurpose where
  runMessage msg e@(Repurpose attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you payment _) | ident == eventId attrs -> do
      let n = fromIntegral $ sum [fromMaybe 0 (cdCost $ getCardDef card) | card <- paymentDiscardedUpgrades payment]
      ready you
      chooseOne you
        [labeled key do
          createEffect Cards.repurpose attrs (IdentityEntity $ IdentityWithId you)
          cardSpecific attrs "repurposePower" (modifier n)
        | (key, modifier) <- [("$label.repurposeThwart", ThwartModifier),
                             ("$label.repurposeAttack", AttackModifier),
                             ("$label.repurposeDefense", DefenseModifier)]]
      pure e
    _ -> Repurpose <$> runMessage msg attrs

newtype RepurposeEffect = RepurposeEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

repurposeEffect :: CardEffect RepurposeEffect
repurposeEffect = effectWith RepurposeEffect Cards.repurpose (endsL ?~ DisableAtEndOfRound)

instance RunMessage RepurposeEffect where
  runMessage msg e@(RepurposeEffect attrs) = runQueueT $ case msg of
    CardSpecific target "repurposePower" (fromJSON -> Success modifier) | toSource target == effectSource attrs, null (effectModifiers attrs) ->
      pure $ RepurposeEffect $ attrs {effectModifiers = [modifier]}
    _ -> RepurposeEffect <$> runMessage msg attrs
