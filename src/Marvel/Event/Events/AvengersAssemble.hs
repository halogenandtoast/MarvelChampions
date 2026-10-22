module Marvel.Event.Events.AvengersAssemble (
  avengersAssemble,
  avengersAssembleEffect,
  AvengersAssembleEffect (..),
  AvengersAssemble (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Effect.Types
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Trait

avengersAssemble :: EventCard AvengersAssemble
avengersAssemble = event AvengersAssemble Cards.avengersAssemble

newtype AvengersAssemble = AvengersAssemble (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | The player's Avenger characters are readied, then every Avenger character in play,
-- whoever's, gets +1 THW and +1 ATK for the phase.
instance RunMessage AvengersAssemble where
  runMessage msg e@(AvengersAssemble attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      identities <- selectList $ IdentityWithId identityId <> IdentityWithTrait Avenger
      allies <- selectList $ AllyMatches [AllyControlledBy (IdentityWithId identityId), AllyWithTrait Avenger]
      for_ identities ready
      for_ allies ready
      for_ [IdentityEntity (IdentityWithTrait Avenger), AllyEntity (AllyWithTrait Avenger)] $
        createEffect Cards.avengersAssemble e
      pure e
    _ -> AvengersAssemble <$> runMessage msg attrs

newtype AvengersAssembleEffect = AvengersAssembleEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

avengersAssembleEffect :: CardEffect AvengersAssembleEffect
avengersAssembleEffect =
  effectWith AvengersAssembleEffect Cards.avengersAssemble $
    (modifiersL .~ [ThwartModifier 1, AttackModifier 1]) . (endsL ?~ DisableAtEndOfPhase)

instance RunMessage AvengersAssembleEffect where
  runMessage msg (AvengersAssembleEffect attrs) = AvengersAssembleEffect <$> runMessage msg attrs
