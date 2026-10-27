module Marvel.Event.Events.Parry (
  parry,
  Parry (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Count
import Marvel.Matchers

parry :: EventCard Parry
parry = event Parry Cards.parry

newtype Parry = Parry (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When you would take damage: prevent double your ATK of it.
instance RunMessage Parry where
  runMessage msg e@(Parry attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      atk <- selectCount HeroAttackDamage (IdentityWithId you)
      preventDamage you (2 * atk)
      pure e
    _ -> Parry <$> runMessage msg attrs
