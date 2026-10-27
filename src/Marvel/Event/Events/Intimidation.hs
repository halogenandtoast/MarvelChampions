module Marvel.Event.Events.Intimidation (
  intimidation,
  Intimidation (..),
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

intimidation :: EventCard Intimidation
intimidation = event Intimidation Cards.intimidation

newtype Intimidation = Intimidation (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Remove threat from a scheme equal to your ATK.
instance RunMessage Intimidation where
  runMessage msg e@(Intimidation attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      atk <- selectCount HeroAttackDamage (IdentityWithId you)
      removeThreat you attrs atk ThwartableScheme
      pure e
    _ -> Intimidation <$> runMessage msg attrs
