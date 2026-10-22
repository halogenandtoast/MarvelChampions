module Marvel.Event.Events.CrimsonBandsOfCyttorak (
  crimsonBandsOfCyttorak,
  CrimsonBandsOfCyttorak (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Choice
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Invocation
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Ref

crimsonBandsOfCyttorak :: EventCard CrimsonBandsOfCyttorak
crimsonBandsOfCyttorak = event CrimsonBandsOfCyttorak Cards.crimsonBandsOfCyttorak

newtype CrimsonBandsOfCyttorak = CrimsonBandsOfCyttorak (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Special: stun an enemy and deal 7 damage to it.
instance RunMessage CrimsonBandsOfCyttorak where
  runMessage msg e@(CrimsonBandsOfCyttorak attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        pushChoice identityId $ ChooseEnemy AnyEnemy (toTarget attrs)
        pure e
      ResolvedEvent -> e <$ push (invocationResolved attrs)
    ChoseEnemy enemy (isTarget attrs -> True) -> do
      let ident = eventController attrs
      stun <- choiceMessages ident $ Stun (toRef enemy) (toSource attrs)
      damage <- choiceMessages ident $ DamageEnemy (toRef enemy) (toSource attrs) (toDamage 7 FromAbility)
      pushAll $ stun <> damage
      pure e
    _ -> CrimsonBandsOfCyttorak <$> runMessage msg attrs
