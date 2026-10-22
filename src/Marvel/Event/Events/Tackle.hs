module Marvel.Event.Events.Tackle (
  tackle,
  Tackle (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Choice
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource

tackle :: EventCard Tackle
tackle = event Tackle Cards.tackle

newtype Tackle = Tackle (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Tackle where
  runMessage msg e@(Tackle attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId payment _ -> do
        physical <- paidWith Physical payment
        enemies <- selectList AttackableEnemy
        chooseOne identityId
          [ TargetLabel (toRef enemy) $
              Stun (toRef enemy) (toSource attrs)
                : [ DamageEnemy (toRef enemy) (toSource attrs) (toDamage 3 $ FromPlayerAttack identityId)
                  | physical
                  ]
          | enemy <- enemies
          ]
        pure e
      _ -> Tackle <$> runMessage msg attrs
    _ -> Tackle <$> runMessage msg attrs
