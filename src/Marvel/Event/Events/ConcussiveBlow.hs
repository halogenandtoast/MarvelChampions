module Marvel.Event.Events.ConcussiveBlow (
  concussiveBlow,
  ConcussiveBlow (..),
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

concussiveBlow :: EventCard ConcussiveBlow
concussiveBlow = event ConcussiveBlow Cards.concussiveBlow

newtype ConcussiveBlow = ConcussiveBlow (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage ConcussiveBlow where
  runMessage msg e@(ConcussiveBlow attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId payment _ -> do
        physical <- paidWith Physical payment
        enemies <- selectList AttackableEnemy
        chooseOne identityId
          [ TargetLabel (toRef enemy) $
              Confuse (toRef enemy) (toSource attrs)
                : [ DamageEnemy (toRef enemy) (toSource attrs) (toDamage 3 $ FromPlayerAttack identityId)
                  | physical
                  ]
          | enemy <- enemies
          ]
        pure e
      _ -> ConcussiveBlow <$> runMessage msg attrs
    _ -> ConcussiveBlow <$> runMessage msg attrs
