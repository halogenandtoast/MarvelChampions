module Marvel.Event.Events.Uppercut (
  Uppercut,
  uppercut,
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
import Marvel.Query
import Marvel.Ref

uppercut :: EventCard Uppercut
uppercut = event Uppercut Cards.uppercut

newtype Uppercut = Uppercut (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Uppercut where
  runMessage msg e@(Uppercut attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        enemies <- selectList AttackableEnemy
        chooseOne identityId $
          map
            (damageChoice attrs (toDamage 5 $ FromPlayerAttack identityId))
            enemies
        pure e
      _ -> Uppercut <$> runMessage msg attrs
    _ -> Uppercut <$> runMessage msg attrs
