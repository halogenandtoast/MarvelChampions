module Marvel.Event.Events.Haymaker
  ( Haymaker
  , haymaker
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Attrs
import Marvel.Event.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Target

haymaker :: EventCard Haymaker
haymaker = event Haymaker Cards.haymaker

newtype Haymaker = Haymaker EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage Haymaker where
  runMessage msg e@(Haymaker attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent identityId _ _ -> do
        pushAll =<< choiceMessages
          identityId
          (ChooseDamage (toSource attrs) (toDamage 3 $ FromPlayerAttack identityId) AttackableEnemy)
        pure e
      _ -> Haymaker <$> runMessage msg attrs
    _ -> Haymaker <$> runMessage msg attrs
