module Marvel.Event.Events.Uppercut
  ( Uppercut
  , uppercut
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
import Marvel.Query
import Marvel.Question
import Marvel.Source
import Marvel.Target

uppercut :: EventCard Uppercut
uppercut = event Uppercut Cards.uppercut

newtype Uppercut = Uppercut EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage Uppercut where
  runMessage msg e@(Uppercut attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent identityId _ _ -> do
        enemies <- selectList AttackableEnemy
        chooseOne identityId $ map (damageChoice attrs (toDamage 5 FromAttack)) enemies
        pure e
      _ -> Uppercut <$> runMessage msg attrs
    _ -> Uppercut <$> runMessage msg attrs
