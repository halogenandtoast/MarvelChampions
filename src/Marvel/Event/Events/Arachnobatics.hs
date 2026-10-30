module Marvel.Event.Events.Arachnobatics where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Damage
import Marvel.Matchers
import Marvel.Query
import Marvel.Status

arachnobatics :: EventCard Arachnobatics
arachnobatics = event Arachnobatics Cards.arachnobatics

newtype Arachnobatics = Arachnobatics (Attrs Event)
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Arachnobatics where
  runMessage msg e@(Arachnobatics attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      chooseOne you
        [ targeting (enemyRef enemy) do
            stunned <- elem (toCharacter enemy) <$> selectList (CharacterWithStatus StunnedStatus)
            confused <- elem (toCharacter enemy) <$> selectList (CharacterWithStatus ConfusedStatus)
            let damage = 2 + (if stunned then 3 else 0) + (if confused then 3 else 0)
            dealDamage enemy attrs (toDamage damage $ FromPlayerAttack you)
        | enemy <- enemies ]
      pure e
    _ -> Arachnobatics <$> runMessage msg attrs
