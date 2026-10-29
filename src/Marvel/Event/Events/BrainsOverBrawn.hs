module Marvel.Event.Events.BrainsOverBrawn (brainsOverBrawn, BrainsOverBrawn (..)) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Count
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

brainsOverBrawn :: EventCard BrainsOverBrawn
brainsOverBrawn = event BrainsOverBrawn Cards.brainsOverBrawn

newtype BrainsOverBrawn = BrainsOverBrawn (Attrs Event)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent BrainsOverBrawn where
  eventTargets (BrainsOverBrawn attrs) = do
    let ident = eventController attrs
    thw <- selectCount HeroThwartValue (IdentityWithId ident)
    stunned <- selectAny (IdentityWithId ident <> StunnedIdentity)
    pure $ if thw > 0 || stunned then Nothing else Just []

instance RunMessage BrainsOverBrawn where
  runMessage msg a@(BrainsOverBrawn attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent ident _ _) | eid == eventId attrs -> do
      thw <- selectCount HeroThwartValue (IdentityWithId ident)
      enemies <- selectList (AttackableBy $ IdentityWithId ident)
      unless (null enemies) $ chooseOne ident
        [targeting (enemyRef enemy) (dealDamage enemy attrs $ toDamage thw $ FromPlayerAttack ident) | enemy <- enemies]
      pure a
    _ -> BrainsOverBrawn <$> runMessage msg attrs
