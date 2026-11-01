module Marvel.Event.Events.PhotonBeam where

import Marvel.Prelude
import Marvel.Ability ()
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
import Marvel.Window qualified as W

photonBeam :: EventCard PhotonBeam
photonBeam = event PhotonBeam Cards.photonBeam

newtype PhotonBeam = PhotonBeam (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage PhotonBeam where
  runMessage msg e@(PhotonBeam attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      chooseOne you [targeting (enemyRef enemy) do
        dealDamageThen enemy attrs (toDamage 4 $ FromPlayerAttack you) (cardSpecific attrs "progress" (1 :: Natural))
        | enemy <- enemies]
      pure e
    CheckWindows windows -> do
      for_ [() | W.Window W.After window <- windows, source <- defeatedBy window, source == toSource attrs] $ \_ ->
        insteadOf pending $ cardSpecific attrs "progress" (2 :: Natural)
      PhotonBeam <$> runMessage msg attrs
    CardSpecific (isTarget attrs -> True) "progress" (fromJSON -> Success (n :: Natural)) ->
      e <$ placeCounters (eventController attrs) n
    _ -> PhotonBeam <$> runMessage msg attrs

   where
    defeatedBy = \case
      W.DefeatedMinion _ _ source _ -> [source]
      W.DefeatedVillain _ _ source _ -> [source]
      _ -> []
    pending = \case
      CardSpecific (isTarget attrs -> True) "progress" _ -> True
      _ -> False
