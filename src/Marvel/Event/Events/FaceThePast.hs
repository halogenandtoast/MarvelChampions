module Marvel.Event.Events.FaceThePast where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource.Types ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window ()
import Marvel.Identity.Types ()

import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

faceThePast :: EventCard FaceThePast
faceThePast = event FaceThePast Cards.faceThePast

newtype FaceThePast = FaceThePast (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage FaceThePast where
  runMessage msg e@(FaceThePast attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      ready you
      drawCards you 3
      lastingModifiers attrs (IdentityEntity $ IdentityWithId you) [CannotAttackVillain] False
      removeFromGame attrs
      pure e
    _ -> FaceThePast <$> runMessage msg attrs
