module Marvel.Event.Events.ElectromagneticBlast where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Card
import Marvel.Count (Count(selectCount))
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
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource.Types ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window qualified as W
import Marvel.Identity.Types ()

import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Attachment.Types ()
electromagneticBlast :: EventCard ElectromagneticBlast
electromagneticBlast = event (ElectromagneticBlast . (`With` False)) Cards.electromagneticBlast

newtype ElectromagneticBlast = ElectromagneticBlast (Attrs Event `With` Bool)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent ElectromagneticBlast where
  toEventAttrs (ElectromagneticBlast (attrs `With` _)) = attrs

instance RunMessage ElectromagneticBlast where
  runMessage msg e@(ElectromagneticBlast (attrs `With` removedLast)) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      schemes <- selectList ThwartableScheme
      chooseOne you [targeting (schemeRef scheme) $ thwartSchemeAs you scheme attrs 3 >> cardSpecific attrs "blasted" scheme | scheme <- schemes]
      pure e
    CheckWindows windows -> do
      let removed = any (\case W.Window W.After (W.ThreatRemovedBy source _ before amount) -> source == toSource attrs && amount > 0 && amount == before; _ -> False) windows
      pure $ ElectromagneticBlast (attrs `With` (removedLast || removed))
    CardSpecific (isTarget attrs -> True) "blasted" (fromJSON -> Success scheme) -> do
      n <- selectCount SchemeThreat (SchemeWithId scheme)
      when (removedLast && n == 0) do
        legal <- selectList AttachmentWithPrintedHeroActionOrResponse
        chooseOne (eventController attrs) $ labeled "$label.done" (pure ()) : [targeting aid $ removeFromPlay aid | aid <- legal]
      pure e
    _ -> (\a -> ElectromagneticBlast (a `With` removedLast)) <$> runMessage msg attrs
