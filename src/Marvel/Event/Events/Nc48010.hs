module Marvel.Event.Events.Nc48010 where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Boost ()
import Marvel.Phase ()
import Marvel.Ally.Types ()
import Marvel.Minion.Types ()
import Marvel.Nightcrawler.Helpers (legalOtherForms)
import Marvel.Minion.Cards ()
import Marvel.SideScheme.Cards ()
import Marvel.Hand ()
import Marvel.Id (CharacterId (IdentityCharacter))
import Marvel.Keyword ()
import Marvel.Matchers (CharacterMatcher (CharacterWithId, CharacterCanBeReadied))
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query (selectAny)
import Marvel.Question (Choice (ChangeToForm))
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window ()
import Marvel.Event.Types
import Marvel.Event.Cards qualified as Cards

nc48010 :: EventCard Nc48010
nc48010 = event Nc48010 Cards.nc48010

newtype Nc48010 = Nc48010 (Attrs Event)
  deriving anyclass HasModifiersFor
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent Nc48010 where
  eventTargets (Nc48010 attrs) = do
    let you = eventController attrs
    forms <- legalOtherForms you
    canReady <- selectAny (CharacterWithId (IdentityCharacter you) <> CharacterCanBeReadied)
    pure $ if not (null forms) || canReady then Nothing else Just []

instance RunMessage Nc48010 where
  runMessage msg a@(Nc48010 attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      forms <- legalOtherForms you
      unless (null forms) $ chooseOrRunOne you [pure $ ChangeToForm side | side <- forms]
      ready you
      pure a
    _ -> Nc48010 <$> runMessage msg attrs
