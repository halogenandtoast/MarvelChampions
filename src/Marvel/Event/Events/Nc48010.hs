module Marvel.Event.Events.Nc48010 where

import Marvel.Prelude
import Data.HashMap.Strict qualified as HashMap
import Marvel.Identity.Types qualified as P
import Marvel.Ability ()
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source (getModifiers)
import Marvel.GameValue ()
import Marvel.Boost ()
import Marvel.Phase ()
import Marvel.Ally.Types ()
import Marvel.Minion.Types ()
import Marvel.Nightcrawler.Helpers ()
import Marvel.Minion.Cards ()
import Marvel.SideScheme.Cards ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection
import Marvel.Query ()
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
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Nc48010 where
  runMessage msg a@(Nc48010 attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      current <- project P.PlayerIdentitySide you
      sides <- project P.PlayerIdentitySides you
      modifiers <- getModifiers you
      let legal = [side | (side, face) <- HashMap.toList sides, side /= current, CannotChangeForm `notElem` modifiers,
            case face of P.HeroSide _ -> CannotChangeToHeroForm `notElem` modifiers; _ -> True]
      chooseOrRunOne you [pure $ ChangeToForm side | side <- legal]
      ready you
      pure a
    _ -> Nc48010 <$> runMessage msg attrs
