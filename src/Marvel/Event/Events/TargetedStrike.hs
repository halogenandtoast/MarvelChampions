module Marvel.Event.Events.TargetedStrike (targetedStrike, TargetedStrike (..)) where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

targetedStrike :: EventCard TargetedStrike
targetedStrike = event TargetedStrike Cards.targetedStrike

newtype TargetedStrike = TargetedStrike (Attrs Event)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent TargetedStrike where
  eventTargets (TargetedStrike attrs) = do
    confused <- selectAny (IdentityWithId (eventController attrs) <> ConfusedIdentity)
    schemes <- asYou (eventController attrs) $ selectList SchemeYouCanThwart
    pure $ if confused || notNull schemes then Nothing else Just []

instance RunMessage TargetedStrike where
  runMessage msg e@(TargetedStrike attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      schemes <- asYou (eventController attrs) $ selectList SchemeYouCanThwart
      unless (null schemes) $ chooseOne you
        [targeting (schemeRef scheme) (thwartScheme scheme attrs 3) | scheme <- schemes]
      pure e
    _ -> TargetedStrike <$> runMessage msg attrs
