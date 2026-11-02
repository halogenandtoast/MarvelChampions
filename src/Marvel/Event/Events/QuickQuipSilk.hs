module Marvel.Event.Events.QuickQuipSilk where
import Marvel.Prelude
import Marvel.Id
import Marvel.Query
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Matchers
import Marvel.Question ()
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers ()

import Marvel.Ability ()
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.Event.Types

quickQuipSilk :: EventCard QuickQuipSilk
quickQuipSilk = event QuickQuipSilk Cards.quickQuipSilk
newtype QuickQuipSilk = QuickQuipSilk (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage QuickQuipSilk where
  runMessage msg u@(QuickQuipSilk attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      enemies <- selectList EnemyCanBeConfused
      chooseOne you [targeting (enemyRef enemy) (confuse enemy attrs >> cardSpecific attrs "second" you) | enemy <- enemies]
      pure u
    CardSpecific (isTarget attrs -> True) "second" (fromJSON -> Success (you :: IdentityId)) -> do
      enemies <- selectList EnemyCanBeConfused
      chooseOne you [targeting (enemyRef enemy) (confuse enemy attrs) | enemy <- enemies]
      pure u
    _ -> QuickQuipSilk <$> runMessage msg attrs
