module Marvel.Event.Events.SideBySide where

import Marvel.Prelude
import Marvel.Game.Source ()
import Marvel.Matchers
import Marvel.Payment
import Marvel.Question ()
import Marvel.Upgrade.Cards ()
import Marvel.Ability ()
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

sideBySide :: EventCard SideBySide
sideBySide = event SideBySide Cards.sideBySide

newtype SideBySide = SideBySide (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)



instance RunMessage SideBySide where
  runMessage msg a@(SideBySide attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you payment _) | eid == eventId attrs -> do
      let sidekicks = [aid | ReadiedAllyPayment aid <- payments payment]
      ready you
      chooseOne you
        [labeled "$label.sideBySideHeal" $ do
          heal you 1
          for_ sidekicks $ \aid -> heal aid 1
        , labeled "$label.sideBySideBoost" $ do
          createEffect Cards.leadFromTheFront attrs (IdentityEntity $ IdentityWithId you)
          for_ sidekicks $ \aid -> createEffect Cards.leadFromTheFront attrs (AllyEntity $ AllyWithId aid)
        ]
      pure a
    _ -> SideBySide <$> runMessage msg attrs
