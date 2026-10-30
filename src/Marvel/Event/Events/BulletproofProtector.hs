module Marvel.Event.Events.BulletproofProtector where

import Marvel.Prelude
import Marvel.Card.Side
import Marvel.Payment
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Stats
import Marvel.Status
import Marvel.Id
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Matchers qualified as Matcher
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

bulletproofProtector :: EventCard BulletproofProtector
bulletproofProtector = event BulletproofProtector Cards.bulletproofProtector

newtype BulletproofProtector = BulletproofProtector (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage BulletproofProtector where
  runMessage msg e@(BulletproofProtector attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      canReady <- identityMatches Matcher.ExhaustedIdentity you
      chooseOne you $
        [ labeled "$label.bulletproofProtectorTough" (becomeTough you >> becomeTough you)
        ] <> [labeled "$label.ready" (ready you) | canReady]
      pure e
    _ -> BulletproofProtector <$> runMessage msg attrs
