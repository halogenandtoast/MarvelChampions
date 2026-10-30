module Marvel.Event.Events.TeamStrike where

import Marvel.Prelude
import Marvel.Card.Side
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
import Marvel.Count
import Marvel.Payment

teamStrike :: EventCard TeamStrike
teamStrike = event TeamStrike Cards.teamStrike

newtype TeamStrike = TeamStrike (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage TeamStrike where
  runMessage msg e@(TeamStrike attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you payment _) | eid == eventId attrs -> do
      hero <- selectCount HeroAttackDamage (IdentityWithId you)
      allies <- traverse (selectCount AllyAttackValue . AllyWithId) (paymentExhaustedAllies payment)
      dealDividedDamage you attrs (toDamage (hero + sum allies) $ FromPlayerAttack you) AttackableEnemy
      pure e
    _ -> TeamStrike <$> runMessage msg attrs
