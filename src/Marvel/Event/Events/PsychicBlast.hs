module Marvel.Event.Events.PsychicBlast where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W ()
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

psychicBlast :: EventCard PsychicBlast
psychicBlast = event PsychicBlast Cards.psychicBlast

newtype PsychicBlast = PsychicBlast (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage PsychicBlast where
  runMessage msg e@(PsychicBlast a) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId a -> do
      withTheVillain you $ \vid -> dealDamage vid a (toDamage 4 FromAbility)
      cardSpecific a "blastMinions" you
      pure e
    CardSpecific (isTarget a -> True) "blastMinions" (fromJSON -> Success you) -> do
      unleashed <- identityMatches (IdentityWithTrait Trait.Unleashed) you
      when unleashed do
        minions <- selectList (MinionEngagedWith $ IdentityWithId you)
        dealDamageToEach minions a (toDamage 4 FromAbility)
      pure e
    _ -> PsychicBlast <$> runMessage msg a
