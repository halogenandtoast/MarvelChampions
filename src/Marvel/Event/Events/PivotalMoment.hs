module Marvel.Event.Events.PivotalMoment (
  pivotalMoment,
  PivotalMoment (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Count
import Marvel.Ref

pivotalMoment :: EventCard PivotalMoment
pivotalMoment = event PivotalMoment Cards.pivotalMoment

newtype PivotalMoment = PivotalMoment (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Deal 2 damage to the villain (5 damage instead if there is no threat on the main
-- scheme).
instance RunMessage PivotalMoment where
  runMessage msg e@(PivotalMoment attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      threat <- selectCount SchemeThreat MainScheme
      chooseDamage you attrs (toDamage (if threat == 0 then 5 else 2) $ FromPlayerAttack you) (VillainEnemy <> AttackableEnemy)
      pure e
    _ -> PivotalMoment <$> runMessage msg attrs
