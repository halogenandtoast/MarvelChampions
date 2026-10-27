module Marvel.Event.Events.IAmGrootExclaim (
  iAmGrootExclaim,
  IAmGrootExclaim (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Damage
import Marvel.Identity.Types (Field (PlayerIdentityCounters))
import Marvel.Matchers
import Marvel.Projection

iAmGrootExclaim :: EventCard IAmGrootExclaim
iAmGrootExclaim = event IAmGrootExclaim Cards.iAmGrootExclaim

newtype IAmGrootExclaim = IAmGrootExclaim (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action (attack): deal damage to an enemy equal to Groot's growth counters.
instance RunMessage IAmGrootExclaim where
  runMessage msg e@(IAmGrootExclaim attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      n <- project PlayerIdentityCounters you
      chooseDamage you attrs (toDamage n $ FromPlayerAttack you) AttackableEnemy
      pure e
    _ -> IAmGrootExclaim <$> runMessage msg attrs
