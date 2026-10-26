module Marvel.Event.Events.DoubleTime (
  doubleTime,
  DoubleTime (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Query
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

doubleTime :: EventCard DoubleTime
doubleTime = event DoubleTime Cards.doubleTime

newtype DoubleTime = DoubleTime (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: Choose two of the following (you may choose the same option twice):
-- deal 2 damage to an enemy; remove 2 threat from a scheme. Neither is an attack or a
-- thwart. The second is chosen after the first resolves, an option with nothing to
-- affect not offered.
instance RunMessage DoubleTime where
  runMessage msg e@(DoubleTime attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      option you
      cardSpecific attrs "second" you
      pure e
    CardSpecific (isTarget attrs -> True) "second" (fromJSON -> Success you) -> do
      option you
      pure e
    _ -> DoubleTime <$> runMessage msg attrs
   where
    option you = do
      enemies <- selectAny DamageableEnemy
      schemes <- selectAny ThwartableScheme
      chooseOne you $
        [labeled "$label.deal2DamageToAnEnemy" (chooseDamage you attrs (toDamage 2 FromAbility) DamageableEnemy) | enemies]
          <> [labeled "$label.remove2ThreatFromAScheme" (removeThreat you attrs 2 ThwartableScheme) | schemes]
