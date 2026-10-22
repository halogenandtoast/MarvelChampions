module Marvel.Event.Events.CrimsonBandsOfCyttorak (
  crimsonBandsOfCyttorak,
  CrimsonBandsOfCyttorak (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Invocation
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

crimsonBandsOfCyttorak :: EventCard CrimsonBandsOfCyttorak
crimsonBandsOfCyttorak = event CrimsonBandsOfCyttorak Cards.crimsonBandsOfCyttorak

newtype CrimsonBandsOfCyttorak = CrimsonBandsOfCyttorak (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Special: stun an enemy and deal 7 damage to it.
instance RunMessage CrimsonBandsOfCyttorak where
  runMessage msg e@(CrimsonBandsOfCyttorak attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        chooseEnemy identityId AnyEnemy attrs
        pure e
      ResolvedEvent -> e <$ invocationResolved attrs
    ChoseEnemy enemy (isTarget attrs -> True) -> do
      stun enemy attrs
      dealDamage enemy attrs (toDamage 7 FromAbility)
      pure e
    _ -> CrimsonBandsOfCyttorak <$> runMessage msg attrs
