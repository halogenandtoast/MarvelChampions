module Marvel.Event.Events.Metamorphosis where

import Marvel.Prelude
import Marvel.Ability qualified as Ability ()
import Marvel.Card
import Marvel.Card.Side ()
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source ()
import Marvel.Identity.Types (Field (PlayerIdentitySide))
import Marvel.Projection
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Ref
import Marvel.Window qualified as W ()

metamorphosis :: EventCard Metamorphosis
metamorphosis = event Metamorphosis Cards.metamorphosis

newtype Metamorphosis = Metamorphosis (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Metamorphosis where
  runMessage msg e@(Metamorphosis attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      before <- project PlayerIdentitySide you
      identityStep you ChooseOtherForm
      cardSpecific attrs "afterChange" (you, before)
      pure e
    CardSpecific (isTarget attrs -> True) "afterChange" (fromJSON -> Success (you, before :: Side)) -> do
      after <- project PlayerIdentitySide you
      when (after /= before) $ do
        warren <- identityMatches (IdentityWithTitle "Warren Worthington III") you
        angel <- identityMatches (IdentityWithTitle "Angel") you
        archangel <- identityMatches (IdentityWithTitle "Archangel") you
        when warren $ drawCards you 1
        when angel $ removeThreat you attrs 2 ThwartableScheme
        when archangel $ chooseDamage you attrs (toDamage 3 FromAbility) AnyEnemy
      pure e
    _ -> Metamorphosis <$> runMessage msg attrs
