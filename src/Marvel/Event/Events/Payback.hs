module Marvel.Event.Events.Payback (
  payback,
  Payback (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Count
import Marvel.Damage
import Marvel.Matchers
import Marvel.Window qualified as W

payback :: EventCard Payback
payback = event Payback Cards.payback

newtype Payback = Payback (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | After the villain attacks you (whichever of your characters it attacked or a
-- defender, docs/rules/hero-drax.md): deal it damage equal to your ATK.
instance RunMessage Payback where
  runMessage msg e@(Payback attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ (Just (W.EnemyAttack enemy _))) | ident == eventId attrs -> do
      atk <- selectCount HeroAttackDamage (IdentityWithId you)
      dealDamage enemy attrs (toDamage atk $ FromPlayerAttack you)
      pure e
    _ -> Payback <$> runMessage msg attrs
