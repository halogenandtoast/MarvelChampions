module Marvel.Event.Events.PsiFlailStrike where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Ability.Types qualified as Ability ()
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

psiFlailStrike :: EventCard PsiFlailStrike
psiFlailStrike = event PsiFlailStrike Cards.psiFlailStrike

newtype PsiFlailStrike = PsiFlailStrike (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage PsiFlailStrike where
  runMessage msg e@(PsiFlailStrike a) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ (Just (W.HeroDefends _ enemy))) | eid == eventId a -> do
      dealDamage enemy a (toDamage 3 $ FromPlayerAttack you)
      stun enemy a
      pure e
    _ -> PsiFlailStrike <$> runMessage msg a
