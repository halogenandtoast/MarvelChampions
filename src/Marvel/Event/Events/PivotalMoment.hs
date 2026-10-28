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
import Marvel.Query (selectList)
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
      -- of two main schemes, the one you choose: one with no threat, if there is one
      -- (Tower Defense, MTS rulebook p.10)
      schemes <- selectList MainScheme
      clear <- anyM (fmap (== 0) . selectCount SchemeThreat . SchemeWithId) schemes
      chooseDamage you attrs (toDamage (if null schemes || clear then 5 else 2) $ FromPlayerAttack you) (VillainEnemy <> AttackableEnemy)
      pure e
    _ -> PivotalMoment <$> runMessage msg attrs
