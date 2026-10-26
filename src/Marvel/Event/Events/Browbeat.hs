module Marvel.Event.Events.Browbeat (
  browbeat,
  Browbeat (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source (getVillainStage)
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

browbeat :: EventCard Browbeat
browbeat = event Browbeat Cards.browbeat

newtype Browbeat = Browbeat (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action (attack), Avenger only (its criteria): 2 damage to the villain and X
-- more (at most 3), X his stage: one instance of 2 + X.
instance RunMessage Browbeat where
  runMessage msg e@(Browbeat attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      stage <- maybe (pure 0) getVillainStage =<< selectOne ActiveVillain
      chooseDamage you attrs (toDamage (2 + min 3 stage) $ FromPlayerAttack you) (VillainEnemy <> AttackableEnemy)
      pure e
    _ -> Browbeat <$> runMessage msg attrs
