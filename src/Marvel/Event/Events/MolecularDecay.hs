module Marvel.Event.Events.MolecularDecay (
  molecularDecay,
  MolecularDecay (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

molecularDecay :: EventCard MolecularDecay
molecularDecay = event MolecularDecay Cards.molecularDecay

newtype MolecularDecay = MolecularDecay (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Choose an enemy, discard the top 2 encounter cards and count their boost icons: 5
-- damage and 1 more for each, one instance of damage.
instance RunMessage MolecularDecay where
  runMessage msg e@(MolecularDecay attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      chooseEnemy you AttackableEnemy attrs
      pure e
    ChoseEnemy enemy (isTarget attrs -> True) -> do
      discardTopOfEncounterDeckThen 2 (countBoostIcons attrs enemy)
      pure e
    WithBoostIcons (isTarget attrs -> True) (fromJSON -> Success (enemy :: EnemyId)) (sum . map snd -> n) -> do
      dealDamage enemy attrs (toDamage (5 + n) $ FromPlayerAttack (eventController attrs))
      pure e
    _ -> MolecularDecay <$> runMessage msg attrs
