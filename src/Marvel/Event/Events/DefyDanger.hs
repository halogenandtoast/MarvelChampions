module Marvel.Event.Events.DefyDanger (
  defyDanger,
  DefyDanger (..),
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
import Marvel.Ref

defyDanger :: EventCard DefyDanger
defyDanger = event DefyDanger Cards.defyDanger

newtype DefyDanger = DefyDanger (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action (attack): 5 damage to an enemy and the top card of the encounter deck
-- discarded; you take 1 damage (not from an attack) for each boost icon on it.
instance RunMessage DefyDanger where
  runMessage msg e@(DefyDanger attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      chooseDamage you attrs (toDamage 5 $ FromPlayerAttack you) AttackableEnemy
      discardTopOfEncounterDeckThen 1 (countBoostIcons attrs ())
      pure e
    WithBoostIcons (isTarget attrs -> True) _ (sum . map snd -> n) -> do
      when (n > 0) $ damageIdentity (eventController attrs) attrs (toDamage n FromAbility)
      pure e
    _ -> DefyDanger <$> runMessage msg attrs
