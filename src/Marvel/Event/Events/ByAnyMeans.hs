module Marvel.Event.Events.ByAnyMeans (
  byAnyMeans,
  ByAnyMeans (..),
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
import Marvel.Query
import Marvel.Ref

byAnyMeans :: EventCard ByAnyMeans
byAnyMeans = event ByAnyMeans Cards.byAnyMeans

newtype ByAnyMeans = ByAnyMeans (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action (attack): 2 threat on the main scheme, 3 damage to the villain, draw 1.
instance RunMessage ByAnyMeans where
  runMessage msg e@(ByAnyMeans attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      -- of two main schemes, the one you choose (Tower Defense, MTS rulebook p.10)
      schemes <- selectList MainScheme
      unless (null schemes) $ chooseOrRunOne you [targeting (schemeRef s) (placeThreat s 2) | s <- schemes]
      chooseDamage you attrs (toDamage 3 $ FromPlayerAttack you) (VillainEnemy <> AttackableEnemy)
      drawCards you 1
      pure e
    _ -> ByAnyMeans <$> runMessage msg attrs
