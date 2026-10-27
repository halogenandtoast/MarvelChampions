module Marvel.Event.Events.ContingencyPlan (
  contingencyPlan,
  ContingencyPlan (..),
) where

import Marvel.Prelude

import Data.List (nub)
import Marvel.Card
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

contingencyPlan :: EventCard ContingencyPlan
contingencyPlan = event ContingencyPlan Cards.contingencyPlan

newtype ContingencyPlan = ContingencyPlan (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action (attack): discard the top 4 cards of your deck; for each different
-- printed resource type among them (energy, mental, physical, wild) 1 damage to an
-- enemy, each its own step (an enemy one defeats is not offered to the next). Then draw
-- 1 card.
instance RunMessage ContingencyPlan where
  runMessage msg e@(ContingencyPlan attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      discardFromDeck you 4 attrs
      pure e
    WithDiscarded (isTarget attrs -> True) _ (onlyPlayerCards -> cards) -> do
      let types = nub $ concatMap (printedResources . getCardDef) cards
      for_ types \_ -> cardSpecific attrs "damage" ()
      drawCards (eventController attrs) 1
      pure e
    CardSpecific (isTarget attrs -> True) "damage" _ -> do
      let you = eventController attrs
      chooseDamage you attrs (toDamage 1 $ FromPlayerAttack you) AttackableEnemy
      pure e
    _ -> ContingencyPlan <$> runMessage msg attrs
