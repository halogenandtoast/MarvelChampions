module Marvel.AlterEgo.AlterEgos.PeniParker where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Types
import Marvel.GameValue
import Marvel.Hand
import Marvel.Stats
import Marvel.AlterEgo.Runner
import Marvel.Obligation.Cards qualified as Obligations

peniParker :: AlterEgoCard PeniParker
peniParker = alterEgo PeniParker Cards.peniParker (HP $ Static 14) (HandSize 4) (Rec 4) [Obligations.inheritedBurden]

newtype PeniParker = PeniParker (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- The engine performs Psychogenetic Compatibility and both linked flips atomically.
-- Maintenance exhausts the other physical card, independently of Peni's REC.
instance HasAbilities PeniParker where
  getAbilities a =
    [ label "$label.spdrMaintenance" $ ability a 1 AlterEgoAction IsSelf
        (ExhaustSupportCost $ SupportNamed "SP//dr Suit") (runAbility a 1)
    ]

instance RunMessage PeniParker where
  runMessage msg p@(PeniParker attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> p <$ drawCards you 2
    _ -> PeniParker <$> runMessage msg attrs
