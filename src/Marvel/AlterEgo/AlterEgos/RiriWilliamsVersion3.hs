module Marvel.AlterEgo.AlterEgos.RiriWilliamsVersion3 where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Matchers ()
import Marvel.Question (runAbility)
import Marvel.Resource.Types ()
import Marvel.Damage ()
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats
import Marvel.Obligation.Cards qualified as Obligations

ririWilliamsVersion3 :: AlterEgoCard RiriWilliamsVersion3
ririWilliamsVersion3 = alterEgo RiriWilliamsVersion3 Cards.ririWilliamsVersion3 (HP $ Static 10) (HandSize 6) (Rec 3) [Obligations.aMinorSetback]

newtype RiriWilliamsVersion3 = RiriWilliamsVersion3 (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities RiriWilliamsVersion3 where
  getAbilities a =
    [ label "Child Prodigy" $ limitedAbility a 3 (PerRound 1) Action IsSelf cost (runAbility a 3)
    | cost <- [ResourceCost Nothing]
    ]

instance RunMessage RiriWilliamsVersion3 where
  runMessage msg h@(RiriWilliamsVersion3 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 3 _ _ -> do
      placeCounters you 1
      pure h
    _ -> RiriWilliamsVersion3 <$> runMessage msg attrs
