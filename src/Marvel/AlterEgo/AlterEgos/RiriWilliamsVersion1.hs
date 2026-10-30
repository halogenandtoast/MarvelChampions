module Marvel.AlterEgo.AlterEgos.RiriWilliamsVersion1 where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Resource.Types
import Marvel.Damage
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

ririWilliamsVersion1 :: AlterEgoCard RiriWilliamsVersion1
ririWilliamsVersion1 = alterEgo RiriWilliamsVersion1 Cards.ririWilliamsVersion1 (HP $ Static 10) (HandSize 6) (Rec 3) [Obligations.aMinorSetback]

newtype RiriWilliamsVersion1 = RiriWilliamsVersion1 (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities RiriWilliamsVersion1 where
  getAbilities a =
    [ label "Child Prodigy" $ limitedAbility a 1 (PerRound 1) Action IsSelf cost (runAbility a 1)
    | cost <- [ResourceCost (Just Mental)]
    ]

instance RunMessage RiriWilliamsVersion1 where
  runMessage msg h@(RiriWilliamsVersion1 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      placeCounters you 1
      pure h
    _ -> RiriWilliamsVersion1 <$> runMessage msg attrs
