module Marvel.AlterEgo.AlterEgos.RiriWilliamsVersion2 where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Matchers ()
import Marvel.Question (runAbility)
import Marvel.Resource.Types
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

ririWilliamsVersion2 :: AlterEgoCard RiriWilliamsVersion2
ririWilliamsVersion2 = alterEgo RiriWilliamsVersion2 Cards.ririWilliamsVersion2 (HP $ Static 10) (HandSize 6) (Rec 3) [Obligations.aMinorSetback]

newtype RiriWilliamsVersion2 = RiriWilliamsVersion2 (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities RiriWilliamsVersion2 where
  getAbilities a =
    [ label "Child Prodigy" $ limitedAbility a 2 (PerRound 1) Action IsSelf cost (runAbility a 2)
    | cost <- [ResourceCost (Just Mental), MultiResourceCost [Nothing, Nothing]]
    ]

instance RunMessage RiriWilliamsVersion2 where
  runMessage msg h@(RiriWilliamsVersion2 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 2 _ _ -> do
      placeCounters you 1
      pure h
    _ -> RiriWilliamsVersion2 <$> runMessage msg attrs
