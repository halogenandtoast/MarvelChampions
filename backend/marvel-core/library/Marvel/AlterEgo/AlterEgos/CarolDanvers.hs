module Marvel.AlterEgo.AlterEgos.CarolDanvers where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Question
import Marvel.Source
import Marvel.Stats

carolDanvers :: AlterEgoCard CarolDanvers
carolDanvers = alterEgo
  CarolDanvers
  Cards.carolDanvers
  (HP $ Static 12)
  (HandSize 6)
  (Rec 4)
  [Cards.familyEmergency]

newtype CarolDanvers = CarolDanvers AlterEgoAttrs
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, IsSource)

instance HasAbilities CarolDanvers where
  getAbilities a =
    [ label "Commander" $ limitedAbility
        a
        1
        (PerRound 1)
        Action
        IsSelf
        NoCost
        (ChooseDrawCards 1 AnyIdentity)
    ]

instance RunMessage CarolDanvers where
  runMessage _ = pure
