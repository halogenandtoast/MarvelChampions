module Marvel.AlterEgo.AlterEgos.PeterParker where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Message
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Payment
import Marvel.Question
import Marvel.Resource
import Marvel.Source
import Marvel.Stats

peterParker :: AlterEgoCard PeterParker
peterParker = alterEgo
  PeterParker
  Cards.peterParker
  (HP $ Static 10)
  (HandSize 6)
  (Rec 3)
  [Cards.evictionNotice]

newtype PeterParker = PeterParker AlterEgoAttrs
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsSource, Entity)

instance HasAbilities PeterParker where
  getAbilities a =
    [ label "Scientist" $ limitedAbility
        a
        1
        (PerRound 1)
        Resource
        IsSelf
        NoCost
        (Pay $ ResourcePayment Mental)
    ]

instance RunMessage PeterParker where
  runMessage msg a = PeterParker <$> runMessage msg (toAttrs a)
