module Marvel.Ally.Allies.Agent13SharonCarter where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Window

agent13SharonCarter :: AllyCard Agent13SharonCarter
agent13SharonCarter = ally Agent13SharonCarter Cards.agent13SharonCarter (Thw 2, 1) (Atk 1, 1) (HP 3)

instance HasAbilities Agent13SharonCarter where
  getAbilities a =
    [ limitedWindowAbility a 1 EntersPlayThis Response (OwnsThis <> SchemeExists ThwartableScheme) NoCost $
        RemoveThreat (toSource a) 2 ThwartableScheme
    ]

newtype Agent13SharonCarter = Agent13SharonCarter (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage Agent13SharonCarter where
  runMessage msg (Agent13SharonCarter attrs) = Agent13SharonCarter <$> runMessage msg attrs
