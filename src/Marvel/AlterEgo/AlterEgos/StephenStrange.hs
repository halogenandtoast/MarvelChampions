module Marvel.AlterEgo.AlterEgos.StephenStrange where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Invocation
import Marvel.Message
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Stats

stephenStrange :: AlterEgoCard StephenStrange
stephenStrange =
  alterEgo
    StephenStrange
    Cards.stephenStrange
    (HP $ Static 10)
    (HandSize 6)
    (Rec 3)
    [Cards.physicalToll]

newtype StephenStrange = StephenStrange (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | He begins the game with an Invocation deck; Natural Talent discards its top card.
-- ponytail: an action is used only on your own turn, so once per turn is once per phase
instance HasAbilities StephenStrange where
  getAbilities a@(StephenStrange attrs) =
    [ ability a 1 Setup NoCriteria NoCost $ runAbility a 1
    , label "Natural Talent" $
        limitedAbility a 2 (PerTurn 1) Action IsSelf NoCost $
          Run [invocation (alterEgoIdentityId attrs) DiscardTopInvocation]
    ]

instance RunMessage StephenStrange where
  runMessage msg ae@(StephenStrange a) = case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      push $ invocation ident SetUpInvocationDeck
      pure ae
    _ -> StephenStrange <$> runMessage msg a
