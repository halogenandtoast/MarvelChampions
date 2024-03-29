module Marvel.AlterEgo.AlterEgos.TonyStark where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Card.Def
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Message
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Stats

tonyStark :: AlterEgoCard TonyStark
tonyStark =
  alterEgo
    TonyStark
    Cards.tonyStark
    (HP $ Static 9)
    (HandSize 6)
    (Rec 3)
    [Cards.businessProblems]

newtype TonyStark = TonyStark (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasAbilities TonyStark where
  getAbilities a =
    [ label "Futurist" $
        limitedAbility a 1 (PerRound 1) Action IsSelf NoCost $
          runAbility a 1
    ]

instance RunMessage TonyStark where
  runMessage msg ae@(TonyStark a) = case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      push . IdentityMessage ident $
        Search
          (SearchIdentityDeck ident $ TopOfDeck 3)
          AnyCard
          SearchDrawOne
          DiscardRest
      pure ae
    _ -> TonyStark <$> runMessage msg a
