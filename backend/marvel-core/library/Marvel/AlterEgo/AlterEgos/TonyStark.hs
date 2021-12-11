module Marvel.AlterEgo.AlterEgos.TonyStark where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo.Attrs
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Message
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Stats
import Marvel.Target

tonyStark :: AlterEgoCard TonyStark
tonyStark = alterEgo
  TonyStark
  Cards.tonyStark
  (HP $ Static 9)
  (HandSize 6)
  (Rec 3)
  [Cards.businessProblems]

newtype TonyStark = TonyStark AlterEgoAttrs
  deriving anyclass IsAlterEgo
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsSource, Entity)

instance HasAbilities TonyStark where
  getAbilities (TonyStark a) =
    [label "Futurist" $ limitedAbility a 1 (PerRound 1) Action IsSelf NoCost (RunAbility (toTarget a) 1)]

instance RunMessage TonyStark where
  runMessage msg a@(TonyStark attrs) = case msg of
    RanAbility target 1 _ | isTarget attrs target -> do
      pushAll $ map
        (IdentityMessage (toId attrs))
        [ Search
          (SearchIdentityDeck (toId attrs) $ TopOfDeck 3)
          AnyCard
          SearchDrawOne
          DiscardRest
        ]
      pure a
    _ -> TonyStark <$> runMessage msg attrs
