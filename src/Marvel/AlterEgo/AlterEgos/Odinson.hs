module Marvel.AlterEgo.AlterEgos.Odinson where

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
import Marvel.Upgrade.Cards qualified as Upgrades

odinson :: AlterEgoCard Odinson
odinson =
  alterEgo
    Odinson
    Cards.odinson
    (HP $ Static 14)
    (HandSize 5)
    (Rec 4)
    [Cards.odinsAnger]

newtype Odinson = Odinson (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasAbilities Odinson where
  getAbilities a =
    [ label "Worthy" $
        limitedAbility a 1 (PerRound 1) Action IsSelf NoCost $
          runAbility a 1
    ]

instance RunMessage Odinson where
  runMessage msg ae@(Odinson a) = case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      push . IdentityMessage ident $
        Search
          (SearchIdentityDeckAndDiscardPile ident)
          (CardIs Upgrades.mjolnir)
          SearchDrawOne
          ShuffleBackIn
      pure ae
    _ -> Odinson <$> runMessage msg a
