module Marvel.AlterEgo.AlterEgos.GrootAlterEgo (
  grootAlterEgo,
  GrootAlterEgo (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Question
import Marvel.Ref
import Marvel.Stats

grootAlterEgo :: AlterEgoCard GrootAlterEgo
grootAlterEgo =
  alterEgo
    GrootAlterEgo
    Cards.grootAlterEgo
    (HP $ Static 10)
    (HandSize 6)
    (Rec 4)
    [Cards.wilt]

newtype GrootAlterEgo = GrootAlterEgo (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Growth Spurt — Action: place 2 growth counters on Groot (to a maximum of 10).
-- (Limit once per round.)
instance HasAbilities GrootAlterEgo where
  getAbilities a =
    [label "Growth Spurt" $ limitedAbility a 1 (PerRound 1) Action IsSelf NoCost $ runAbility a 1]

instance RunMessage GrootAlterEgo where
  runMessage msg ae@(GrootAlterEgo a) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      placeGrowthCounters ident 2
      pure ae
    _ -> GrootAlterEgo <$> runMessage msg a
