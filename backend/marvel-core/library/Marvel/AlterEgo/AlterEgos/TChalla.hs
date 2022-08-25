module Marvel.AlterEgo.AlterEgos.TChalla where

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
import Marvel.Source
import Marvel.Stats
import Marvel.Target
import Marvel.Trait

tChalla :: AlterEgoCard TChalla
tChalla = alterEgo
  TChalla
  Cards.tChalla
  (HP $ Static 11)
  (HandSize 6)
  (Rec 4)
  [Cards.affairsOfState]

newtype TChalla = TChalla (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsSource, IsTarget)

instance HasAbilities TChalla where
  getAbilities a = [ability a 1 Setup NoCriteria NoCost $ runAbility a 1]

instance RunMessage TChalla where
  runMessage msg ae@(TChalla a) = case msg of
    RanAbility (isTarget a -> True) 1 _ -> do
      push . IdentityMessage (alterEgoIdentityId a) $ Search
        (SearchIdentityDeck (alterEgoIdentityId a) AllOfDeck)
        (CardWithTrait BlackPanther <> CardWithType UpgradeType)
        SearchDrawOne
        ShuffleBackIn
      pure ae
    _ -> TChalla <$> runMessage msg a
