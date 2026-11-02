{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.AlterEgo.AlterEgos.Fw54001b where
import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource qualified as R
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.AlterEgo.Types
import Marvel.AlterEgo.Runner ()

fw54001b :: AlterEgoCard Fw54001b
fw54001b = alterEgo Fw54001b Cards.fw54001b (HP $ Static 11) (HandSize 6) (Rec 3) [Cards.fw54027]

newtype Fw54001b = Fw54001b (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw54001b where
  getAbilities a@(Fw54001b attrs) =
    [label "$label.cyberneticallyEnhanced" $ ability a 1 Action (IsSelf <> NotCriteria (UpgradeExists $ UpgradeIs Cards.fw54002)) (ResourceCost Nothing) (runAbility a 1)]

instance RunMessage Fw54001b where
  runMessage msg a@(Fw54001b attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      search you (SearchIdentityDeckAndDiscardPile you) (CardIs Cards.fw54002) (SearchTarget $ toRef attrs) ShuffleBackIn
      pure a
    SearchFoundCards (isTarget attrs -> True) cards -> do
      let found = [card | PlayerCard card <- cards]
      for_ found $ putIntoPlay (alterEgoIdentityId attrs)
      pure a
    _ -> Fw54001b <$> runMessage msg attrs
