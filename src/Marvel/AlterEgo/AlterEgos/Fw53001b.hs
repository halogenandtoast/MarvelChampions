{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.AlterEgo.AlterEgos.Fw53001b where
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

fw53001b :: AlterEgoCard Fw53001b
fw53001b = alterEgo Fw53001b Cards.fw53001b (HP $ Static 10) (HandSize 6) (Rec 3) [Cards.fw53029]

newtype Fw53001b = Fw53001b (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw53001b where
  getAbilities a@(Fw53001b attrs) =
    [label "$label.birdsOfAFeather" $ limited (PerRound 1) $ ability a 1 Action IsSelf (DiscardHandCardsCost 1 $ Just 1) (runAbility a 1)]

instance RunMessage Fw53001b where
  runMessage msg a@(Fw53001b attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      search you (SearchIdentityDeckAndDiscardPile you) (CardWithTrait T.Bird) SearchDrawOne ShuffleBackIn
      pure a
    _ -> Fw53001b <$> runMessage msg attrs
