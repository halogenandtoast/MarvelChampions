{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Support.Supports.Fw54007 where
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
import Marvel.Support.Types

import Marvel.FalconWinter.Helpers
fw54007 :: SupportCard Fw54007
fw54007 = support Fw54007 Cards.fw54007

newtype Fw54007 = Fw54007 (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw54007 where
  getAbilities a@(Fw54007 attrs) =
    [ability a 1 AlterEgoAction OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage Fw54007 where
  runMessage msg a@(Fw54007 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      searchEncounterMinion you
      pure a
    SearchFoundCards (isTarget attrs -> True) cards -> do
      let you = supportController attrs
      chooseOne you [targeting card do
        putEncounterCardIntoPlay you card
        drawCards you 1
        | EncounterCard card <- cards]
      pure a
    _ -> Fw54007 <$> runMessage msg attrs
