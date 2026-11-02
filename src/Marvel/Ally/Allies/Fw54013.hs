{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Ally.Allies.Fw54013 where
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
import Marvel.Ally.Types
import Marvel.Ally.Runner ()

fw54013 :: AllyCard Fw54013
fw54013 = ally Fw54013 Cards.fw54013 (Thw 2, 2) (Atk 1, 1) (HP 3)

newtype Fw54013 = Fw54013 (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw54013 where
  getAbilities a@(Fw54013 attrs) =
    [limitedWindowAbility a 1 (W.EntersPlayThis) Response (OwnsThis) (NoCost) (runAbility a 1)]

instance RunMessage Fw54013 where
  runMessage msg a@(Fw54013 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      search you (SearchIdentityDeckAndDiscardPile you) (CardIs Cards.fw54020) (SearchTarget $ toRef attrs) ShuffleBackIn
      pure a
    SearchFoundCards (isTarget attrs -> True) cards -> do
      for_ [card | PlayerCard card <- cards] $ \card -> putIntoPlayOn (allyController attrs) card (toTarget attrs)
      pure a
    _ -> Fw54013 <$> runMessage msg attrs
