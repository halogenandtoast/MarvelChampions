{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Ally.Allies.Fw53036 where
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

import Marvel.FalconWinter.Helpers
fw53036 :: AllyCard Fw53036
fw53036 = ally Fw53036 Cards.fw53036 (Thw 1, 1) (Atk 1, 1) (HP 3)

newtype Fw53036 = Fw53036 (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw53036 where
  getAbilities a@(Fw53036 attrs) =
    [limitedWindowAbility a 1 (W.AllyThwarted W.When (AllyWithId $ allyId attrs) AnyScheme) Interrupt (OwnsThis) (LookAtTopEncounterCardsAndDiscardOneCost 2) (runAbility a 1)]

instance RunMessage Fw53036 where
  runMessage msg a@(Fw53036 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      countFalconIcons attrs "misty" () [card | EncounterCard card <- paymentEncounterDiscards payment]
      pure a
    WithBoostIcons (isTarget attrs -> True) value counts -> a <$ resolveFalconIcons attrs value counts
    CardSpecific (isTarget attrs -> True) "misty" (fromJSON -> Success (() :: (), bonus :: Natural)) -> do
      addToAllyBasicPower (allyId attrs) bonus
      pure a
    _ -> Fw53036 <$> runMessage msg attrs
