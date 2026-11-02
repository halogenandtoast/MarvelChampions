{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Ally.Allies.Fw53018 where
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

fw53018 :: AllyCard Fw53018
fw53018 = ally Fw53018 Cards.fw53018 (Thw 1, 1) (Atk 1, 1) (HP 3)

newtype Fw53018 = Fw53018 (Attrs Ally)
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw53018 where
  getAbilities a@(Fw53018 attrs) =
    [limitedWindowAbility a 1 (W.PlayThis W.After) Response (OwnsThis) (NoCost) (runAbility a 1) | notNull $ paymentSpentCards $ allyPaidWith attrs]

instance HasModifiersFor Fw53018 where
  getModifiersFor _ target (Fw53018 attrs) | target == toTarget attrs = do
    cards <- heldCardsAt attrs
    let resources = concatMap (printedResources . getCardDef) cards
        has r = r `elem` resources || R.Wild `elem` resources
    pure $ [ThwartModifier 2 | has R.Mental] <> [AttackModifier 2 | has R.Physical] <> [HitPointModifier 2 | has R.Energy]
  getModifiersFor _ _ _ = pure []

instance RunMessage Fw53018 where
  runMessage msg a@(Fw53018 attrs) = runQueueT $ case msg of
    CardSpecific (isTarget attrs -> True) "mgHoldCard" (fromJSON -> Success card) -> pure $ Fw53018 attrs {allyHeldCards = allyHeldCards attrs <> [card]}
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      let cards = paymentSpentCards $ allyPaidWith attrs
      chooseOne you [targeting card $ placePlayerCardUnder attrs card | card <- cards]
      pure a
    _ -> Fw53018 <$> runMessage msg attrs
