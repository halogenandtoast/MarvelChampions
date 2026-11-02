{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Ally.Allies.Fw54003 where
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
import Marvel.Discard
import Marvel.Identity.Types (Field(PlayerIdentityDiscard))
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

fw54003 :: AllyCard Fw54003
fw54003 = ally Fw54003 Cards.fw54003 (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype Fw54003 = Fw54003 (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw54003 where
  getAbilities a@(Fw54003 attrs) =
    [limitedWindowAbility a 1 (W.PlayedFromHand (IdentityWithId $ allyController attrs) (CardIs Cards.fw54003)) Response (OwnsThis <> ExtendedCardExists (InDiscardOf (IdentityWithId $ allyController attrs) $ BasicCardMatches $ CardWithType EventType <> CardWithTrait T.Attack)) (NoCost) (runAbility a 1)]

instance RunMessage Fw54003 where
  runMessage msg a@(Fw54003 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      cards <- filter (cardMatch $ CardWithType EventType <> CardWithTrait T.Attack) <$> projectMap PlayerIdentityDiscard unDiscard you
      chooseOne you [targeting card $ addToHand you card | card <- cards]
      pure a
    _ -> Fw54003 <$> runMessage msg attrs
