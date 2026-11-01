module Marvel.Ally.Allies.Warpath where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Damage
import Marvel.Trait qualified as Trait
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

warpath :: AllyCard Warpath
warpath = ally Warpath Cards.warpath (Thw 2, 2) (Atk 2, 1) (HP 4)

newtype Warpath = Warpath (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Warpath where
  getAbilities a@(Warpath attrs) = [limitedWindowAbility a 1
    (W.AllyDefends $ AllyWithId $ allyId attrs)
    HeroResponse (OwnsThis <> PlayCardWithReductionHasTarget heroAction 0) NoCost (runAbility a 1)]

heroAction :: CardMatcher
heroAction = CardWithType EventType <> CardWithAbilityType HeroAction

instance RunMessage Warpath where
  runMessage msg a@(Warpath attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      hand <- selectList (InHandOf (IdentityWithId you) $ BasicCardMatches heroAction)
      cards <- filterM (canPlayCardWithReduction you 0) hand
      chooseOne you [targeting card (playCardWithReduction you 0 card) | card <- cards]
      pure a
    _ -> Warpath <$> runMessage msg attrs
