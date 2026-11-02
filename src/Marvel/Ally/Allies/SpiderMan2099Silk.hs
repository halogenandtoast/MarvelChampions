module Marvel.Ally.Allies.SpiderMan2099Silk where
import Marvel.Prelude
import Marvel.Query
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question ()
import Marvel.Window qualified as W
import Marvel.Trait qualified as T
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers ()

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.Ally.Runner

spiderMan2099Silk :: AllyCard SpiderMan2099Silk
spiderMan2099Silk = ally SpiderMan2099Silk Cards.spiderMan2099Silk (Thw 2, 1) (Atk 2, 1) (HP 3)
newtype SpiderMan2099Silk = SpiderMan2099Silk (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage SpiderMan2099Silk where
  runMessage msg u@(SpiderMan2099Silk attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      allies <- selectList (AllyWithTrait T.WebWarrior)
      chooseOne you [targeting aid (returnToHand aid) | aid <- allies]
      pure u
    _ -> SpiderMan2099Silk <$> runMessage msg attrs

instance HasAbilities SpiderMan2099Silk where
  getAbilities u@(SpiderMan2099Silk attrs) = [limitedWindowAbility u 1 (W.WindowOneOf [W.AllyAttacked W.After (AllyWithId $ allyId attrs) AnyEnemy, W.AllyThwarted W.After (AllyWithId $ allyId attrs) AnyScheme, W.AllyDefendsAgainst W.After (AllyWithId $ allyId attrs) AnyEnemy]) Response (OwnsThis <> AllyExists (AllyWithTrait T.WebWarrior)) NoCost (runAbility u 1)]
