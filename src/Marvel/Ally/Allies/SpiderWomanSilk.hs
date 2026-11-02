module Marvel.Ally.Allies.SpiderWomanSilk where
import Marvel.Prelude
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question ()
import Marvel.Window qualified as W
import Marvel.Trait qualified as T
import Marvel.Damage
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

spiderWomanSilk :: AllyCard SpiderWomanSilk
spiderWomanSilk = ally SpiderWomanSilk Cards.spiderWomanSilk (Thw 1, 1) (Atk 2, 1) (HP 3)
newtype SpiderWomanSilk = SpiderWomanSilk (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage SpiderWomanSilk where
  runMessage msg u@(SpiderWomanSilk attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> u <$ chooseDamage you attrs (toDamage 1 FromAbility) AnyEnemy
    _ -> SpiderWomanSilk <$> runMessage msg attrs

instance HasAbilities SpiderWomanSilk where
  getAbilities u@(SpiderWomanSilk _attrs) = [limitedWindowAbility u 1 (W.AllyEntersPlay W.After (AllyWithTrait T.WebWarrior)) Response (OwnsThis <> EnemyExists AnyEnemy) NoCost (runAbility u 1)]
