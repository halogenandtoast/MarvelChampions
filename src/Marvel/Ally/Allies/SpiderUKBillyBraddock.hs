module Marvel.Ally.Allies.SpiderUKBillyBraddock where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card
import Marvel.Cost
import Marvel.Entity
import Marvel.Modifier
import Marvel.Ref
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Trait (Trait (WebWarrior))
import Marvel.Window qualified as W

spiderUKBillyBraddock :: AllyCard SpiderUKBillyBraddock
spiderUKBillyBraddock = ally SpiderUKBillyBraddock Cards.spiderUKBillyBraddock (Thw 1, 1) (Atk 2, 2) (HP 5)

newtype SpiderUKBillyBraddock = SpiderUKBillyBraddock (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities SpiderUKBillyBraddock where
  getAbilities a@(SpiderUKBillyBraddock attrs) =
    [ limitedWindowAbility a 1 (W.AllyDefendsAgainst W.When (AllyWithId $ allyId attrs) AnyEnemy)
        Interrupt OwnsThis NoCost (runAbility a 1)
    ]

instance RunMessage SpiderUKBillyBraddock where
  runMessage msg a@(SpiderUKBillyBraddock attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 [W.AllyDefendedAgainst _ enemy] _ -> do
      n <- countControlledCardsWithTrait you WebWarrior
      dealDamage enemy attrs (toDamage n FromAbility)
      pure a
    _ -> SpiderUKBillyBraddock <$> runMessage msg attrs
