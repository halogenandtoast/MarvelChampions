module Marvel.Hero.Heroes.SpiderMan where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Window qualified as W

spiderMan :: HeroCard SpiderMan
spiderMan =
  hero
    SpiderMan
    Cards.spiderMan
    (HP $ Static 10)
    (HandSize 5)
    (Thw 1)
    (Atk 2)
    (Def 3)

instance HasAbilities SpiderMan where
  getAbilities a =
    [ label "Spider-Sense" $
        limitedWindowAbility
          a
          1
          (W.EnemyWouldAttack VillainEnemy You)
          Interrupt
          IsSelf
          NoCost
          (ChooseDrawCards 1 You)
    ]

newtype SpiderMan = SpiderMan (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage SpiderMan where
  runMessage msg (SpiderMan attrs) = SpiderMan <$> runMessage msg attrs
