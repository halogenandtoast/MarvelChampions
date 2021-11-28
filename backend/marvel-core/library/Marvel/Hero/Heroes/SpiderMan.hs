module Marvel.Hero.Heroes.SpiderMan where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Attrs
import Marvel.Hero.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message
import Marvel.Question
import Marvel.Source
import Marvel.Stats
import Marvel.Target
import Marvel.Window qualified as W

spiderMan :: HeroCard SpiderMan
spiderMan = hero
  SpiderMan
  Cards.spiderMan
  (HP $ Static 10)
  (HandSize 5)
  (Thw 1)
  (Atk 2)
  (Def 3)

instance HasAbilities SpiderMan where
  getAbilities a =
    [ label "Spider-Sense" $ limitedWindowAbility
        a
        1
        (W.EnemyWouldAttack VillainEnemy You)
        Interrupt
        IsSelf
        NoCost
        (ChooseDrawCards 1 You)
    ]

newtype SpiderMan = SpiderMan HeroAttrs
  deriving anyclass IsHero
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsSource, IsTarget, Entity)

instance RunMessage SpiderMan where
  runMessage msg (SpiderMan attrs) = SpiderMan <$> runMessage msg attrs
