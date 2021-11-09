{-# LANGUAGE UndecidableInstances #-}
module Marvel.Hero.Heroes.SpiderMan where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Attrs
import qualified Marvel.Hero.Cards as Cards
import Marvel.Message
import Marvel.Question
import Marvel.Source
import Marvel.Stats
import Marvel.Target

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
    [ label "Spider-Sense"
        $ ability a 1 Interrupt IsSelf NoCost (RunAbility (toTarget a) 1)
    ]

newtype SpiderMan = SpiderMan HeroAttrs
  deriving anyclass IsHero
  deriving newtype (Show, Eq, HasStartingHP, ToJSON, FromJSON, IsSource, IsTarget, HasCardCode, Entity)

instance RunMessage SpiderMan where
  runMessage msg (SpiderMan attrs) = SpiderMan <$> runMessage msg attrs
