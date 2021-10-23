module Marvel.Hero.Heroes.SpiderMan where

import Marvel.Prelude

import Marvel.GameValue
import Marvel.Hero.Attrs
import Marvel.Hero.Cards qualified as Cards
import Marvel.Message

spiderMan :: HeroCard SpiderMan
spiderMan = hero
  SpiderMan
  Cards.spiderMan
  (HP $ Static 10)
  (HandSize 5)
  (Thw 1)
  (Atk 2)
  (Def 3)

newtype SpiderMan = SpiderMan HeroAttrs
  deriving anyclass IsHero
  deriving newtype (Show, Eq, HasStartingHP, ToJSON, FromJSON)

deriving newtype instance HasIdentityAttrs SpiderMan
deriving newtype instance RunMessage SpiderMan
