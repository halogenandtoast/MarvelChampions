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

instance HasIdentityAttrs SpiderMan where
  identityAttrsL = lens
    (\(SpiderMan attrs) -> view identityAttrsL attrs)
    \(SpiderMan attrs) x -> SpiderMan $ set identityAttrsL x attrs

instance RunMessage SpiderMan where
  runMessage msg (SpiderMan attrs) = SpiderMan <$> runMessage msg attrs
