module Marvel.Hero.Heroes.CaptainMarvel where

import Marvel.Prelude

import Marvel.GameValue
import Marvel.Hero.Attrs
import Marvel.Hero.Cards qualified as Cards
import Marvel.Message

captainMarvel :: HeroCard CaptainMarvel
captainMarvel = hero
  CaptainMarvel
  Cards.captainMarvel
  (HP $ Static 10)
  (HandSize 5)
  (Thw 2)
  (Atk 2)
  (Def 1)

newtype CaptainMarvel = CaptainMarvel HeroAttrs
  deriving anyclass IsHero
  deriving newtype (Show, Eq, HasStartingHP, ToJSON, FromJSON)

instance HasIdentityAttrs CaptainMarvel where
  identityAttrsL = lens
    (\(CaptainMarvel attrs) -> view identityAttrsL attrs)
    \(CaptainMarvel attrs) x -> CaptainMarvel $ set identityAttrsL x attrs

instance RunMessage CaptainMarvel where
  runMessage msg (CaptainMarvel attrs) = CaptainMarvel <$> runMessage msg attrs
