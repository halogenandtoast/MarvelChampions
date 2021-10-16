module Marvel.Hero.Heroes.CaptainMarvel where

import Marvel.Prelude

import Marvel.GameValue
import Marvel.Hero.Attrs
import Marvel.Hero.Cards qualified as Cards
import Marvel.Message

captainMarvel :: HeroCard CaptainMarvel
captainMarvel =
  hero CaptainMarvel Cards.captainMarvel (HP $ Static 10) (HandSize 5) (Thw 2) (Atk 2) (Def 1)

newtype CaptainMarvel = CaptainMarvel HeroAttrs
  deriving anyclass IsHero
  deriving newtype (Show, Eq, HasStartingHP, HasIdentityAttrs, ToJSON, FromJSON)

instance RunMessage CaptainMarvel where
  runMessage msg (CaptainMarvel attrs) = CaptainMarvel <$> runMessage msg attrs
