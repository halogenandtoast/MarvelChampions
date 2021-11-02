module Marvel.Hero.Heroes.CaptainMarvel where

import Marvel.Prelude

import Marvel.Ability
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

deriving newtype instance HasAbilities CaptainMarvel
deriving newtype instance HasIdentityAttrs CaptainMarvel
deriving newtype instance RunMessage CaptainMarvel
