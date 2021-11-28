module Marvel.Hero.Heroes.CaptainMarvel where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Attrs
import Marvel.Hero.Cards qualified as Cards
import Marvel.Message
import Marvel.Stats

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
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities CaptainMarvel where
  getAbilities _ = []

instance RunMessage CaptainMarvel where
  runMessage _ = pure
