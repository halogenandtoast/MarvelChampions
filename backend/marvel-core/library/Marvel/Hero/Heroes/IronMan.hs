module Marvel.Hero.Heroes.IronMan where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Attrs
import Marvel.Hero.Cards qualified as Cards
import Marvel.Message
import Marvel.Source
import Marvel.Stats
import Marvel.Target

ironMan :: HeroCard IronMan
ironMan = hero
  IronMan
  Cards.ironMan
  (HP $ Static 9)
  (HandSize 1)
  (Thw 2)
  (Atk 1)
  (Def 1)

instance HasAbilities IronMan where
  getAbilities _ = []

newtype IronMan = IronMan HeroAttrs
  deriving anyclass IsHero
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsSource, IsTarget, Entity)

instance RunMessage IronMan where
  runMessage msg (IronMan attrs) = IronMan <$> runMessage msg attrs
