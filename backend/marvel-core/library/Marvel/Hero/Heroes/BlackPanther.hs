module Marvel.Hero.Heroes.BlackPanther where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Message
import Marvel.Modifier
import Marvel.Source
import Marvel.Stats
import Marvel.Target

blackPanther :: HeroCard BlackPanther
blackPanther = hero
  BlackPanther
  Cards.blackPanther
  (HP $ Static 11)
  (HandSize 5)
  (Thw 2)
  (Atk 2)
  (Def 2)

instance HasAbilities BlackPanther where
  getAbilities _ = []

newtype BlackPanther = BlackPanther (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsSource, IsTarget)

instance RunMessage BlackPanther where
  runMessage msg (BlackPanther attrs) = BlackPanther <$> runMessage msg attrs
