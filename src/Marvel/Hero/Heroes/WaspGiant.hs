module Marvel.Hero.Heroes.WaspGiant (
  waspGiant,
  WaspGiant (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Message
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats

waspGiant :: HeroCard WaspGiant
waspGiant =
  hero
    WaspGiant
    Cards.waspGiant
    (HP $ Static 11)
    (HandSize 5)
    (Thw 2)
    (Atk 2)
    (Def 3)

instance HasAbilities WaspGiant where
  getAbilities _ = []

newtype WaspGiant = WaspGiant (Attrs Hero)
  deriving anyclass (IsHero)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | [star] Threat you remove using your basic thwart power can be divided among schemes
-- as you choose. [star] Damage you deal using your basic attack power can be divided
-- among enemies as you choose.
instance HasModifiersFor WaspGiant where
  getModifiersFor _ target (WaspGiant a) | isTarget a target = pure [DividesBasicThwart, DividesBasicAttack]
  getModifiersFor _ _ _ = pure []

instance RunMessage WaspGiant where
  runMessage msg (WaspGiant attrs) = WaspGiant <$> runMessage msg attrs
