module Marvel.Hero.Heroes.SpdrSuitHero where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Types
import Marvel.GameValue
import Marvel.Hand
import Marvel.Stats
import Marvel.Hero.Runner

spdrSuitHero :: HeroCard SpdrSuitHero
spdrSuitHero = hero SpdrSuitHero Cards.spdrSuitHero (HP $ Static 14) (HandSize 3) (Thw 2) (Atk 2) (Def 2)

newtype SpdrSuitHero = SpdrSuitHero (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- Sync Ratio's selectable Interface resource abilities are built by the engine,
-- so each ready upgrade is offered independently during payment.
instance RunMessage SpdrSuitHero where
  runMessage msg (SpdrSuitHero attrs) = runQueueT $ SpdrSuitHero <$> runMessage msg attrs
