module Marvel.Hero.Heroes.MagikHero where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue
import Marvel.Hand
import Marvel.Stats
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner

magikHero :: HeroCard MagikHero
magikHero = hero MagikHero Cards.magikHero (HP $ Static 10) (HandSize 5) (Thw 1) (Atk 2) (Def 2)

newtype MagikHero = MagikHero (Attrs Hero)
  deriving anyclass (IsHero, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor MagikHero where
  getModifiersFor _ (IdentityRef you) (MagikHero attrs) =
    pure [PlayTopOfDeckAsHand | you == heroIdentityId attrs]
  getModifiersFor _ _ _ = pure []

instance RunMessage MagikHero where
  runMessage msg (MagikHero attrs) = runQueueT $ MagikHero <$> runMessage msg attrs
