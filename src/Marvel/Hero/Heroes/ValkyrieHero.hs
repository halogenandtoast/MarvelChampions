module Marvel.Hero.Heroes.ValkyrieHero (valkyrieHero, ValkyrieHero (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Def (CardMatcher (CardIs))
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Upgrade.Cards qualified as Upgrades

valkyrieHero :: HeroCard ValkyrieHero
valkyrieHero = hero ValkyrieHero Cards.valkyrieHero (HP $ Static 12) (HandSize 5) (Thw 1) (Atk 2) (Def 1)

newtype ValkyrieHero = ValkyrieHero (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasAbilities ValkyrieHero where
  getAbilities a =
    [ label "$label.deathPerception" $ ability a 1 HeroAction
        (IsSelf <> ExtendedCardExists (AffordableCardBy You $ InSetAsideOf You $ BasicCardMatches $ CardIs Upgrades.deathGlow))
        NoCost (runAbility a 1)
    ]

instance RunMessage ValkyrieHero where
  runMessage msg h@(ValkyrieHero attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> h <$ playCardFromSetAside ident "25002"
    _ -> ValkyrieHero <$> runMessage msg attrs
