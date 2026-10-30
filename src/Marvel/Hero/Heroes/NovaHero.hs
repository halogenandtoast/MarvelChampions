module Marvel.Hero.Heroes.NovaHero where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Stats
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Window qualified as W

novaHero :: HeroCard NovaHero
novaHero = hero NovaHero Cards.novaHero (HP $ Static 10) (HandSize 5) (Thw 1) (Atk 1) (Def 2)

newtype NovaHero = NovaHero (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities NovaHero where
  getAbilities a@(NovaHero attrs) =
    [ limitedWindowAbility a 1
        (W.WindowOneOf [W.MakesBasicAttack W.After you, W.MakesBasicThwart W.After you, W.HeroDefended W.After you AnyEnemy])
        Response
        (IsSelf <> UpgradeExists (helmet <> UpgradeCanBeReadied))
        NoCost (runAbility a 1)
    ]
   where
    you = IdentityWithId $ heroIdentityId attrs
    helmet = UpgradeIs Upgrades.supernovaHelmet <> UpgradeControlledBy you

instance RunMessage NovaHero where
  runMessage msg h@(NovaHero attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      helmets <- selectList (UpgradeIs Upgrades.supernovaHelmet <> upgradeControlledBy (heroIdentityId attrs))
      traverse_ ready helmets
      pure h
    _ -> NovaHero <$> runMessage msg attrs
