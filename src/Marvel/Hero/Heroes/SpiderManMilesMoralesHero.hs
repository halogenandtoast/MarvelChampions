module Marvel.Hero.Heroes.SpiderManMilesMoralesHero where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.GameValue
import Marvel.Hand
import Marvel.Stats
import Marvel.Damage
import Marvel.Matchers
import Marvel.Query

spiderManMilesMoralesHero :: HeroCard SpiderManMilesMoralesHero
spiderManMilesMoralesHero = hero SpiderManMilesMoralesHero Cards.spiderManMilesMoralesHero (HP $ Static 9) (HandSize 5) (Thw 2) (Atk 2) (Def 2)

newtype SpiderManMilesMoralesHero = SpiderManMilesMoralesHero (Attrs Hero)
  deriving anyclass (IsHero, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage SpiderManMilesMoralesHero where
  runMessage msg h@(SpiderManMilesMoralesHero attrs) = runQueueT $ case msg of
    CardSpecific (isTarget attrs -> True) "venomBlast" _ -> do
      blocked <- elem CannotTriggerPrintedHeroAbilities <$> getModifiers attrs
      unless blocked $ specialAbility (heroIdentityId attrs) attrs do
        enemies <- selectList DamageableEnemy
        unless (null enemies) $ chooseOne (heroIdentityId attrs)
          [ targeting (enemyRef enemy) do
              dealDamage enemy attrs (toDamage 2 FromAbility)
              stun enemy attrs
          | enemy <- enemies ]
      pure h
    CardSpecific (isTarget attrs -> True) "spiderCamouflage" _ -> do
      blocked <- elem CannotTriggerPrintedHeroAbilities <$> getModifiers attrs
      unless blocked $ specialAbility (heroIdentityId attrs) attrs do
        becomeTough (heroIdentityId attrs)
        enemies <- selectList AnyEnemy
        unless (null enemies) $ chooseOne (heroIdentityId attrs)
          [targeting (enemyRef enemy) (confuse enemy attrs) | enemy <- enemies]
      pure h
    _ -> SpiderManMilesMoralesHero <$> runMessage msg attrs
