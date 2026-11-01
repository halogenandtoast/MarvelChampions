module Marvel.Hero.Heroes.ShadowcatHero where

import Marvel.Prelude
import Marvel.Card.Side ()
import Marvel.Payment ()
import Marvel.Game.Source ()
import Marvel.Hand
import Marvel.Stats
import Marvel.Status ()
import Marvel.Id ()
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Upgrade.Cards qualified as Upgrades

shadowcatHero :: HeroCard ShadowcatHero
shadowcatHero = hero ShadowcatHero Cards.shadowcatHero (HP $ Static 9) (HandSize 5) (Thw 2) (Atk 2) (Def 2)

newtype ShadowcatHero = ShadowcatHero (Attrs Hero)
  deriving anyclass (IsHero, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor ShadowcatHero where
  getModifiersFor _ (IdentityRef you) (ShadowcatHero a) | you == heroIdentityId a = do
    phased <- isInMassForm you Upgrades.phased
    pure $ if phased then [IgnoresGuard, IgnoresPatrol, IgnoresCrisis] else []
  getModifiersFor _ _ _ = pure []

instance RunMessage ShadowcatHero where
  runMessage msg (ShadowcatHero attrs) = runQueueT $ case msg of
    _ -> ShadowcatHero <$> runMessage msg attrs
