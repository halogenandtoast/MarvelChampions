module Marvel.Hero.Heroes.PhoenixHero where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner

phoenixHero :: HeroCard PhoenixHero
phoenixHero = hero PhoenixHero Cards.phoenixHero (HP $ Static 9) (HandSize 5) (Thw 3) (Atk 1) (Def 2)

newtype PhoenixHero = PhoenixHero (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities PhoenixHero where
  getAbilities (PhoenixHero a) =
    [(ability a 1 HeroResource IsSelf (CountersFromUpgradeCost (UpgradeNamed "Phoenix Force") 1) (Pay $ ResourcePayment Wild)) {abilityLimit = PerPhase 1}]

instance RunMessage PhoenixHero where
  runMessage msg (PhoenixHero a) = runQueueT $ PhoenixHero <$> runMessage msg a
