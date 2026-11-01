module Marvel.Ally.Allies.VictorMancha where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability qualified as Ability ()
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Query ()
import Marvel.Id ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question ()
import Marvel.Ref
import Marvel.Trait qualified as Trait ()
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Upgrade.Cards qualified as Upgrades ()
import Marvel.Window qualified as W ()
import Marvel.Stats ()
import Marvel.Hp ()
victorMancha :: AllyCard VictorMancha
victorMancha = ally VictorMancha Cards.victorMancha (Thw 0, 1) (Atk 0, 1) (HP 4)

newtype VictorMancha = VictorMancha (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor VictorMancha where
  getModifiersFor _ (AllyRef aid) (VictorMancha a) | aid == allyId a = pure [AttackDamageReduction 1]
  getModifiersFor _ _ _ = pure []

instance RunMessage VictorMancha where
  runMessage msg (VictorMancha attrs) = VictorMancha <$> runMessage msg attrs
