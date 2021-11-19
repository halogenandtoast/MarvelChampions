module Marvel.Upgrade.Upgrades.SpiderTracer where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message
import Marvel.Source
import Marvel.Target
import Marvel.Upgrade.Attrs
import Marvel.Upgrade.Cards qualified as Cards

spiderTracer :: UpgradeCard SpiderTracer
spiderTracer = upgrade SpiderTracer Cards.spiderTracer

newtype SpiderTracer = SpiderTracer UpgradeAttrs
  deriving anyclass IsUpgrade
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities SpiderTracer where
  getAbilities _ = []

instance RunMessage SpiderTracer where
  runMessage msg (SpiderTracer a) = SpiderTracer <$> runMessage msg a
