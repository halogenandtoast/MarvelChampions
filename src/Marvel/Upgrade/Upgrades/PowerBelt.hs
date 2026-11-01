module Marvel.Upgrade.Upgrades.PowerBelt where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection ()
import Marvel.Identity.Types ()
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W ()
import Marvel.Iceman.Cards qualified as Cards
import Marvel.Iceman.Helpers ()
import Marvel.Upgrade.Types

powerBelt :: UpgradeCard PowerBelt
powerBelt = upgrade PowerBelt Cards.powerBelt

newtype PowerBelt = PowerBelt (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor PowerBelt where
  getModifiersFor _ (IdentityRef you) (PowerBelt attrs) = pure [HitPointModifier 3 | you == upgradeController attrs]
  getModifiersFor _ _ _ = pure []
instance HasAbilities PowerBelt where
  getAbilities a = [ability a 1 HeroResource (OwnsThis <> PayingFor (CardWithTrait Trait.Ice)) ExhaustCost (Pay $ ResourcePayment Wild)]
instance RunMessage PowerBelt where
  runMessage msg (PowerBelt attrs) = runQueueT $ PowerBelt <$> runMessage msg attrs
