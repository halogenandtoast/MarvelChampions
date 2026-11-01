module Marvel.Upgrade.Upgrades.Nc48005 where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Boost ()
import Marvel.Phase ()
import Marvel.Ally.Types ()
import Marvel.Minion.Types ()
import Marvel.Nightcrawler.Helpers ()
import Marvel.Minion.Cards ()
import Marvel.SideScheme.Cards ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection ()
import Marvel.Query ()
import Marvel.Question (Choice (Pay))
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window ()
import Marvel.Upgrade.Types
import Marvel.Upgrade.Cards qualified as Cards

nc48005 :: UpgradeCard Nc48005
nc48005 = upgrade Nc48005 Cards.nc48005

newtype Nc48005 = Nc48005 (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Nc48005 where
  getModifiersFor _ (IdentityRef you) (Nc48005 attrs) = pure [RestrictedLimitModifier 1 | you == upgradeController attrs]
  getModifiersFor _ _ _ = pure []

instance HasAbilities Nc48005 where
  getAbilities a = [ability a 1 Resource (OwnsThis <> PayingFor (CardWithType EventType)) ExhaustCost (Pay $ ResourcePayment Wild)]

instance RunMessage Nc48005 where
  runMessage msg (Nc48005 attrs) = runQueueT $ Nc48005 <$> runMessage msg attrs
