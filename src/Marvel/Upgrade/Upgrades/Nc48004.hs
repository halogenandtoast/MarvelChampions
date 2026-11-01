module Marvel.Upgrade.Upgrades.Nc48004 where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
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
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window ()
import Marvel.Upgrade.Types
import Marvel.Upgrade.Cards qualified as Cards

nc48004 :: UpgradeCard Nc48004
nc48004 = upgrade Nc48004 Cards.nc48004

newtype Nc48004 = Nc48004 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Nc48004 where
  getModifiersFor _ (UpgradeRef uid) (Nc48004 attrs) = pure [RestrictedSlots 2 | uid == upgradeId attrs]
  getModifiersFor _ (IdentityRef you) (Nc48004 _) = do
    hero <- selectAny (IdentityWithId you <> IdentityWithTitle "Nightcrawler" <> HeroIdentity)
    pure $ if hero then [AttackModifier 1, DefenseModifier 1, KeywordModifier (Retaliate 1)] else []
  getModifiersFor _ _ _ = pure []

instance RunMessage Nc48004 where
  runMessage msg (Nc48004 attrs) = runQueueT $ Nc48004 <$> runMessage msg attrs
