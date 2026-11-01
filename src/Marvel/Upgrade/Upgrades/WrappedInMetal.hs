module Marvel.Upgrade.Upgrades.WrappedInMetal where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource.Types ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait qualified as T
import Marvel.Window ()
import Marvel.Identity.Types ()

import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

wrappedInMetal :: UpgradeCard WrappedInMetal
wrappedInMetal = upgrade WrappedInMetal Cards.wrappedInMetal

newtype WrappedInMetal = WrappedInMetal (Attrs Upgrade)
  deriving anyclass (HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade WrappedInMetal where
  upgradeTargets _ = Just . map (enemyRef . EnemyMinionId) <$> selectList (AnyMinion <> NotMinion (MinionWithTrait T.Elite))

instance HasModifiersFor WrappedInMetal where
  getBlankingModifiersFor _ target (WrappedInMetal a) = pure [Blank | Just target == (enemyRef <$> upgradeAttachedEnemy a)]
  getModifiersFor _ target (WrappedInMetal a) = pure
    [m | Just target == (enemyRef <$> upgradeAttachedEnemy a), m <- [CannotActivate, Blank]]

instance RunMessage WrappedInMetal where
  runMessage msg (WrappedInMetal attrs) = runQueueT $ case msg of
    _ -> WrappedInMetal <$> runMessage msg attrs
