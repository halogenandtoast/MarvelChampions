module Marvel.Upgrade.Upgrades.PracticedDefense where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Window qualified as W ()
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

practicedDefense :: UpgradeCard PracticedDefense
practicedDefense = upgrade PracticedDefense Cards.practicedDefense

newtype PracticedDefense = PracticedDefense (Attrs Upgrade)
  deriving anyclass (HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade PracticedDefense where
  upgradeTargets _ = Just . map enemyRef <$> selectList (NotEnemy $ EnemyWithUpgrade $ UpgradeIs Cards.practicedDefense)

instance HasModifiersFor PracticedDefense where
  getModifiersFor _ target (PracticedDefense attrs) = pure
    [AttackPenalty 1 | Just enemy <- [upgradeAttachedEnemy attrs], target == enemyRef enemy]

instance RunMessage PracticedDefense where
  runMessage msg (PracticedDefense attrs) = runQueueT $ PracticedDefense <$> runMessage msg attrs
