module Marvel.Attachment.Attachments.Cable where

import Marvel.Minion.Cards qualified as Minions
import Marvel.Prelude
import Data.List ()
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Status ()
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Payment ()
import Marvel.Query
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Keyword qualified as Keyword ()
import {-# SOURCE #-} Marvel.Game ()

import Marvel.Question ( runAbility )
import Marvel.Ability
import Marvel.Modifier
import Marvel.Entity
import Marvel.Card
import Marvel.Ref
import Marvel.Message.Lifted
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Stats ()
import Marvel.GameValue ()
import Marvel.Hand ()

cable40034 :: AttachmentCard Cable40034
cable40034 = attachment Cable40034 Cards.cable40034

newtype Cable40034 = Cable40034 (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40034 where
  runMessage msg a@(Cable40034 attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment _) | aid == attachmentId attrs -> do
      stryfe <- selectOne (MinionIs Minions.cable40032)
      enemy <- maybe (fmap EnemyVillainId <$> selectOne ActiveVillain) (pure . Just . EnemyMinionId) stryfe
      for_ enemy \e -> attachTo aid e >> attachedTo e aid
      pure a
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      card <- getBoostCard (toTarget attrs)
      for_ card $ \c -> consumeBoostCard c >> attachEncounterCard c enemy
      pure a
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [(enemy, dmg) | W.EnemyWouldBeDamaged enemy _ dmg <- windows] \(enemy, dmg) -> do
        cancelEnemyDamage enemy
        when (damageAmount dmg >= 2) $ removeFromPlay attrs
      pure a
    _ -> Cable40034 <$> runMessage msg attrs

instance HasAbilities Cable40034 where
  getAbilities a@(Cable40034 attrs) =
    [limitedWindowAbility a 1 (W.EnemyWouldTakeDamage $ maybe (NotEnemy AnyEnemy) EnemyWithId $ attachmentEnemy attrs)
      ForcedInterrupt NoCriteria NoCost (runAbility a 1)]
