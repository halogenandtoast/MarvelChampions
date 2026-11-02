{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Attachment.Attachments.Fw53039 where
import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource qualified as R
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.Attachment.Types

import Marvel.FalconWinter.Helpers
fw53039 :: AttachmentCard Fw53039
fw53039 = attachment Fw53039 Cards.fw53039

newtype Fw53039 = Fw53039 (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw53039 where
  getAbilities a@(Fw53039 attrs) =
    [limitedWindowAbility a 1 (W.EnemyWouldBeDealtDamageAtLeast (maybe AnyEnemy EnemyWithId $ attachmentEnemy attrs) 3) ForcedInterrupt NoCriteria NoCost (runAbility a 1)]

instance HasModifiersFor Fw53039 where
  getModifiersFor _ target (Fw53039 attrs) | Just target == (enemyRef <$> attachmentEnemy attrs) = pure [TraitModifier T.Aerial]
  getModifiersFor _ _ _ = pure []

instance RunMessage Fw53039 where
  runMessage msg a@(Fw53039 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      for_ [(enemy, dmg) | W.EnemyDamageWouldBeDealt enemy _ dmg <- windows, damageAmount dmg >= 3] $ \(enemy, dmg) -> do
        preventEnemyDamage enemy (damageAmount dmg)
        removeFromPlay attrs
      pure a
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      attachToNemesisOrVillain you attrs Cards.fw53038
      pure a
    _ -> Fw53039 <$> runMessage msg attrs
