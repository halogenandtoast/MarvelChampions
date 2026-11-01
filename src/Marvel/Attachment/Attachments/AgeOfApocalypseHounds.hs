module Marvel.Attachment.Attachments.AgeOfApocalypseHounds where

import Marvel.Prelude
import Marvel.Question (runAbility)
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query
import Marvel.Minion.Cards qualified as Minions
import Marvel.Window qualified as W
import Marvel.Card
import Marvel.Entity
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Ability ()
import Marvel.Modifier ()
import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype AoaHoundsAttachment = AoaHoundsAttachment (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ahabsEnergySpear :: AttachmentCard AoaHoundsAttachment
ahabsEnergySpear = attachment AoaHoundsAttachment Cards.ahabsEnergySpear

instance HasAbilities AoaHoundsAttachment where
  getAbilities a@(AoaHoundsAttachment attrs) =
    [limitedWindowAbility a 1 (W.EnemyAttacked W.After (EnemyWithId enemy) AnyIdentity)
      ForcedResponse NoCriteria NoCost (runAbility a 1) | Just enemy <- [attachmentEnemy attrs]]

instance HasModifiersFor AoaHoundsAttachment where
  getModifiersFor _ target (AoaHoundsAttachment attrs) = pure $
    if Just target == (enemyRef <$> attachmentEnemy attrs)
      then [AttackModifier 2, KeywordModifier Overkill, KeywordModifier Piercing]
      else []

instance RunMessage AoaHoundsAttachment where
  runMessage msg a@(AoaHoundsAttachment attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment _) | aid == attachmentId attrs -> do
      ahab <- selectOne $ MinionIs Minions.ahab
      case ahab of
        Just mid -> attachTo (attachmentId attrs) mid
        Nothing -> selectOne ActiveVillain >>= traverse_ (attachTo (attachmentId attrs))
      pure a
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> a <$ removeFromPlay attrs
    _ -> AoaHoundsAttachment <$> runMessage msg attrs
