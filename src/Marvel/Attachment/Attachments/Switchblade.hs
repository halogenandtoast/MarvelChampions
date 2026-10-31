module Marvel.Attachment.Attachments.Switchblade where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Status
import Marvel.Window qualified as W
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Keyword

switchblade :: AttachmentCard Switchblade
switchblade = attachment Switchblade Cards.switchblade

newtype Switchblade = Switchblade (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Switchblade where
  -- The printed +2 ATK sits outside its text box and survives blanking.
  getPrintedModifiersFor _ target (Switchblade attrs) = pure
    [ AttackModifier 2
    | Just enemy <- [attachmentEnemy attrs]
    , target == enemyRef enemy
    ]
  getModifiersFor _ target (Switchblade attrs) = pure
    [ KeywordModifier Piercing
    | Just enemy <- [attachmentEnemy attrs]
    , target == enemyRef enemy
    ]

instance RunMessage Switchblade where
  runMessage msg a@(Switchblade attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      minions <- selectList AnyMinion
      ranked <- for minions $ \mid -> (mid,) <$> getEnemyPrintedAttack (EnemyMinionId mid)
      let highest = maximum (map snd ranked)
      eligible <- filterM (\(mid, _) -> do
        modifiers <- getModifiers mid
        pure $ CannotHaveAttachedCards `notElem` modifiers && CannotHaveAttachedEncounterCards `notElem` modifiers)
        [(mid, atk) | (mid, atk) <- ranked, atk == highest]
      if null eligible then surge you >> removeFromPlay attrs
      else chooseOrRunOne you [targeting (MinionRef mid) (attachTo aid (EnemyMinionId mid)) | (mid, _) <- eligible]
      pure a
    _ -> Switchblade <$> runMessage msg attrs
