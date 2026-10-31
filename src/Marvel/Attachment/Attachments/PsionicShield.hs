module Marvel.Attachment.Attachments.PsionicShield where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Window qualified as W
import Marvel.Trait qualified as Trait
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types

psionicShield :: AttachmentCard PsionicShield
psionicShield = attachment PsionicShield Cards.psionicShield

newtype PsionicShield = PsionicShield (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor PsionicShield where
  getModifiersFor _ (MinionRef mid) (PsionicShield attrs)
    | attachmentEnemy attrs == Just (EnemyMinionId mid) = pure [AttackModifier 1, SchemeModifier 1]
  getModifiersFor _ _ _ = pure []

instance HasAbilities PsionicShield where
  getAbilities (PsionicShield attrs) = case attachmentEnemy attrs of
    Just (EnemyMinionId mid) ->
      [windowAbility attrs 1 (W.MinionWouldLeavePlay $ MinionWithId mid) ForcedInterrupt NoCost (runAbility attrs 1)]
    _ -> []

instance RunMessage PsionicShield where
  runMessage msg a@(PsionicShield attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      minions <- selectList AnyMinion
      if null minions
        then surge you >> removeFromPlay attrs
        else chooseOrRunOne you [targeting mid (attachTo aid mid) | mid <- minions]
      pure a
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      for_ [mid | Just (EnemyMinionId mid) <- [attachmentEnemy attrs]] $ \mid -> do
        cancelMinionLeavingPlay mid
        healAllDamage mid
        removeFromPlay attrs
      pure a
    _ -> PsionicShield <$> runMessage msg attrs
