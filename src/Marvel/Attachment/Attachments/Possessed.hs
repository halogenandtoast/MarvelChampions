module Marvel.Attachment.Attachments.Possessed where

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
import Marvel.Attachment.Attachments.AllyAsMinion
import Marvel.Trait qualified as Trait
import Marvel.Count

possessed :: AttachmentCard Possessed
possessed = attachment Possessed Cards.possessed

newtype Possessed = Possessed (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Possessed where
  getBlankingModifiersFor _ (MinionRef mid) (Possessed attrs) = pure [Blank | attachedAllyMinion attrs == Just mid]
  getBlankingModifiersFor _ _ _ = pure []
  getModifiersFor _ target (Possessed attrs) = do
    traits <- allyMinionTrait Trait.Controlled attrs target
    pure $ traits <> [Blank | case target of MinionRef mid -> attachedAllyMinion attrs == Just mid; _ -> False]

instance RunMessage Possessed where
  runMessage msg a@(Possessed attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      allies <- selectList AnyAlly
      ranked <- for allies $ \ally -> (ally,) <$> selectCount AllyThwartValue (AllyWithId ally)
      let lowest = minimum (map snd ranked)
      eligible <- filterM (\(ally, _) -> do
        modifiers <- getModifiers (AllyRef ally)
        pure $ CannotHaveAttachedCards `notElem` modifiers && CannotHaveAttachedEncounterCards `notElem` modifiers)
        [(ally, thw) | (ally, thw) <- ranked, thw == lowest]
      if null eligible then surge you >> removeFromPlay attrs
      else chooseOrRunOne you
        [ targeting (AllyRef ally) do
            let mid = MinionId $ unAllyId ally
            allyBecomesMinion ally
            attachTo aid (EnemyMinionId mid)
            minionStep mid MinionCheckHitPoints
        | (ally, _) <- eligible
        ]
      pure a
    _ -> do
      revertsToAlly attrs msg
      Possessed <$> runMessage msg attrs
