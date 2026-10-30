module Marvel.Attachment.Attachments.ExperimentalInjection where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Query
import Marvel.Trait (Trait (Creature))

experimentalInjection :: AttachmentCard ExperimentalInjection
experimentalInjection = attachment ExperimentalInjection Cards.experimentalInjection

newtype ExperimentalInjection = ExperimentalInjection (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor ExperimentalInjection where
  getModifiersFor _ (MinionRef mid) (ExperimentalInjection attrs)
    | attachmentEnemy attrs == Just (EnemyMinionId mid) = pure [HitPointModifier 4, TraitModifier Creature]
  getModifiersFor _ _ _ = pure []

instance RunMessage ExperimentalInjection where
  runMessage msg a@(ExperimentalInjection attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      minions <- selectList (MinionWithMostRemainingHitPointsAmong AnyMinion)
      if null minions
        then removeFromPlay attrs >> surge you
        else chooseOrRunOne you [targeting mid (attachTo aid mid) | mid <- minions]
      pure a
    _ -> ExperimentalInjection <$> runMessage msg attrs
