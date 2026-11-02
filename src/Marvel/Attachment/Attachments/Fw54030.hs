{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Attachment.Attachments.Fw54030 where
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
fw54030 :: AttachmentCard Fw54030
fw54030 = attachment Fw54030 Cards.fw54030

newtype Fw54030 = Fw54030 (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw54030 where
  getAbilities a@(Fw54030 attrs) =
    [ability a 1 HeroAction NoCriteria (ExhaustCharacterCost $ CharacterControlledBy You) (runAbility a 1)]

instance HasModifiersFor Fw54030 where
  getModifiersFor _ target (Fw54030 attrs) | Just target == (enemyRef <$> attachmentEnemy attrs) = pure [AttackModifier 1]
  getModifiersFor _ _ _ = pure []

instance RunMessage Fw54030 where
  runMessage msg a@(Fw54030 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      removeFromPlay attrs
      pure a
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      attachToNemesisOrVillain you attrs Cards.fw54028
      cardSpecific attrs "activateAttached" you
      pure a
    CardSpecific (isTarget attrs -> True) "activateAttached" (fromJSON -> Success you) -> do
      for_ (attachmentEnemy attrs) $ \enemy -> activateEnemy enemy you
      pure a
    _ -> Fw54030 <$> runMessage msg attrs
