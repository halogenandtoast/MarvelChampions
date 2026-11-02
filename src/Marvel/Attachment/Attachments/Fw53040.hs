{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Attachment.Attachments.Fw53040 where
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
fw53040 :: AttachmentCard Fw53040
fw53040 = attachment Fw53040 Cards.fw53040

newtype Fw53040 = Fw53040 (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw53040 where
  getAbilities a@(Fw53040 attrs) =
    [ability a 1 HeroAction NoCriteria (ExhaustCharactersWithThwartGreaterThanCost enemy) (runAbility a 1) | enemy <- maybeToList $ attachmentEnemy attrs]

instance HasModifiersFor Fw53040 where
  getModifiersFor _ target (Fw53040 attrs) | Just target == (enemyRef <$> attachmentEnemy attrs) = do
    cards <- maybe (pure []) attachmentsOnEnemy $ attachmentEnemy attrs
    let others = filter (\card -> toCardCode card /= "53040" && cardMatch (CardWithTrait T.Tech) card) cards
    pure [SchemeModifier 1, AttackModifier (1 + fromIntegral (length others))]
  getModifiersFor _ _ _ = pure []

instance RunMessage Fw53040 where
  runMessage msg a@(Fw53040 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      removeFromPlay attrs
      pure a
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      attachToNemesisOrVillain you attrs Cards.fw53038
      pure a
    _ -> Fw53040 <$> runMessage msg attrs
