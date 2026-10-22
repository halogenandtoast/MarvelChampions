module Marvel.Attachment.Attachments.RedSkullsLuger (
  redSkullsLuger,
  RedSkullsLuger (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Criteria
import Marvel.Entity
import Marvel.Cost
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource

redSkullsLuger :: AttachmentCard RedSkullsLuger
redSkullsLuger = attachment RedSkullsLuger Cards.redSkullsLuger

newtype RedSkullsLuger = RedSkullsLuger (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | +1 ATK and +1 SCH, and his attacks gain piercing and ranged.
instance HasModifiersFor RedSkullsLuger where
  getModifiersFor _ (VillainRef vid) (RedSkullsLuger a)
    | Just (EnemyVillainId vid) == attachmentEnemy a =
        pure [AttackModifier 1, SchemeModifier 1, KeywordModifier Piercing, KeywordModifier Ranged]
  getModifiersFor _ _ _ = pure []

instance HasAbilities RedSkullsLuger where
  getAbilities (RedSkullsLuger a) = case attachmentEnemy a of
    Just _ ->
      [ ability a 1 HeroAction NoCriteria (MultiResourceCost [Just Energy, Just Mental, Just Physical]) $
          DiscardTarget (toRef a)
      ]
    _ -> []

instance RunMessage RedSkullsLuger where
  runMessage msg a@(RedSkullsLuger attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      attachToTheVillain attrs
      pure a
    -- Boost: attach to Red Skull
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      -- with no villain in play (as between one Kang and the next), nothing
      selectOne ActiveVillain >>= \case
        Nothing -> pure a
        Just villain -> do
          putBoostIntoPlay attrs =<< getActivePlayerId
          attachTo (attachmentId attrs) villain
          pure a
    _ -> RedSkullsLuger <$> runMessage msg attrs
