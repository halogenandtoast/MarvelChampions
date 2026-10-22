module Marvel.Attachment.Attachments.GoblinGlider (
  goblinGlider,
  GoblinGlider (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource

goblinGlider :: AttachmentCard GoblinGlider
goblinGlider = attachment GoblinGlider Cards.goblinGlider

newtype GoblinGlider = GoblinGlider (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | The enemy it is attached to gets +1 ATK.
instance HasModifiersFor GoblinGlider where
  getModifiersFor _ target (GoblinGlider a) = pure $ case (target, attachmentEnemy a) of
    (VillainRef vid, Just (EnemyVillainId vid')) | vid == vid' -> [AttackModifier 1]
    (MinionRef mid, Just (EnemyMinionId mid')) | mid == mid' -> [AttackModifier 1]
    _ -> []

instance HasAbilities GoblinGlider where
  getAbilities (GoblinGlider a) = [ability a 1 HeroAction NoCriteria (MultiResourceCost [Just Energy, Just Energy]) $ DiscardTarget (toRef a) | isJust (attachmentEnemy a)]

-- | Attached to the enemy with the highest printed hit points, without another Goblin
-- Glider; with none, it gains surge.
instance RunMessage GoblinGlider where
  runMessage msg a@(GoblinGlider attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment you) | ident == attachmentId attrs -> do
      enemies <- selectList (EnemyWithHighestPrintedHitPointsAmong $ NotEnemy $ EnemyWithAttachment $ AttachmentIs Cards.goblinGlider)
      if null enemies
        then do
          removeFromPlay attrs
          surge you
        else chooseOrRunOne you [targeting enemy (attachTo ident enemy) | enemy <- enemies]
      pure a
    _ -> GoblinGlider <$> runMessage msg attrs
