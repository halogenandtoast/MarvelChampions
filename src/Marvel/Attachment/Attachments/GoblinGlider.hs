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
import Marvel.Entity
import Marvel.Criteria
import Marvel.Game.Source
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

-- | +1 ATK to the enemy it is attached to.
instance HasModifiersFor GoblinGlider where
  getModifiersFor _ target (GoblinGlider a) = pure case (target, attachmentEnemy a) of
    (VillainRef vid, Just (EnemyVillainId vid')) | vid == vid' -> [AttackModifier 1]
    (MinionRef mid, Just (EnemyMinionId mid')) | mid == mid' -> [AttackModifier 1]
    _ -> []

-- | Hero Action: spend [energy] [energy] resources → discard this card.
instance HasAbilities GoblinGlider where
  getAbilities (GoblinGlider a) = case attachmentEnemy a of
    Just _ -> [ability a 1 HeroAction NoCriteria (MultiResourceCost [Just Energy, Just Energy]) $ DiscardTarget (toRef a)]
    Nothing -> []

-- | Attach to the enemy with the highest printed hit points without another Goblin
-- Glider, the player choosing among those tied; with none, it is discarded and surges.
instance RunMessage GoblinGlider where
  runMessage msg a@(GoblinGlider attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment you) | ident == attachmentId attrs -> do
      candidates <- selectList $ NotEnemy (EnemyWithAttachment $ AttachmentIs Cards.goblinGlider)
      hps <- traverse (\e -> (e,) <$> getEnemyPrintedHitPoints e) candidates
      let highest = maximum (0 : map snd hps)
      case [e | (e, hp) <- hps, hp == highest] of
        [] -> do
          removeFromPlay attrs
          surge you
        es -> chooseOrRunOne you [targeting e (attachTo ident e) | e <- es]
      pure a
    _ -> GoblinGlider <$> runMessage msg attrs
