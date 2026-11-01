module Marvel.Attachment.Attachments.HarpoonsHarpoon where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost.Types
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Window qualified as W

harpoonsHarpoon :: AttachmentCard HarpoonsHarpoon
harpoonsHarpoon = attachment HarpoonsHarpoon Cards.harpoonsHarpoon

newtype HarpoonsHarpoon = HarpoonsHarpoon (Attrs Attachment)
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor HarpoonsHarpoon where
  getModifiersFor _ (MinionRef mid) (HarpoonsHarpoon attrs)
    | attachmentEnemy attrs == Just (EnemyMinionId mid) = pure [AttackModifier 1]
  getModifiersFor _ _ _ = pure []

instance HasAbilities HarpoonsHarpoon where
  getAbilities a@(HarpoonsHarpoon attrs) =
    [ windowAbility a 1 (W.EnemyAttacked W.After (EnemyWithId enemy) AnyIdentity)
        ForcedResponse NoCost (runAbility a 1)
    | Just enemy <- [attachmentEnemy attrs] ]

instance RunMessage HarpoonsHarpoon where
  runMessage msg a@(HarpoonsHarpoon attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      harpoon <- selectOne $ MinionIs Minions.harpoon
      case harpoon of
        Just mid -> attachTo aid mid
        Nothing -> do
          searchAndRevealEncounterCard you (CardIs Minions.harpoon)
          cardSpecific attrs "attach" you
      pure a
    CardSpecific (isTarget attrs -> True) "attach" (fromJSON -> Success you) -> do
      harpoon <- selectOne $ MinionIs Minions.harpoon
      case harpoon of
        Just mid -> attachTo (attachmentId attrs) mid
        Nothing -> removeFromPlay attrs >> surge you
      pure a
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ windows $ \case
        W.EnemyAttack enemy _ -> getBoostedPlayer enemy >>= \you -> dealIndirectDamage you attrs 2
        W.EnemyAttackAlly enemy _ _ -> getBoostedPlayer enemy >>= \you -> dealIndirectDamage you attrs 2
        _ -> pure ()
      pure a
    _ -> HarpoonsHarpoon <$> runMessage msg attrs
