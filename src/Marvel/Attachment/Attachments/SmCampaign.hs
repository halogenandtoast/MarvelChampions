module Marvel.Attachment.Attachments.SmCampaign where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Allies
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria (Criteria (NoCriteria))
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Resource (Resource (..))
import Marvel.Window qualified as W

newtype SmCampaignAttachment = SmCampaignAttachment (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

smSnitchesGetStitches :: AttachmentCard SmCampaignAttachment
smSnitchesGetStitches = attachment SmCampaignAttachment Cards.smSnitchesGetStitches

instance HasAbilities SmCampaignAttachment where
  getAbilities a@(SmCampaignAttachment attrs) = case attachmentAlly attrs of
    Nothing -> []
    Just aid ->
      [ windowAbility a 1 (W.EnemyAttacked W.When VillainEnemy AnyIdentity) ForcedInterrupt NoCost (runAbility a 1)
      , windowAbility a 2 (W.AllyDefeatedByAttackOfEnemy W.When (AllyWithId aid) VillainEnemy) ForcedInterrupt NoCost (runAbility a 2)
      ] <> [ability a (10 + i) Action NoCriteria
          (ExhaustAlliesMatchingCost (AllyWithId aid) 1 <> MultiResourceCost [Just resource, Just resource]) (runAbility a $ 10 + i)
        | (i, resource) <- zip [0 ..] [Energy, Mental, Physical, Wild]]

instance RunMessage SmCampaignAttachment where
  runMessage msg a@(SmCampaignAttachment attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      venoms <- selectList (AllyIs Allies.venomEddieBrock)
      case venoms of
        venom : _ -> attachToAlly aid venom
        [] -> surge you >> removeFromPlay attrs
      pure a
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ (attachmentAlly attrs) $ \aid ->
        for_ [vid | W.EnemyAttack (EnemyVillainId vid) _ <- windows] $ \vid -> redirectVillainAttackToAlly vid aid
      pure a
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> do
      addToVictoryDisplay attrs
      traverse_ addToVictoryDisplay (attachmentAlly attrs)
      pure a
    RanAbility _ (isTarget attrs -> True) n _ _ | n >= 10 -> a <$ removeFromPlay attrs
    _ -> SmCampaignAttachment <$> runMessage msg attrs
