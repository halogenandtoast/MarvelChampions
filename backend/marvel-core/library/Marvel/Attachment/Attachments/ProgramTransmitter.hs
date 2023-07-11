module Marvel.Attachment.Attachments.ProgramTransmitter (
  programTransmitter,
  ProgramTransmitter (..),
)
where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Resource
import Marvel.Window qualified as W

programTransmitter :: AttachmentCard ProgramTransmitter
programTransmitter = attachment ProgramTransmitter Cards.programTransmitter

newtype ProgramTransmitter = ProgramTransmitter (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities ProgramTransmitter where
  getAbilities (ProgramTransmitter a) = case attachmentEnemy a of
    Just (EnemyVillainId vid) ->
      [ windowAbility a 1 (W.VillainSchemed W.After $ VillainWithId vid) ForcedResponse NoCost $ runAbility a 1
      , ability a 2 HeroAction NoCriteria (MultiResourceCost [Just Mental, Just Mental] <> ExhaustCost) (TargetLabel (toRef a) [DiscardTarget $ toRef a])
      ]
    _ -> []

instance RunMessage ProgramTransmitter where
  runMessage msg a@(ProgramTransmitter attrs) = case msg of
    AttachmentMessage ident msg' | ident == attachmentId attrs -> case msg' of
      RevealAttachment _ -> do
        villainId <- selectJust ActiveVillain
        push $ VillainMessage villainId $ AttachedToVillain ident
        pure . ProgramTransmitter $ attrs & enemyL ?~ EnemyVillainId villainId
      _ -> ProgramTransmitter <$> runMessage msg attrs
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      sideSchemes <- selectList AnySideScheme
      pushAll [SideSchemeMessage sideScheme $ SideSchemePlaceThreat 1 | sideScheme <- sideSchemes]
      pure a
    _ -> ProgramTransmitter <$> runMessage msg attrs
