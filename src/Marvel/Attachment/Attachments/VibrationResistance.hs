module Marvel.Attachment.Attachments.VibrationResistance (
  vibrationResistance,
  VibrationResistance (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity ()
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref

vibrationResistance :: AttachmentCard VibrationResistance
vibrationResistance = attachment VibrationResistance Cards.vibrationResistance

newtype VibrationResistance = VibrationResistance (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Reduce the damage attached enemy takes from each attack by 1.
instance HasModifiersFor VibrationResistance where
  getModifiersFor _ target (VibrationResistance a)
    | Just enemy <- attachmentEnemy a, isEnemy enemy = pure [AttackDamageReduction 1]
   where
    isEnemy = \case
      EnemyVillainId vid -> target == VillainRef vid
      EnemyMinionId mid -> target == MinionRef mid
      EnemyAttachmentId aid -> target == AttachmentRef aid
  getModifiersFor _ _ _ = pure []

-- | Hero Action: exhaust your hero → discard this card.
instance HasAbilities VibrationResistance where
  getAbilities (VibrationResistance a) = case attachmentEnemy a of
    Just _ -> [ability a 1 HeroAction NoCriteria ExhaustCost $ DiscardTarget (toRef a)]
    Nothing -> []

-- | Attach to Avalanche, if able. If you cannot, attach to the villain.
instance RunMessage VibrationResistance where
  runMessage msg (VibrationResistance attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      avalanche <- selectOne $ MinionIs Minions.avalanche
      villain <- selectOne ActiveVillain
      case (EnemyMinionId <$> avalanche) <|> (EnemyVillainId <$> villain) of
        Nothing -> VibrationResistance attrs <$ removeFromPlay attrs
        Just enemy -> do
          attachedTo enemy ident
          pure . VibrationResistance $ attrs & enemyL ?~ enemy
    _ -> VibrationResistance <$> runMessage msg attrs
