module Marvel.Attachment.Attachments.BlackjacksBazooka (
  blackjacksBazooka,
  BlackjacksBazooka (..),
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
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource

blackjacksBazooka :: AttachmentCard BlackjacksBazooka
blackjacksBazooka = attachment BlackjacksBazooka Cards.blackjacksBazooka

newtype BlackjacksBazooka = BlackjacksBazooka (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | +2 ATK to the attached enemy.
instance HasModifiersFor BlackjacksBazooka where
  getModifiersFor _ target (BlackjacksBazooka a) | Just enemy <- attachmentEnemy a, isEnemy enemy = pure [AttackModifier 2]
   where
    isEnemy = \case
      EnemyVillainId vid -> target == VillainRef vid
      EnemyMinionId mid -> target == MinionRef mid
      EnemyAttachmentId aid -> target == AttachmentRef aid
  getModifiersFor _ _ _ = pure []

-- | Hero Action: spend [mental][mental][mental] resources → discard this card.
instance HasAbilities BlackjacksBazooka where
  getAbilities (BlackjacksBazooka a) = case attachmentEnemy a of
    Just _ ->
      [ability a 1 HeroAction NoCriteria (MultiResourceCost [Just Mental, Just Mental, Just Mental]) $ DiscardTarget (toRef a)]
    Nothing -> []

-- | Attach to Blackjack O'Hare. If you cannot, attach to the villain.
instance RunMessage BlackjacksBazooka where
  runMessage msg (BlackjacksBazooka attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      blackjack <- selectOne $ MinionIs Minions.blackjackOHare
      villain <- selectOne ActiveVillain
      case (EnemyMinionId <$> blackjack) <|> (EnemyVillainId <$> villain) of
        Nothing -> BlackjacksBazooka attrs <$ removeFromPlay attrs
        Just enemy -> do
          attachedTo enemy ident
          pure . BlackjacksBazooka $ attrs & enemyL ?~ enemy
    _ -> BlackjacksBazooka <$> runMessage msg attrs
