module Marvel.Attachment.Attachments.LaserRifle (
  laserRifle,
  LaserRifle (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Id
import Marvel.Keyword (Keyword (Ranged))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Window

laserRifle :: AttachmentCard LaserRifle
laserRifle = attachment LaserRifle Cards.laserRifle

newtype LaserRifle = LaserRifle (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | +1 ATK to the villain.
instance HasModifiersFor LaserRifle where
  getModifiersFor _ (VillainRef vid) (LaserRifle a)
    | Just (EnemyVillainId vid) == attachmentEnemy a = pure [AttackModifier 1]
  getModifiersFor _ _ _ = pure []

-- | [star] Forced Interrupt: when the attached villain attacks, the attack gains ranged.
-- Hero Action: spend [energy][physical] resources → discard this card.
instance HasAbilities LaserRifle where
  getAbilities (LaserRifle a) = case attachmentEnemy a of
    Just enemy ->
      [ windowAbility a 1 (EnemyAttacked When (EnemyWithId enemy) AnyIdentity) ForcedInterrupt NoCost $ runAbility a 1
      , ability a 2 HeroAction NoCriteria (MultiResourceCost [Just Energy, Just Physical]) $ DiscardTarget (toRef a)
      ]
    Nothing -> []

instance RunMessage LaserRifle where
  runMessage msg a@(LaserRifle attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      villainId <- selectJust ActiveVillain
      attachedTo villainId ident
      pure . LaserRifle $ attrs & enemyL ?~ EnemyVillainId villainId
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      for_ (attachmentEnemy attrs) (`attackGains` Ranged)
      pure a
    _ -> LaserRifle <$> runMessage msg attrs
