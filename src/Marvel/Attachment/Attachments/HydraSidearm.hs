module Marvel.Attachment.Attachments.HydraSidearm (
  hydraSidearm,
  HydraSidearm (..),
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

hydraSidearm :: AttachmentCard HydraSidearm
hydraSidearm = attachment HydraSidearm Cards.hydraSidearm

newtype HydraSidearm = HydraSidearm (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | +1 ATK to the villain.
instance HasModifiersFor HydraSidearm where
  getModifiersFor _ (VillainRef vid) (HydraSidearm a)
    | Just (EnemyVillainId vid) == attachmentEnemy a = pure [AttackModifier 1]
  getModifiersFor _ _ _ = pure []

-- | [star] Forced Interrupt: when the attached villain attacks, the attack gains ranged.
-- Hero Action: spend [mental][physical] resources → discard this card.
instance HasAbilities HydraSidearm where
  getAbilities (HydraSidearm a) = case attachmentEnemy a of
    Just enemy ->
      [ windowAbility a 1 (EnemyAttacked When (EnemyWithId enemy) AnyIdentity) ForcedInterrupt NoCost $ runAbility a 1
      , ability a 2 HeroAction NoCriteria (MultiResourceCost [Just Mental, Just Physical]) $ DiscardTarget (toRef a)
      ]
    Nothing -> []

instance RunMessage HydraSidearm where
  runMessage msg a@(HydraSidearm attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      villainId <- selectJust ActiveVillain
      attachedTo villainId ident
      pure . HydraSidearm $ attrs & enemyL ?~ EnemyVillainId villainId
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      for_ (attachmentEnemy attrs) (`attackGains` Ranged)
      pure a
    _ -> HydraSidearm <$> runMessage msg attrs
