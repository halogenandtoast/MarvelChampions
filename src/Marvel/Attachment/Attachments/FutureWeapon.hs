module Marvel.Attachment.Attachments.FutureWeapon (
  futureWeapon,
  FutureWeapon (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Entity
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Question
import Marvel.Window

futureWeapon :: AttachmentCard FutureWeapon
futureWeapon = attachment (FutureWeapon . (`With` Meta False)) Cards.futureWeapon

-- | Whether Kang's attack under way is the one it armed.
newtype Meta = Meta {armed :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype FutureWeapon = FutureWeapon (Attrs Attachment `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsAttachment FutureWeapon where
  toAttachmentAttrs (FutureWeapon (a `With` _)) = a

-- | Attach to Kang: +2 ATK.
instance HasModifiersFor FutureWeapon where
  getModifiersFor _ (VillainRef vid) (FutureWeapon (a `With` _))
    | Just (EnemyVillainId vid) == attachmentEnemy a = pure [AttackModifier 2]
  getModifiersFor _ _ _ = pure []

-- | [star] Forced Interrupt: when Kang attacks, the attack gains overkill. If this
-- attack damages a hero, that hero is stunned. After this attack, discard Future Weapon.
instance HasAbilities FutureWeapon where
  getAbilities (FutureWeapon (a `With` _)) = case attachmentEnemy a of
    Just enemy -> [windowAbility a 1 (EnemyAttacked When (EnemyWithId enemy) AnyIdentity) ForcedInterrupt NoCost $ runAbility a 1]
    _ -> []

instance RunMessage FutureWeapon where
  runMessage msg w@(FutureWeapon (attrs `With` meta)) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> w <$ attachToTheVillain attrs
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> case attachmentEnemy attrs of
      Just (EnemyVillainId vid) -> do
        attackGains vid Overkill
        flip afterMatching (removeFromPlay attrs) \case
          VillainMessage vid' VillainEndAttack -> vid == vid'
          _ -> False
        pure . FutureWeapon $ attrs `With` Meta True
      _ -> pure w
    (attackDamages -> Just (enemy, IdentityCharacter hero))
      | armed meta, Just enemy == attachmentEnemy attrs -> w <$ stunIdentity hero
    _ -> FutureWeapon . (`With` meta) <$> runMessage msg attrs
