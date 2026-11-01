module Marvel.Attachment.Attachments.BeetleArmorMkIV (
  beetleArmorMkIV,
  BeetleArmorMkIV (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Entity ()
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

beetleArmorMkIV :: AttachmentCard BeetleArmorMkIV
beetleArmorMkIV = attachment BeetleArmorMkIV Cards.beetleArmorMkIV

newtype BeetleArmorMkIV = BeetleArmorMkIV (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Attached character gets +4 hit points.
instance HasModifiersFor BeetleArmorMkIV where
  getModifiersFor _ target (BeetleArmorMkIV a)
    | Just enemy <- attachmentEnemy a, isEnemy enemy = pure [HitPointModifier 4]
   where
    isEnemy = \case
      EnemyVillainId vid -> target == VillainRef vid
      EnemyMinionId mid -> target == MinionRef mid
  getModifiersFor _ _ _ = pure []

-- | Attach to Beetle, if able. If you cannot, attach to the villain. A villain's hit
-- points are counted down, so his are gained as it attaches and lost as it leaves with
-- him still in play (as Distracting Taunts).
instance RunMessage BeetleArmorMkIV where
  runMessage msg (BeetleArmorMkIV attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      beetle <- selectOne $ MinionIs Minions.beetle
      villain <- selectOne ActiveVillain
      case (EnemyMinionId <$> beetle) <|> (EnemyVillainId <$> villain) of
        Nothing -> BeetleArmorMkIV attrs <$ removeFromPlay attrs
        Just enemy -> do
          attachedTo enemy ident
          case enemy of
            EnemyVillainId v -> changeVillainHitPoints v 4
            EnemyMinionId _ -> pure ()
          pure . BeetleArmorMkIV $ attrs & enemyL ?~ enemy
    RemoveFromPlay target | target == toTarget attrs -> do
      for_ (attachmentEnemy attrs) \case
        EnemyVillainId v -> do
          inPlay <- selectAny (VillainWithId v)
          when inPlay $ changeVillainHitPoints v (-4)
        EnemyMinionId _ -> pure ()
      BeetleArmorMkIV <$> runMessage msg attrs
    _ -> BeetleArmorMkIV <$> runMessage msg attrs
