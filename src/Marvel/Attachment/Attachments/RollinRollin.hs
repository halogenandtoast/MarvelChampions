module Marvel.Attachment.Attachments.RollinRollin where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Status

rollinRollin :: AttachmentCard RollinRollin
rollinRollin = attachment RollinRollin Cards.rollinRollin

newtype RollinRollin = RollinRollin (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor RollinRollin where
  getModifiersFor _ (MinionRef mid) (RollinRollin attrs)
    | attachmentEnemy attrs == Just (EnemyMinionId mid) = do
        tough <- characterMatches (CharacterWithStatus ToughStatus) (MinionCharacter mid)
        pure $ AttackModifier 2 : [AttacksCannotBeDefended | tough]
  getModifiersFor _ _ _ = pure []

instance RunMessage RollinRollin where
  runMessage msg a@(RollinRollin attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      armadillo <- selectOne (MinionIs Minions.armadillo)
      case armadillo of
        Just mid -> attachTo aid mid
        Nothing -> do
          searchAndPutIntoPlayEncounterCard you (CardIs Minions.armadillo)
          cardSpecific attrs "attach" ()
      pure a
    CardSpecific (isTarget attrs -> True) "attach" _ -> do
      armadillo <- selectOne (MinionIs Minions.armadillo)
      case armadillo of
        Just mid -> attachTo (attachmentId attrs) mid
        Nothing -> removeFromPlay attrs
      pure a
    _ -> RollinRollin <$> runMessage msg attrs
