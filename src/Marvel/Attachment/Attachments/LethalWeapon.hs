module Marvel.Attachment.Attachments.LethalWeapon (lethalWeapon, LethalWeapon (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Allies
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
import Marvel.Question (Choice (..))
import Marvel.Query
import Marvel.Ref

lethalWeapon :: AttachmentCard LethalWeapon
lethalWeapon = attachment LethalWeapon Cards.lethalWeapon

newtype LethalWeapon = LethalWeapon (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- The printed attachment supplies +1 ATK, omitted from MarvelCDB's text.
instance HasModifiersFor LethalWeapon where
  getModifiersFor _ ref (LethalWeapon attrs) = pure
    [AttackModifier 1 | case ref of
      AllyRef aid -> attachmentAlly attrs == Just aid
      MinionRef mid -> attachmentEnemy attrs == Just (EnemyMinionId mid)
      VillainRef vid -> attachmentEnemy attrs == Just (EnemyVillainId vid)
      _ -> False]

instance HasAbilities LethalWeapon where
  getAbilities (LethalWeapon attrs) =
    [ability attrs 1 HeroAction NoCriteria (DiscardUpgradeCost $ UpgradeControlledBy You <> UpgradeThatCanBeDiscarded) $ DiscardTarget (toTarget attrs)]

instance RunMessage LethalWeapon where
  runMessage msg a@(LethalWeapon attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      allies <- selectList $ AllyIs Allies.gamoraNebula
      minions <- (<>) <$> selectList (MinionIs Minions.gamoraNemesis) <*> selectList (MinionIs Allies.gamoraNebula)
      let gamoras = map AllyCharacter allies <> map MinionCharacter minions
      case gamoras of
        [] -> selectOne ActiveVillain >>= \case
          Nothing -> removeFromPlay attrs
          Just villain -> attachToCharacter aid (VillainCharacter villain)
        [gamora] -> attachToCharacter aid gamora
        _ -> chooseOne you [targeting (characterRef gamora) (attachToCharacter aid gamora) | gamora <- gamoras]
      pure a
    _ -> LethalWeapon <$> runMessage msg attrs
