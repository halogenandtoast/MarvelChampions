module Marvel.Attachment.Attachments.PhotographicReflexes (
  photographicReflexes,
  PhotographicReflexes (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Damage
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue ()
import Marvel.Ref
import Marvel.Window qualified as W

photographicReflexes :: AttachmentCard PhotographicReflexes
photographicReflexes = attachment PhotographicReflexes Cards.photographicReflexes

newtype PhotographicReflexes = PhotographicReflexes (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Forced Interrupt: when a player attacks Taskmaster, prevent all damage that would be
-- dealt to Taskmaster and deal an equal amount of damage to that player's identity
-- instead. Then, discard Photographic Reflexes. (Max once per attack.)" A player's
-- attack is the hero's, by basic power or card; an ally is not the player.
instance HasAbilities PhotographicReflexes where
  getAbilities (PhotographicReflexes a) = case attachmentEnemy a of
    Just (EnemyVillainId vid) ->
      [ windowAbility a 1 (W.VillainDamagedBy W.When (VillainWithId vid) (AttackFromPlayer AnyIdentity)) ForcedInterrupt NoCost $
          RunAbility (toTarget a) 1
      ]
    _ -> []

instance RunMessage PhotographicReflexes where
  runMessage msg a@(PhotographicReflexes attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      -- with no villain in play to attach to, it is discarded
      selectOne ActiveVillain >>= \case
        Nothing -> PhotographicReflexes attrs <$ removeFromPlay attrs
        Just villainId -> do
          attachedTo villainId ident
          pure . PhotographicReflexes $ attrs & enemyL ?~ EnemyVillainId villainId
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [(vid, p, damageAmount dmg) | W.DamagedVillain vid dmg <- windows, FromPlayerAttack p <- [damageSource dmg]] \(vid, p, n) -> do
        let theDamage = \case
              VillainMessage vid' (VillainDamaged _ _) -> vid' == vid
              _ -> False
        -- another copy may have prevented it already: once per attack
        pending <- isQueued theDamage
        when pending do
          insteadOf theDamage $ damageIdentity p attrs (toDamage n FromAbility)
          removeFromPlay attrs
      pure a
    _ -> PhotographicReflexes <$> runMessage msg attrs
