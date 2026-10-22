module Marvel.Attachment.Attachments.TemporalShield (
  temporalShield,
  TemporalShield (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Damage
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Queue (isQueued)
import Marvel.Question
import Marvel.Entity
import Marvel.Ref
import Marvel.Window qualified as W

temporalShield :: AttachmentCard TemporalShield
temporalShield = attachment TemporalShield Cards.temporalShield

newtype TemporalShield = TemporalShield (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Attach to Kang. Forced Interrupt: when Kang is attacked, discard Temporal Shield →
-- prevent all damage from this attack and deal 1 damage to the attacker. (Max 1 per
-- attack.)
instance HasAbilities TemporalShield where
  getAbilities (TemporalShield a) = case attachmentEnemy a of
    Just (EnemyVillainId vid) ->
      [windowAbility a 1 (W.VillainDamagedBy W.When (VillainWithId vid) AnyAttack) ForcedInterrupt NoCost $ runAbility a 1]
    _ -> []

instance RunMessage TemporalShield where
  runMessage msg a@(TemporalShield attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> a <$ attachToTheVillain attrs
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [(vid, d) | W.DamagedVillain vid d <- windows] \(vid, d) -> do
        let damaged = \case
              VillainMessage vid' (VillainDamaged _ _) -> vid' == vid
              _ -> False
        -- the attack's damage is still to come: no other shield has prevented it
        pending <- isQueued damaged
        when pending do
          removeFromPlay attrs
          insteadOf damaged $ for_ (attacker $ damageSource d) \c -> damageCharacter c attrs (toDamage 1 FromAbility)
      pure a
    _ -> TemporalShield <$> runMessage msg attrs
   where
    attacker = \case
      FromPlayerAttack ident -> Just (IdentityCharacter ident)
      FromAllyAttack ally -> Just (AllyCharacter ally)
      _ -> Nothing
