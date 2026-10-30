module Marvel.Attachment.Attachments.GobblerGlider where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Trait

gobblerGlider :: AttachmentCard GobblerGlider
gobblerGlider = attachment GobblerGlider Cards.gobblerGlider

newtype GobblerGlider = GobblerGlider (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor GobblerGlider where
  getModifiersFor _ target (GobblerGlider a)
    | Just enemy <- attachmentEnemy a, target == enemyRef enemy =
        pure [AttackModifier 1, SchemeModifier 1, TraitModifier Aerial]
  getModifiersFor _ _ _ = pure []

instance RunMessage GobblerGlider where
  runMessage msg g@(GobblerGlider attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment you) | ident == attachmentId attrs -> do
      preferred <- selectList (MinionIs Minions.theGreenGobbler)
      minions <- if null preferred then selectList AnyMinion else pure preferred
      case minions of
        [] -> removeFromPlay attrs
        _ -> chooseOrRunOne you [targeting m (attachTo ident m) | m <- minions]
      pure g
    _ -> GobblerGlider <$> runMessage msg attrs
