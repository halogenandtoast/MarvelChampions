module Marvel.Attachment.Attachments.DeathFactor where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Status
import Marvel.Window qualified as W
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types

deathFactor :: AttachmentCard DeathFactor
deathFactor = attachment DeathFactor Cards.deathFactor

newtype DeathFactor = DeathFactor (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities DeathFactor where
  getAbilities a@(DeathFactor attrs) = case attachmentIdentity attrs of
    Nothing -> []
    Just you ->
      [windowAbility a 1 (W.TurnEnds W.After $ IdentityWithId you) ForcedResponse NoCost (runAbility a 1)
      ,windowAbility a 2 (W.UsesRecoveryAt W.When $ IdentityWithId you) AlterEgoInterrupt NoCost (runAbility a 2)]

instance RunMessage DeathFactor where
  runMessage msg e@(DeathFactor attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      attachToIdentity aid you
      pure $ DeathFactor attrs {attachmentIdentity = Just you}
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      for_ (attachmentIdentity attrs) (\you -> damageIdentity you attrs $ toDamage 1 FromAbility)
      pure e
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> do
      for_ (attachmentIdentity attrs) \you -> cancelMatchingMessage (== IdentityMessage you (SideMessage Recovered))
      removeFromPlay attrs
      pure e
    _ -> DeathFactor <$> runMessage msg attrs
