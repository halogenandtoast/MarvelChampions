module Marvel.Attachment.Attachments.AgeOfApocalypseGenosha where

import Marvel.Prelude
import Marvel.Window ()
import Marvel.Query ()
import Marvel.Keyword ()
import Marvel.Id ()
import Marvel.Game.Source ()
import Marvel.Ability
import Marvel.AgeOfApocalypse.Settings ()
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Entity
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Trait qualified as Trait

newtype AoaGenoshaAttachment = AoaGenoshaAttachment (Attrs Attachment)
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

escapedMutant :: AttachmentCard AoaGenoshaAttachment
escapedMutant = attachment AoaGenoshaAttachment Cards.escapedMutant

instance HasModifiersFor AoaGenoshaAttachment where
  getModifiersFor _ (MinionRef mid) (AoaGenoshaAttachment attrs) | Just you <- attachmentIdentity attrs = do
    genosha <- minionMatches (MinionWithTrait Trait.Genosha) mid
    pure [QuickstrikeWhenEngaging you | genosha]
  getModifiersFor _ _ _ = pure []

instance HasAbilities AoaGenoshaAttachment where
  getAbilities a@(AoaGenoshaAttachment attrs) =
    [ability a 1 AlterEgoAction (SelfMatches (IdentityWithId you) <> CanResolveSettingSpecial)
      ResolveSettingSpecialCost (runAbility a 1) | Just you <- [attachmentIdentity attrs]]

instance RunMessage AoaGenoshaAttachment where
  runMessage msg e@(AoaGenoshaAttachment attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> e <$ attachToIdentity aid you
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> e <$ removeFromPlay attrs
    _ -> AoaGenoshaAttachment <$> runMessage msg attrs
