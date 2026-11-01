module Marvel.Attachment.Attachments.TabulaRasa16 where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source
import Marvel.Matchers ()
import Marvel.Resource
import Marvel.Question
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types

tabulaRasa16 :: AttachmentCard TabulaRasa16
tabulaRasa16 = attachment TabulaRasa16 Cards.tabulaRasa16

newtype TabulaRasa16 = TabulaRasa16 (Attrs Attachment)
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor TabulaRasa16 where
  getBlankingModifiersFor _ (IdentityRef you) (TabulaRasa16 attrs) = pure [Blank | attachmentIdentity attrs == Just you]
  getBlankingModifiersFor _ _ _ = pure []
  getModifiersFor source target card = getBlankingModifiersFor source target card

instance HasAbilities TabulaRasa16 where
  getAbilities (TabulaRasa16 attrs) =
    [ability attrs 1 AlterEgoAction NoCriteria (MultiResourceCost [Just Mental, Just Mental]) (runAbility attrs 1)]

instance RunMessage TabulaRasa16 where
  runMessage msg a@(TabulaRasa16 attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> a <$ attachToIdentity aid you
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      putBoostIntoPlay attrs you
      attachToIdentity (attachmentId attrs) you
      pure a
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> a <$ removeFromPlay attrs
    _ -> TabulaRasa16 <$> runMessage msg attrs
