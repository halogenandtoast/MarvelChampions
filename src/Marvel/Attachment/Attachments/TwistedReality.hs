module Marvel.Attachment.Attachments.TwistedReality (
  twistedReality,
  TwistedReality (..),
) where

import Marvel.Prelude

import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Ability
import Marvel.Modifier
import Marvel.Ref
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query

twistedReality :: AttachmentCard TwistedReality
twistedReality = attachment TwistedReality Cards.twistedReality

newtype TwistedReality = TwistedReality (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Incite 1 (the keyword). Attach to a side scheme, the player revealing it choosing
-- which; with none in play it cannot attach, and is discarded. Forced Interrupt (errata):
-- when the attached side scheme is defeated, deal the first player an encounter card.
instance RunMessage TwistedReality where
  runMessage msg a@(TwistedReality attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment you) | ident == attachmentId attrs -> do
      schemes <- selectList AnySideScheme
      if null schemes
        then removeFromPlay attrs
        else chooseOrRunOne you [targeting s (attachToSideScheme ident s) | s <- schemes]
      pure a
    SideSchemeMessage sid DefeatSideScheme | attachmentSideScheme attrs == Just sid -> do
      first <- selectJust FirstPlayer
      dealEncounterCard first
      pure a
    _ -> TwistedReality <$> runMessage msg attrs
