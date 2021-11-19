module Marvel.Attachment.Attachments.ArmoredRhinoSuit where

import Marvel.Prelude

import Marvel.Attachment.Attrs
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Queue
import Marvel.Source
import Marvel.Target

armoredRhinoSuit :: AttachmentCard ArmoredRhinoSuit
armoredRhinoSuit = attachment ArmoredRhinoSuit Cards.armoredRhinoSuit

newtype ArmoredRhinoSuit = ArmoredRhinoSuit AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage ArmoredRhinoSuit where
  runMessage msg a@(ArmoredRhinoSuit attrs) = case msg of
    AttachmentMessage aid msg' | aid == toId attrs -> case msg' of
      RevealAttachment -> do
        villainId <- selectJust ActiveVillain
        push $ VillainMessage villainId $ AttachedToVillain aid
        pure a
    _ -> ArmoredRhinoSuit <$> runMessage msg attrs
