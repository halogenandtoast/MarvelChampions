module Marvel.Attachment.Attachments.Haywire where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Resource

haywire :: AttachmentCard Haywire
haywire = attachment Haywire Cards.haywire

newtype Haywire = Haywire (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Haywire where
  getModifiersFor _ (CardIdRef cid) (Haywire attrs) = case attachmentIdentity attrs of
    Nothing -> pure []
    Just you -> do
      hand <- unHand <$> project PlayerIdentityHand you
      pure [PrintedResourcesAs Energy | any ((== cid) . pcCardId) hand]
  getModifiersFor _ _ _ = pure []

instance HasAbilities Haywire where
  getAbilities (Haywire attrs) = case attachmentIdentity attrs of
    Nothing -> []
    Just you ->
      [ (ability attrs 1 HeroAction (SelfMatches $ IdentityWithId you) (DiscardControlledCardCost $ CardWithResource Energy) (runAbility attrs 1)) {abilityLabel = Just "$label.discardEnergyCard"}
      , (ability attrs 2 HeroAction (SelfMatches $ IdentityWithId you) (IndirectDamageCost 2) (runAbility attrs 2)) {abilityLabel = Just "$label.takeTwoIndirectDamage"}
      ]

instance RunMessage Haywire where
  runMessage msg a@(Haywire attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> a <$ attachToIdentity aid you
    RanAbility _ (isTarget attrs -> True) idx _ _ | idx `elem` [1, 2] -> a <$ removeFromPlay attrs
    _ -> Haywire <$> runMessage msg attrs
