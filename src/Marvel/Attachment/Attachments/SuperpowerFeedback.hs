module Marvel.Attachment.Attachments.SuperpowerFeedback where
import Marvel.Prelude
import Marvel.Card
import Marvel.Game.Source
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Domino
import Marvel.Deck
import Marvel.Discard
import Marvel.Hand
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Resource.Types
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types

superpowerFeedback :: AttachmentCard SuperpowerFeedback
superpowerFeedback = attachment SuperpowerFeedback Cards.superpowerFeedback
newtype SuperpowerFeedback = SuperpowerFeedback (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities SuperpowerFeedback where
  getAbilities a@(SuperpowerFeedback attrs) = case attachmentIdentity attrs of
    Nothing -> []
    Just you ->
      [ windowAbility a 1 (W.ResolvesAbility W.After (IdentityWithId you)
          (CardMatchesAny [HeroSpecificCard, CardWithType HeroType, CardWithType AlterEgoType]))
          ForcedResponse NoCost (runAbility a 1)
      , ability a 2 AlterEgoAction (SelfMatches $ IdentityWithId you)
          (DiscardMatchingHandCardCost HeroSpecificCard) (runAbility a 2)
      ]

instance RunMessage SuperpowerFeedback where
  runMessage msg a@(SuperpowerFeedback attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      attachToIdentity aid you
      pure a
    RanAbility you (isTarget attrs -> True) 1 _ _ -> a <$ damageIdentity you attrs (toDamage 1 FromAbility)
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> a <$ removeFromPlay attrs
    _ -> SuperpowerFeedback <$> runMessage msg attrs
