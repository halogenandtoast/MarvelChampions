-- | The attachment of the Legions of Hel modular set (The Mad Titan's Shadow).
module Marvel.Attachment.Attachments.LegionsOfHel (
  fallenWarrior,
  FallenWarrior (..),
) where

import Marvel.Prelude

import Data.Aeson (Result (..), fromJSON)
import Marvel.Ability
import Marvel.Attachment.Attachments.AllyAsMinion
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Entity (isTarget)
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Trait (Trait (Undead))

fallenWarrior :: AttachmentCard FallenWarrior
fallenWarrior = attachment FallenWarrior Cards.fallenWarrior

-- | Treat attached ally as an Undead minion with a blank text box. Attached minion's SCH
-- is equal to its printed THW and it does not take consequential damage (a minion
-- never does).
newtype FallenWarrior = FallenWarrior (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor FallenWarrior where
  getModifiersFor _ target (FallenWarrior a) = allyMinionTrait Undead a target

-- | When Revealed: discard cards from the top of your deck until you discard an ally.
-- Put that ally into play engaged with you with Fallen Warrior attached to it. With no
-- ally before the deck runs out, or the ally kept out by a unique card in play, it is
-- discarded with nothing attached.
instance RunMessage FallenWarrior where
  runMessage msg a@(FallenWarrior attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment you) | ident == attachmentId attrs -> do
      discardFromDeckUntil you (CardWithType AllyType) attrs
      cardSpecific attrs "attached" ()
      pure a
    WithDiscardedMatch (isTarget attrs -> True) FromDeck (PlayerCard ally) -> do
      you <- maybe getActivePlayerId pure (pcOwner ally)
      putAllyIntoPlayAsMinion you ally
      cardSpecific attrs "attach" (MinionId $ unCardId $ pcCardId ally)
      pure a
    CardSpecific (isTarget attrs -> True) "attach" (fromJSON -> Success mid) -> do
      inPlay <- selectAny (MinionWithId mid)
      when inPlay $ attachTo (attachmentId attrs) (EnemyMinionId mid)
      pure a
    CardSpecific (isTarget attrs -> True) "attached" _ -> do
      unless (isJust $ attachmentEnemy attrs) $ removeFromPlay attrs
      pure a
    _ -> do
      revertsToAlly attrs msg
      FallenWarrior <$> runMessage msg attrs
