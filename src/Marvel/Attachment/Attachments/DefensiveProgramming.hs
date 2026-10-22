module Marvel.Attachment.Attachments.DefensiveProgramming (
  defensiveProgramming,
  DefensiveProgramming (..),
  attachToMostRemaining,
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

defensiveProgramming :: AttachmentCard DefensiveProgramming
defensiveProgramming = attachment DefensiveProgramming Cards.defensiveProgramming

newtype DefensiveProgramming = DefensiveProgramming (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Attached minion gets +2 hit points and gains guard.
instance HasModifiersFor DefensiveProgramming where
  getModifiersFor _ (MinionRef mid) (DefensiveProgramming a)
    | Just (EnemyMinionId mid) == attachmentEnemy a = pure [HitPointModifier 2, KeywordModifier Guard]
  getModifiersFor _ _ _ = pure []

instance RunMessage DefensiveProgramming where
  runMessage msg (DefensiveProgramming attrs) = runQueueT $ DefensiveProgramming <$> attachToMostRemaining Cards.defensiveProgramming msg attrs

-- | "Attach to the minion with the most remaining hit points without another copy of
-- this attached. If you cannot, this card gains surge": with no such minion it is
-- discarded, and its player surges.
attachToMostRemaining :: (ReverseQueue m) => CardDef -> Message -> Attrs Attachment -> m (Attrs Attachment)
attachToMostRemaining def msg attrs = case msg of
  AttachmentMessage ident (RevealAttachment identityId) | ident == attachmentId attrs -> do
    minions <- selectList $ MinionWithMostRemainingHitPointsAmong $ NotMinion $ MinionWithAttachment (AttachmentIs def)
    if null minions
      then removeFromPlay attrs >> surge identityId
      else chooseOrRunOne identityId [targeting mid (attachTo ident mid) | mid <- minions]
    pure attrs
  _ -> runMessage msg attrs
