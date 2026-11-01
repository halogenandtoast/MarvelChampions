module Marvel.Attachment.Attachments.Cybermods where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types

cybermods :: AttachmentCard Cybermods
cybermods = attachment (\a -> Cybermods (a `With` Nothing)) Cards.cybermods

newtype Cybermods = Cybermods (Attrs Attachment `With` Maybe IdentityId)
  deriving anyclass HasModifiersFor
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsAttachment Cybermods where
  toAttachmentAttrs (Cybermods (attrs `With` _)) = attrs

instance HasAbilities Cybermods where
  getAbilities a@(Cybermods (attrs `With` _)) =
    [ windowAbility a 1 (W.CardWithIdWouldBeDiscardedFromPlay $ CardId $ unMinionId mid) ForcedInterrupt NoCost (runAbility a 1)
    | Just (EnemyMinionId mid) <- [attachmentEnemy attrs]
    ]

instance RunMessage Cybermods where
  runMessage msg a@(Cybermods (attrs `With` resolver)) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      enemies <- selectList (EnemyWithTitle "Lady Deathstrike")
      let minions = [mid | EnemyMinionId mid <- enemies]
      if null minions then do
        discardUntil FromEncounterDeck (CardWithType MinionType) attrs
        cardSpecific attrs "checkAttached" ()
      else chooseOrRunOne you [targeting mid (attachTo aid mid) | mid <- minions]
      pure $ Cybermods (attrs `With` Just you)
    WithDiscardedMatch (isTarget attrs -> True) FromEncounterDeck (EncounterCard card) -> do
      for_ resolver $ \you -> do
        putEncounterCardIntoPlay you card
        cardSpecific attrs "attach" (MinionId $ unCardId $ ecCardId card)
      pure a
    CardSpecific (isTarget attrs -> True) "attach" (fromJSON -> Success mid) -> do
      inPlay <- selectAny (MinionWithId mid)
      when inPlay $ attachTo (attachmentId attrs) (EnemyMinionId mid)
      pure a
    CardSpecific (isTarget attrs -> True) "checkAttached" _ -> do
      unless (isJust $ attachmentEnemy attrs) $ removeFromPlay attrs
      pure a
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [card | W.WouldBeDiscardedFromPlay (Right card) <- windows
                , Just (EnemyMinionId $ MinionId $ unCardId $ ecCardId card) == attachmentEnemy attrs] $ \card ->
        insteadOf (== DiscardedCard (EncounterCard card)) (shuffleIntoEncounterDeck [card])
      pure a
    _ -> (\attrs' -> Cybermods (attrs' `With` resolver)) <$> runMessage msg attrs
