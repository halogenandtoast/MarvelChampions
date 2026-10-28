-- | The attachments of the Enchantress modular set (The Mad Titan's Shadow).
module Marvel.Attachment.Attachments.Enchantress (
  beguiled,
  Beguiled (..),
  seduced,
  Seduced (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Attachments.AllyAsMinion
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait (Trait (Enthralled))
import Marvel.Trait qualified as Trait

beguiled :: AttachmentCard Beguiled
beguiled = attachment Beguiled Cards.beguiled

-- | Treat attached ally as an Enthralled minion with a blank text box. Attached
-- minion's SCH is equal to its printed THW and it does not take consequential damage
-- (a minion never does).
newtype Beguiled = Beguiled (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Beguiled where
  getModifiersFor _ target (Beguiled a) = allyMinionTrait Enthralled a target

-- | When Revealed: attach to the ally with the highest cost without Beguiled attached
-- (any player's: the revealing player breaks a tie). Attached ally engages its
-- controller: it does not leave play; its damage stays, the cards attached to it are
-- discarded. Otherwise, this card gains surge. An ally that cannot have (encounter)
-- cards attached is not one it can attach to.
instance RunMessage Beguiled where
  runMessage msg a@(Beguiled attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment you) | ident == attachmentId attrs -> do
      allies <- filterM attachable =<< selectList AnyAlly
      costed <- for allies \aid -> (aid,) . fromMaybe 0 . (cdCost =<<) <$> getsGame (`abilityCardDef` AllyRef aid)
      let highest = maximum (map snd costed)
          tied = [aid | (aid, c) <- costed, c == highest]
          enthrall aid = do
            allyBecomesMinion aid
            attachTo ident (EnemyMinionId $ MinionId $ unAllyId aid)
      if null allies
        then do
          surge you
          removeFromPlay attrs
        else chooseOrRunOne you [targeting (AllyRef aid) (enthrall aid) | aid <- tied]
      pure a
    _ -> do
      revertsToAlly attrs msg
      Beguiled <$> runMessage msg attrs
   where
    attachable aid = do
      modifiers <- getModifiers (AllyRef aid)
      pure $ CannotHaveAttachedCards `notElem` modifiers && CannotHaveAttachedEncounterCards `notElem` modifiers

seduced :: AttachmentCard Seduced
seduced = attachment Seduced Cards.seduced

newtype Seduced = Seduced (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | You cannot make basic attacks (nor attempt one to remove a stun) or play Attack
-- events.
instance HasModifiersFor Seduced where
  getModifiersFor _ (IdentityRef ident) (Seduced a)
    | attachmentIdentity a == Just ident =
        pure [CannotBasicAttack, CannotPlayCards (CardWithType EventType <> CardWithTrait Trait.Attack)]
  getModifiersFor _ _ _ = pure []

-- | Alter-Ego Action: spend [energy] [mental] resources → discard this card.
instance HasAbilities Seduced where
  getAbilities (Seduced a) = case attachmentIdentity a of
    Just _ -> [ability a 1 AlterEgoAction NoCriteria (MultiResourceCost [Just Energy, Just Mental]) $ DiscardTarget (toRef a)]
    Nothing -> []

-- | Attach to your identity.
instance RunMessage Seduced where
  runMessage msg a@(Seduced attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment you) | ident == attachmentId attrs -> do
      attachToIdentity ident you
      pure a
    _ -> Seduced <$> runMessage msg attrs
