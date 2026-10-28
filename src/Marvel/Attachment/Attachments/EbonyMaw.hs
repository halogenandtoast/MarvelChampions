-- | The attachments of Ebony Maw's set (The Mad Titan's Shadow, scenario 1).
module Marvel.Attachment.Attachments.EbonyMaw (
  abjuration,
  Abjuration (..),
  restrained,
  Restrained (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource

abjuration :: AttachmentCard Abjuration
abjuration = attachment Abjuration Cards.abjuration

newtype Abjuration = Abjuration (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Prevent all damage to Ebony Maw: before his tough status card would.
instance HasModifiersFor Abjuration where
  getModifiersFor _ target (Abjuration a)
    | Just enemy <- attachmentEnemy a, target == enemyRef enemy = pure [CannotTakeDamage]
  getModifiersFor _ _ _ = pure []

-- | Attach to Ebony Maw. Forced Response: after Abjuration prevents 2 or more damage
-- from a single attack, discard it: a hero's or an ally's attack, or what its overkill
-- deals past a minion it defeated.
instance RunMessage Abjuration where
  runMessage msg a@(Abjuration attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      attachToTheVillain attrs
      pure a
    VillainMessage vid (VillainDamaged _ d)
      | attachmentEnemy attrs == Just (EnemyVillainId vid)
      , damageAmount d >= 2
      , fromAttack (damageSource d) -> do
          removeFromPlay attrs
          pure a
    _ -> Abjuration <$> runMessage msg attrs
   where
    fromAttack = \case
      FromPlayerAttack _ -> True
      FromAllyAttack _ -> True
      FromOverkill -> True
      _ -> False

restrained :: AttachmentCard Restrained
restrained = attachment Restrained Cards.restrained

newtype Restrained = Restrained (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Attached character cannot ready.
instance HasModifiersFor Restrained where
  getModifiersFor _ target (Restrained a)
    | Just target `elem` [IdentityRef <$> attachmentIdentity a, AllyRef <$> attachmentAlly a] = pure [CannotReady]
  getModifiersFor _ _ _ = pure []

-- | Hero Action: spend [energy] [physical] resources → discard this card. Only the
-- attached character's controller may (RR Attachment).
instance HasAbilities Restrained where
  getAbilities (Restrained a)
    | attached a = [ability a 1 HeroAction NoCriteria (MultiResourceCost [Just Energy, Just Physical]) $ DiscardTarget (toRef a)]
    | otherwise = []

-- | Attach to a friendly character with the highest ATK (the revealing player decides
-- ties) and exhaust it. With no hero or ally in play, it is discarded.
instance RunMessage Restrained where
  runMessage msg a@(Restrained attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment you) | ident == attachmentId attrs -> do
      characters <- selectList AttachableHeroOrAllyWithHighestAttack
      let restrain c = do
            attachToCharacter ident c
            case c of
              IdentityCharacter i -> exhaust i
              AllyCharacter ally -> exhaust ally
              _ -> pure ()
      case characters of
        [] -> removeFromPlay attrs
        [c] -> restrain c
        _ -> chooseOne you [targeting (characterRef c) (restrain c) | c <- characters]
      pure a
    _ -> Restrained <$> runMessage msg attrs
