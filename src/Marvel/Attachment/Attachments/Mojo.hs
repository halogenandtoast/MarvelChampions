module Marvel.Attachment.Attachments.Mojo where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Window qualified as W

newtype MojoAttachment = MojoAttachment (Attrs Attachment `With` Natural)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

mojo39027, mojo39028 :: AttachmentCard MojoAttachment
mojo39027 = attachment (\a -> MojoAttachment (a `With` 0)) Cards.mojo39027
mojo39028 = attachment (\a -> MojoAttachment (a `With` 0)) Cards.mojo39028

instance IsAttachment MojoAttachment where
  toAttachmentAttrs (MojoAttachment (a `With` _)) = a

instance HasModifiersFor MojoAttachment where
  getModifiersFor _ target (MojoAttachment (a `With` _))
    | Just enemy <- attachmentEnemy a, target == enemyRef enemy = pure $
      [AttackModifier 1] <> if toCardCode a == "39027" then [SchemeModifier 1] else [KeywordModifier $ Retaliate 2]
  getModifiersFor _ _ _ = pure []

instance HasAbilities MojoAttachment where
  getAbilities (MojoAttachment (a `With` _)) = case attachmentEnemy a of
    Just (EnemyVillainId vid) | toCardCode a == "39028" ->
      [windowAbility a 1 (W.VillainWouldTakeDamage (VillainWithId vid) AnyDamage) ForcedInterrupt NoCost (runAbility a 1)]
    Just (EnemyVillainId vid) ->
      [ability a 2 HeroAction NoCriteria (MultiResourceCost $ map Just [Energy, Mental, Physical]) (DiscardTarget $ toRef a)
      ,windowAbility a 3 (W.EnemyAttackedAnyTarget W.After (EnemyWithId $ EnemyVillainId vid)) ForcedResponse NoCost (runAbility a 3)]
    _ -> []

instance RunMessage MojoAttachment where
  runMessage msg e@(MojoAttachment (a `With` dealt)) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment _) | aid == attachmentId a -> e <$ attachToTheVillain a
    AttachmentMessage aid (AttachmentDamaged n) | aid == attachmentId a -> do
      when (attachmentDamage a + n >= 5) $ removeFromPlay a
      MojoAttachment . (`With` dealt) <$> runMessage msg a
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      for_ [(vid, damageAmount d) | W.DamagedVillain vid d <- windows] $ \(vid,n) ->
        insteadOf (\case VillainMessage other (VillainDamaged _ _) -> vid == other; _ -> False) (damageAttachment (attachmentId a) n)
      pure e
    CheckWindows windows | toCardCode a == "39027" -> do
      let amount = sum [n | W.Window _ (W.EnemyAttacksAndDamages enemy _ n) <- windows, Just enemy == attachmentEnemy a]
      pure $ MojoAttachment (a `With` (dealt + amount))
    RanAbility _ (isTarget a -> True) 3 _ _ -> do
      discardTopOfEncounterDeck dealt
      pure $ MojoAttachment (a `With` 0)
    Boost (RevealedAsBoost target enemy) | isTarget a target, toCardCode a == "39028" -> do
      you <- getBoostedPlayer enemy
      revealBoost a you
      pure e
    _ -> MojoAttachment . (`With` dealt) <$> runMessage msg a
