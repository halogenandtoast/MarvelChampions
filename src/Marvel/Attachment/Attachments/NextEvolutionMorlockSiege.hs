module Marvel.Attachment.Attachments.NextEvolutionMorlockSiege where

import Marvel.Prelude
import Marvel.Question (runAbility)
import Marvel.Card
import Marvel.Entity
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source ()
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.NextEvolution.Helpers
import Marvel.Query ()
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype MorlockSiegeAttachment = MorlockSiegeAttachment (Attrs Attachment `With` Natural)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40082 :: AttachmentCard MorlockSiegeAttachment
ne40082 = attachment (\a -> MorlockSiegeAttachment (a `With` 0)) Cards.ne40082

ne40083 :: AttachmentCard MorlockSiegeAttachment
ne40083 = attachment (\a -> MorlockSiegeAttachment (a `With` 0)) Cards.ne40083

instance IsAttachment MorlockSiegeAttachment where
  toAttachmentAttrs (MorlockSiegeAttachment (a `With` _)) = a

instance HasModifiersFor MorlockSiegeAttachment where
  getModifiersFor _ target (MorlockSiegeAttachment (a `With` _))
    | Just enemy <- attachmentEnemy a, target == enemyRef enemy = do
      count <- routedCount
      pure $ [AttackModifier 1] <> if toCardCode a == "40082" then [SchemeModifier 1]
        else [KeywordModifier Steady | count == 1] <> [KeywordModifier Stalwart | count == 2]
  getModifiersFor _ _ _ = pure []

instance HasAbilities MorlockSiegeAttachment where
  getAbilities (MorlockSiegeAttachment (a `With` count)) = case attachmentEnemy a of
    Just enemy ->
      if toCardCode a == "40082"
        then [ability a 1 HeroAction NoCriteria
          (Costs [ExhaustCharacterCost $ CharacterControlledBy You, MultiResourceCost $ replicate (fromIntegral count) Nothing]) (runAbility a 1)]
        else [ability a 1 HeroAction NoCriteria (EnemyAttacksCost enemy) (runAbility a 1)]
    _ -> []

instance RunMessage MorlockSiegeAttachment where
  runMessage msg e@(MorlockSiegeAttachment (a `With` count)) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment _) | aid == attachmentId a -> do
      attachToTheVillain a
      routed <- routedCount
      pure $ MorlockSiegeAttachment (a `With` routed)
    CardSpecific GameRef "neAfterRoutingVillain" (fromJSON -> Success (n,(_ :: VillainId))) ->
      pure $ MorlockSiegeAttachment (a `With` (n :: Natural))
    RanAbility _ (isTarget a -> True) 1 _ _ -> e <$ removeFromPlay a
    Boost (RevealedAsBoost target enemy) | isTarget a target, toCardCode a == "40082" -> do
      routed <- routedCount
      addEnemyBoost enemy routed
      pure e
    _ -> (\updated -> MorlockSiegeAttachment (updated `With` count)) <$> runMessage msg a
