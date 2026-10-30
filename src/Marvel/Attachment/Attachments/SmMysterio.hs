module Marvel.Attachment.Attachments.SmMysterio where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Criteria
import Marvel.Cost
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W

newtype SmMysterioAttachment = SmMysterioAttachment (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

smHumongousHallucination :: AttachmentCard SmMysterioAttachment
smHumongousHallucination = attachment SmMysterioAttachment Cards.smHumongousHallucination

smMasterfulMirage :: AttachmentCard SmMysterioAttachment
smMasterfulMirage = attachment SmMysterioAttachment Cards.smMasterfulMirage

instance HasAbilities SmMysterioAttachment where
  getAbilities a@(SmMysterioAttachment attrs)
    | toCardCode attrs == "27089" = [ability a 1 HeroAction NoCriteria (ResourceCost Nothing <> ShuffleEncounterCardsIntoOwnDeckCost 2) (DiscardTarget $ toRef attrs)]
    | otherwise = case attachmentEnemy attrs of
        Just (EnemyVillainId vid) -> [windowAbility a 2 (W.VillainWouldTakeDamageFrom (VillainWithId vid) (DamageByPlayer AnyIdentity)) ForcedInterrupt NoCost (runAbility a 2)]
        _ -> []

instance RunMessage SmMysterioAttachment where
  runMessage msg a@(SmMysterioAttachment attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment _) | aid == attachmentId attrs -> do
      villains <- selectList AnyVillain
      mysterios <- filterM (\vid -> maybe False (cardMatch $ CardWithTitle "Mysterio") <$> getsGame (`abilityCardDef` VillainRef vid)) villains
      case mysterios of
        vid : _ -> attachTo aid vid
        [] -> removeFromPlay attrs
      pure a
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> do
      for_ [vid | Just (EnemyVillainId vid) <- [attachmentEnemy attrs]] \vid -> do
        source <- pendingVillainDamageSource vid
        player <- maybe (pure Nothing) getSourceController source
        for_ player \you ->
          insteadOf (\case VillainMessage enemy (VillainDamaged _ _) -> enemy == vid; _ -> False) do
            discardTopOfDeck you 4
            removeFromPlay attrs
      pure a
    Boost (RevealedAsBoost target enemy) | isTarget attrs target, toCardCode attrs == "27090" -> do
      dealBoost enemy
      pure a
    _ -> SmMysterioAttachment <$> runMessage msg attrs
