module Marvel.Attachment.Attachments.AgeOfApocalypseDarkBeast where

import Marvel.Prelude
import Marvel.Window ()
import Marvel.Ability
import Marvel.AgeOfApocalypse.Settings ()
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Entity
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Trait ()

newtype AoaDarkBeastAttachment = AoaDarkBeastAttachment (Attrs Attachment `With` Maybe IdentityId)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsAttachment AoaDarkBeastAttachment where
  toAttachmentAttrs (AoaDarkBeastAttachment (attrs `With` _)) = attrs

highTechGoggles, geneticEnhancement, cruelExperiment :: AttachmentCard AoaDarkBeastAttachment
highTechGoggles = attachment (AoaDarkBeastAttachment . (`With` Nothing)) Cards.highTechGoggles
geneticEnhancement = attachment (AoaDarkBeastAttachment . (`With` Nothing)) Cards.geneticEnhancement
cruelExperiment = attachment (AoaDarkBeastAttachment . (`With` Nothing)) Cards.cruelExperiment

instance HasModifiersFor AoaDarkBeastAttachment where
  getModifiersFor _ target (AoaDarkBeastAttachment (attrs `With` _))
    | Just enemy <- attachmentEnemy attrs, target == enemyRef enemy = pure $ case toCardCode attrs of
        "45122" -> [SchemeModifier 1]
        "45123" -> [AttackModifier 1]
        _ -> [AttackModifier 1, SchemeModifier 1, HitPointModifier 2, KeywordModifier Guard]
  getModifiersFor _ _ _ = pure []

instance HasAbilities AoaDarkBeastAttachment where
  getAbilities a@(AoaDarkBeastAttachment (attrs `With` _)) =
    [ability a 1 HeroAction CanResolveSettingSpecial (ExhaustCost <> ResolveSettingSpecialCost) (runAbility a 1)
    | toCardCode attrs `elem` ["45122", "45123"], isJust (attachmentEnemy attrs)]

instance RunMessage AoaDarkBeastAttachment where
  runMessage msg e@(AoaDarkBeastAttachment (attrs `With` actor)) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      if toCardCode attrs == "45124" then do
        discardUntil FromEncounterDeck (CardWithType MinionType) attrs
        cardSpecific attrs "checkAttached" ()
      else attachToTheVillain attrs
      pure $ AoaDarkBeastAttachment (attrs `With` Just you)
    WithDiscardedMatch (isTarget attrs -> True) FromEncounterDeck (EncounterCard card) -> do
      who <- maybe getActivePlayerId pure actor
      revealEncounterCard who card
      cardSpecific attrs "attachExperiment" (MinionId $ unCardId $ ecCardId card)
      pure e
    CardSpecific (isTarget attrs -> True) "attachExperiment" (fromJSON -> Success mid) -> do
      exists <- selectAny (MinionWithId mid)
      when exists $ attachTo (attachmentId attrs) (EnemyMinionId mid)
      pure e
    CardSpecific (isTarget attrs -> True) "checkAttached" _ -> do
      unless (isJust $ attachmentEnemy attrs) $ removeFromPlay attrs
      pure e
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> e <$ removeFromPlay attrs
    Boost (RevealedAsBoost target _) | target == toRef attrs, toCardCode attrs `elem` ["45122", "45123"] -> do
      -- Attach this physical boost card; no new copy and no reveal ability.
      beasts <- selectList (EnemyWithTitle "Dark Beast")
      for_ (headMay beasts) $ attachEncounterCard (MkEncounterCard (CardId $ unAttachmentId $ attachmentId attrs) $ getCardDef attrs)
      pure e
    _ -> AoaDarkBeastAttachment . (`With` actor) <$> runMessage msg attrs
