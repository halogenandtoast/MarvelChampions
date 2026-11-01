module Marvel.Attachment.Attachments.AgeOfApocalypseCelestialTech where

import Marvel.Prelude
import Marvel.Damage
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Query
import Marvel.Question
import Marvel.Resource
import Marvel.Window qualified as W
import Marvel.Card
import Marvel.Entity
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Ability ()
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype AoaCelestialTechAttachment = AoaCelestialTechAttachment (Attrs Attachment `With` Maybe IdentityId)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

celestialArmor :: AttachmentCard AoaCelestialTechAttachment
celestialArmor = attachment (AoaCelestialTechAttachment . (`With` Nothing)) Cards.celestialArmor

celestialWeapon :: AttachmentCard AoaCelestialTechAttachment
celestialWeapon = attachment (AoaCelestialTechAttachment . (`With` Nothing)) Cards.celestialWeapon

instance IsAttachment AoaCelestialTechAttachment where
  toAttachmentAttrs (AoaCelestialTechAttachment (attrs `With` _)) = attrs

instance HasCardCode AoaCelestialTechAttachment where
  toCardCode (AoaCelestialTechAttachment (attrs `With` _)) = toCardCode attrs

instance IsRef AoaCelestialTechAttachment where
  toRef (AoaCelestialTechAttachment (attrs `With` _)) = toRef attrs

instance HasAbilities AoaCelestialTechAttachment where
  getAbilities a@(AoaCelestialTechAttachment (attrs `With` _)) =
    [limitedWindowAbility a 1 matcher ForcedInterrupt NoCriteria NoCost (runAbility a 1)
    | Just (EnemyVillainId vid) <- [attachmentEnemy attrs]
    , let matcher = if toCardCode attrs == "45156" then W.VillainSchemed W.When (VillainWithId vid)
            else W.EnemyAttacked W.When (EnemyWithId $ EnemyVillainId vid) AnyIdentity]
instance RunMessage AoaCelestialTechAttachment where
  runMessage msg a@(AoaCelestialTechAttachment (attrs `With` player)) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment _) | aid == attachmentId attrs -> do
      selectOne ActiveVillain >>= traverse_ (attachTo aid)
      pure a
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      for_ (attachmentEnemy attrs) $ \enemy -> do
        you <- getBoostedPlayer enemy
        cardSpecific attrs "resolveCelestial" you
      pure a
    CardSpecific (isTarget attrs -> True) "resolveCelestial" (fromJSON -> Success you) -> do
      discardFromDeck you 1 attrs
      pure $ AoaCelestialTechAttachment $ attrs `With` Just you
    WithDiscarded (isTarget attrs -> True) FromDeck cards -> do
      let icons = concatMap (printedResources . getCardDef) cards
          has resource = resource `elem` icons || Wild `elem` icons
      for_ player $ \you -> case toCardCode attrs of
        "45156" -> do
          when (has Energy) $ for_ (attachmentEnemy attrs) (`heal` 2)
          when (has Mental) $ confuseIdentity you >> removeFromPlay attrs
          when (has Physical) $ for_ (attachmentEnemy attrs) enemyBecomesTough
        "45157" -> do
          when (has Energy) $ damageIdentity you attrs $ toDamage 2 FromAbility
          when (has Mental) $ discardFromHand you 1
          when (has Physical) $ stunIdentity you >> removeFromPlay attrs
        _ -> pure ()
      pure a
    _ -> AoaCelestialTechAttachment . (`With` player) <$> runMessage msg attrs
