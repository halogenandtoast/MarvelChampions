module Marvel.Attachment.Attachments.AgeOfApocalypseFourHorsemen where

import Marvel.Prelude
import Marvel.Id
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source ()
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Scenario.AgeOfApocalypseFourHorsemen
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Card
import Marvel.Entity
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Ability ()
import Marvel.Modifier ()
import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype AoaFourHorsemenAttachment = AoaFourHorsemenAttachment (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

goldenHorse :: AttachmentCard AoaFourHorsemenAttachment
goldenHorse = attachment AoaFourHorsemenAttachment Cards.goldenHorse

metalWings :: AttachmentCard AoaFourHorsemenAttachment
metalWings = attachment AoaFourHorsemenAttachment Cards.metalWings

instance HasAbilities AoaFourHorsemenAttachment where
  getAbilities a@(AoaFourHorsemenAttachment attrs) =
    [limitedWindowAbility a 1 (W.IdentityAttacked W.After You $ EnemyWithId enemy)
      HeroResponse NoCriteria NoCost (runAbility a 1) | Just enemy <- [attachmentEnemy attrs]]

instance HasModifiersFor AoaFourHorsemenAttachment where
  getModifiersFor _ target (AoaFourHorsemenAttachment attrs) | Just target == (enemyRef <$> attachmentEnemy attrs) = pure $
    [AttackModifier 1, SchemeModifier 1] <> case toCardCode attrs of
      "45090" -> [TraitModifier Trait.Aerial]
      "45091" -> [KeywordModifier $ Retaliate 1]
      _ -> []
  getModifiersFor _ _ _ = pure []
instance RunMessage AoaFourHorsemenAttachment where
  runMessage msg a@(AoaFourHorsemenAttachment attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      if toCardCode attrs == "45091" then do
        death <- findHorseman "Death"
        for_ death $ \vid -> attachTo (attachmentId attrs) vid >> setActiveVillain vid
      else do
        villains <- selectList AnyVillain
        candidates <- filterM (fmap not . enemyMatches (EnemyWithTrait Trait.Aerial) . EnemyVillainId) villains
        points <- traverse horsemanHitPoints candidates
        let lowest = if null points then 0 else minimum points
        if null candidates then removeFromPlay attrs
          else chooseOne you [targeting vid $ attachTo (attachmentId attrs) vid | (vid, hp) <- zip candidates points, hp == lowest]
      pure a
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      for_ [vid | Just (EnemyVillainId vid) <- [attachmentEnemy attrs]] $ \vid -> horsemanResponse vid you
      removeFromPlay attrs
      cardSpecific GameRef "aoaHorsemenCheckVictory" ()
      pure a
    RemoveFromPlay target | isTarget attrs target -> do
      cardSpecific GameRef "aoaHorsemenCheckVictory" ()
      AoaFourHorsemenAttachment <$> runMessage msg attrs
    _ -> AoaFourHorsemenAttachment <$> runMessage msg attrs
