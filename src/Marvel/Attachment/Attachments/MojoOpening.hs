module Marvel.Attachment.Attachments.MojoOpening where

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.MojoOpening
import Marvel.Prelude
import Marvel.Query ()
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait qualified as Trait

newtype MojoOpeningAttachment = MojoOpeningAttachment (Attrs Attachment)
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

mojo39005, mojo39006, mojo39018 :: AttachmentCard MojoOpeningAttachment
mojo39005 = attachment MojoOpeningAttachment Cards.mojo39005
mojo39006 = attachment MojoOpeningAttachment Cards.mojo39006
mojo39018 = attachment MojoOpeningAttachment Cards.mojo39018

instance HasModifiersFor MojoOpeningAttachment where
  getModifiersFor _ target (MojoOpeningAttachment a)
    | Just enemy <- attachmentEnemy a, target == enemyRef enemy = pure $ case toCardCode a of
        "39005" -> [KeywordModifier $ Retaliate 1, KeywordModifier Stalwart]
        "39006" -> [AttackModifier 1, SchemeModifier 1]
        "39018" -> [AttackModifier $ attachmentCounters a]
        _ -> []
    | otherwise = pure []

instance HasAbilities MojoOpeningAttachment where
  getAbilities (MojoOpeningAttachment a) =
    [ability a 1 HeroAction (EnemyExists $ EnemyWithTrait Trait.Cornered) (MultiResourceCost [Just Physical, Just Physical]) (runAbility a 1) | toCardCode a == "39018", attachmentCounters a > 0]

instance RunMessage MojoOpeningAttachment where
  runMessage msg e@(MojoOpeningAttachment a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment _) | aid == attachmentId a -> do
      attachToTheVillain a
      when (toCardCode a == "39018") $ placeCounters a 3
      pure e
    CardSpecific (VillainRef vid) "magogReset" _ | attachmentEnemy a == Just (EnemyVillainId vid), toCardCode a `elem` ["39005", "39006"] -> do
      players <- fromIntegral <$> getStartingPlayerCount
      rateChallengers players
      removeFromPlay a
      pure e
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      removeCounters a 1
      when (attachmentCounters a == 1) $ removeFromPlay a
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      revealEncounterCard you $ MkEncounterCard (CardId $ unAttachmentId $ attachmentId a) (getCardDef a)
      pure e
    _ -> MojoOpeningAttachment <$> runMessage msg a
