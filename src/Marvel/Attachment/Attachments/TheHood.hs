module Marvel.Attachment.Attachments.TheHood where

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Prelude
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Villain.Villains.TheHood
import Marvel.Window
import Marvel.Window qualified as W

newtype HoodAttachment = HoodAttachment (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

establishedDominance, theHoodsMantle, theHoodsPistol :: AttachmentCard HoodAttachment
establishedDominance = attachment HoodAttachment Cards.establishedDominance
theHoodsMantle = attachment HoodAttachment Cards.theHoodsMantle
theHoodsPistol = attachment HoodAttachment Cards.theHoodsPistol

instance HasModifiersFor HoodAttachment where
  getModifiersFor _ target (HoodAttachment a) = pure $ case attachmentEnemy a of
    Just enemy | target == enemyRef enemy -> case toCardCode a of
      "24008" -> [KeywordModifier (Retaliate 1), KeywordModifier Steady]
      "24009" -> [AttackModifier 1, SchemeModifier 1]
      _ -> []
    _ -> []

instance HasAbilities HoodAttachment where
  getAbilities (HoodAttachment a) = case toCardCode a of
    "24007" -> case attachmentIdentity a of
      Nothing -> []
      Just you ->
        [ windowAbility a 1 (EnemyActivates After VillainEnemy (IdentityWithId you)) ForcedResponse NoCost $ runAbility a 1
        , ability a 2 AlterEgoAction (SelfMatches $ IdentityWithId you) (ExhaustCost <> PlaceThreatOnMainSchemeCost 2) $ runAbility a 2
        ]
    "24008" -> [ability a 2 HeroAction NoCriteria (MultiResourceCost [Just Energy, Just Mental, Just Physical]) $ DiscardTarget (toRef a)]
    _ -> [ability a 2 HeroAction NoCriteria (MultiResourceCost [Just Mental, Just Physical]) $ DiscardTarget (toRef a)]

instance RunMessage HoodAttachment where
  runMessage msg e@(HoodAttachment attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      if toCardCode attrs == "24007" then attachToIdentity aid you else attachToTheVillain attrs
      pure e
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      traverse_ foulPlay (attachmentIdentity attrs)
      pure e
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> do
      removeFromPlay attrs
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget attrs target, toCardCode attrs == "24009" -> do
      putBoostIntoPlay attrs =<< getBoostedPlayer enemy
      pure e
    _ -> HoodAttachment <$> runMessage msg attrs
