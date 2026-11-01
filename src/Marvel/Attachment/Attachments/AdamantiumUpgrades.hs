module Marvel.Attachment.Attachments.AdamantiumUpgrades where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Status ()
import Marvel.Window qualified as W ()
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Keyword (Keyword (Piercing))
import Marvel.Resource

adamantiumUpgrades :: AttachmentCard AdamantiumUpgrades
adamantiumUpgrades = attachment AdamantiumUpgrades Cards.adamantiumUpgrades

newtype AdamantiumUpgrades = AdamantiumUpgrades (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor AdamantiumUpgrades where
  getModifiersFor _ target (AdamantiumUpgrades a) = pure $
    if maybe False (\enemy -> target == enemyRef enemy || target == toRef enemy) (attachmentEnemy a) then [AttackModifier 2, KeywordModifier Piercing] else []

instance HasAbilities AdamantiumUpgrades where
  getAbilities a = [ability a 1 HeroAction NoCriteria (MultiResourceCost [Just Energy, Just Mental, Just Physical]) (runAbility a 1)]

instance RunMessage AdamantiumUpgrades where
  runMessage msg e@(AdamantiumUpgrades attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      enemies <- selectList (AnyEnemy <> NotEnemy (EnemyWithAttachment $ AttachmentIs Cards.adamantiumUpgrades))
      if null enemies then surge you >> removeFromPlay attrs
      else chooseOne you [targeting (enemyRef enemy) (attachTo aid enemy) | enemy <- enemies]
      pure e
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> e <$ removeFromPlay attrs
    _ -> AdamantiumUpgrades <$> runMessage msg attrs
