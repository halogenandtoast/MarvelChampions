module Marvel.Attachment.Attachments.AgeOfApocalypseApocalypse where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Scenario.AgeOfApocalypseApocalypse
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Card
import Marvel.Entity
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Ability ()
import Marvel.Modifier ()
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype AoaApocalypseAttachment = AoaApocalypseAttachment (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

noLongerWorthy :: AttachmentCard AoaApocalypseAttachment
noLongerWorthy = attachment AoaApocalypseAttachment Cards.noLongerWorthy

cyberpathy :: AttachmentCard AoaApocalypseAttachment
cyberpathy = attachment AoaApocalypseAttachment Cards.cyberpathy

biomorphing :: AttachmentCard AoaApocalypseAttachment
biomorphing = attachment AoaApocalypseAttachment Cards.biomorphing

molecularControl :: AttachmentCard AoaApocalypseAttachment
molecularControl = attachment AoaApocalypseAttachment Cards.molecularControl

theFittest :: AttachmentCard AoaApocalypseAttachment
theFittest = attachment AoaApocalypseAttachment Cards.theFittest

instance HasModifiersFor AoaApocalypseAttachment where
  getModifiersFor _ target (AoaApocalypseAttachment attrs) | Just target == (enemyRef <$> attachmentEnemy attrs) = case toCardCode attrs of
    "45105b" -> do
      prelates <- selectAny $ MinionWithTrait Trait.Prelate
      pure [CannotTakeDamage | prelates]
    "45106" -> pure [SchemeModifier 1]
    "45107" -> pure [AttackModifier 1, KeywordModifier Overkill]
    "45108" -> pure [KeywordModifier $ Retaliate 1, KeywordModifier Stalwart]
    "45109" -> pure [AttackModifier 1, SchemeModifier 1, HitPointModifier 5]
    _ -> pure []
  getModifiersFor _ _ _ = pure []

instance HasAbilities AoaApocalypseAttachment where
  getAbilities a@(AoaApocalypseAttachment attrs) =
    [limitedWindowAbility a 1 (W.VillainSchemed W.After $ VillainWithId vid) ForcedResponse NoCriteria NoCost (runAbility a 1)
    | toCardCode attrs == "45106", Just (EnemyVillainId vid) <- [attachmentEnemy attrs]]
instance RunMessage AoaApocalypseAttachment where
  runMessage msg a@(AoaApocalypseAttachment attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      if toCardCode attrs == "45109" then do
        minions <- selectList MinionWithHighestPrintedHitPoints
        if null minions then surge you >> removeFromPlay attrs else chooseOne you [targeting mid $ do
          attachTo aid mid
          enemyBecomesTough mid | mid <- minions]
      else do
        current <- apocalypse
        for_ current $ \vid -> do
          attachTo aid vid
          when (toCardCode attrs == "45105b") $ do
            n <- fromIntegral <$> fromGameValue (PerPlayer 5)
            heal vid n
      pure a
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      schemes <- selectList $ NotScheme MainScheme
      for_ schemes $ \sid -> placeThreat sid 1
      pure a
    Boost (RevealedAsBoost target _) | isTarget attrs target, toCardCode attrs `elem` ["45106", "45107", "45108"] -> do
      card <- getBoostCard target
      current <- apocalypse
      for_ card $ \encounter -> for_ current $ \vid -> consumeBoostCard encounter >> attachEncounterCard encounter vid
      pure a
    _ -> AoaApocalypseAttachment <$> runMessage msg attrs
