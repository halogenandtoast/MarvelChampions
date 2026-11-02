module Marvel.CampaignPlayers.NemesisAttachments where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Attachments.Thanos (discardAfterBasicAttack)
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.CampaignPlayers.EncounterCards qualified as Cards
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Query
import Marvel.Ref
import Marvel.Resource.Types

cn56026, cn56057, cn60035 :: AttachmentCard NemesisAttachment
cn56026 = attachment NemesisAttachment Cards.enc56026
cn56057 = attachment NemesisAttachment Cards.enc56057
cn60035 = attachment NemesisAttachment Cards.enc60035

newtype NemesisAttachment = NemesisAttachment (Attrs Attachment)
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities NemesisAttachment where
  getAbilities (NemesisAttachment a) = case toCardCode a of
    "56026" -> discardAfterBasicAttack a [Energy, Physical]
    "60035" -> [ability a 1 HeroAction NoCriteria (MultiResourceCost [Just Physical, Just Physical]) $ runAbility a 1]
    _ -> []

instance HasModifiersFor NemesisAttachment where
  getModifiersFor _ target (NemesisAttachment a) = pure $
    if Just target == (enemyRef <$> attachmentEnemy a) || Just target == (AllyRef <$> attachmentAlly a)
    then case toCardCode a of
      "56026" -> [AttackModifier 1, SchemeModifier 1]
      "56057" -> [AttackModifier 1, SchemeModifier 1, HitPointModifier 3, KeywordModifier $ Retaliate 1]
      "60035" -> [AttackModifier 1, KeywordModifier Piercing]
      _ -> []
    else []

instance RunMessage NemesisAttachment where
  runMessage msg e@(NemesisAttachment a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> case toCardCode a of
      "56026" -> do
        hood <- selectOne $ MinionIs Cards.enc56024
        maybe (attachToTheVillain a) (attachTo aid) hood
        pure e
      "56057" -> do
        searchEverywhereAndPutIntoPlayEncounterCard you (CardIs Cards.enc56055)
        cardSpecific a "attachFantasticPowers" you
        pure e
      "60035" -> do
        enemies <- selectList EnemyWithHighestAttack
        unless (null enemies) $ chooseOrRunOne you [targeting (enemyRef enemy) (attachTo aid enemy) | enemy <- enemies]
        pure e
      _ -> pure e
    CardSpecific (isTarget a -> True) "attachFantasticPowers" (fromJSON -> Success (you :: IdentityId)) -> do
      skrulls <- selectList $ MinionIs Cards.enc56055
      for_ skrulls $ \mid -> do
        attachTo (attachmentId a) mid
        hero <- identityMatches HeroIdentity you
        when hero $ enemyAttacks mid you
      pure e
    RanAbility you (isTarget a -> True) 1 _ _ | toCardCode a == "60035" -> do
      elektras <- selectList $ AllyWithTitle "Elektra"
      chooseOrRunOne you $ [targeting ally (attachToAlly (attachmentId a) ally) | ally <- elektras]
        <> [labeled "$label.discard" $ removeFromPlay a]
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      case toCardCode a of
        "56026" -> putBoostIntoPlay a =<< getBoostedPlayer enemy
        "60035" -> attackGains enemy Piercing
        _ -> pure ()
      pure e
    _ -> NemesisAttachment <$> runMessage msg a
