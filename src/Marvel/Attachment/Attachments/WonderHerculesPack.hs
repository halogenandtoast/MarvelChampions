module Marvel.Attachment.Attachments.WonderHerculesPack where

import Marvel.Prelude
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword qualified as K
import Marvel.Matchers
import Marvel.Query
import Marvel.Question
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Attachment.Types
import Marvel.Hercules.Cards qualified as HC

hc59002 :: AttachmentCard WmHcAttachment
hc59002 = attachment (WmHcAttachment . (`With` (Nothing, False))) HC.hc59002

hc59003 :: AttachmentCard WmHcAttachment
hc59003 = attachment (WmHcAttachment . (`With` (Nothing, False))) HC.hc59003

hc59039 :: AttachmentCard WmHcAttachment
hc59039 = attachment (WmHcAttachment . (`With` (Nothing, False))) HC.hc59039

newtype WmHcAttachment = WmHcAttachment (Attrs Attachment `With` (Maybe IdentityId, Bool))
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsAttachment WmHcAttachment where
  toAttachmentAttrs (WmHcAttachment (attrs `With` _)) = attrs

instance HasModifiersFor WmHcAttachment where
  getModifiersFor _ target (WmHcAttachment (attrs `With` (owner, _attackDamaged))) = case toCardCode attrs of
    "59002" | Just enemy <- attachmentEnemy attrs, target == enemyRef enemy -> pure $ [HitPointModifier 6, TraitModifier T.Elite] <> [DamageOnlyFromCharacterAttacks $ IdentityCharacter you | you <- maybe [] pure owner]
    "59003" | Just sid <- attachmentSideScheme attrs, target == SideSchemeRef sid -> pure $ [KeywordModifier K.Assault] <> [ThreatRemovableOnlyByCharacterThwarts $ IdentityCharacter you | you <- maybe [] pure owner]
    "59039" | Just enemy <- attachmentEnemy attrs, target == enemyRef enemy -> pure [AttackModifier 2]
    _ -> pure []

instance HasAbilities WmHcAttachment where
  getAbilities a@(WmHcAttachment (attrs `With` _)) = case toCardCode attrs of
    "59002" -> [limitedWindowAbility a 1 (W.MinionDefeated W.When $ MinionWithId mid) ForcedInterrupt NoCriteria NoCost $ runAbility a 1 | EnemyMinionId mid <- maybe [] pure $ attachmentEnemy attrs]
    "59003" -> [limitedWindowAbility a 1 (W.SideSchemeDefeated W.When $ SideSchemeWithId sid) ForcedInterrupt NoCriteria NoCost $ runAbility a 1 | sid <- maybe [] pure $ attachmentSideScheme attrs]
    "59039" -> [limitedWindowAbility a 1 (W.EnemyAttackedAnyTarget W.After $ EnemyWithId enemy) ForcedResponse NoCriteria NoCost $ runAbility a 1 | enemy <- maybe [] pure $ attachmentEnemy attrs]
    _ -> []

instance RunMessage WmHcAttachment where
  runMessage msg a@(WmHcAttachment (attrs `With` (owner, attackDamaged))) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      case toCardCode attrs of
        "59002" -> do
          cards <- findEncounterCards $ CardWithType MinionType <> NotCardMatching (CardWithTrait T.Elite)
          valid <- filterM (\card -> do
            let mid = MinionId $ unCardId $ ecCardId card
            present <- selectAny $ MinionWithId mid
            elite <- if present then enemyMatches (EnemyWithTrait T.Elite) (EnemyMinionId mid) else pure False
            pure $ encounterCardPrintedHitPoints you card >= 6 && not elite) cards
          if null valid then removeFromPlay attrs else chooseOne you [targeting card do
            revealEncounterCard you card
            cardSpecific attrs "attachHydra" (you, card)
            | card <- valid]
        "59003" -> do
          cards <- findEncounterCards $ CardWithType SideSchemeType
          available <- filterM (\card -> not <$> selectAny (SideSchemeWithId $ SideSchemeId $ unCardId $ ecCardId card)) cards
          if null available then removeFromPlay attrs else chooseOne you [targeting card do
            revealEncounterCardWithPlayerCount you 1 card
            cardSpecific attrs "attachPathos" card
            | card <- available]
        "59039" -> do
          ares <- selectOne $ MinionIs HC.hc59036
          case ares of
            Just mid -> attachTo aid mid
            Nothing -> attachToTheVillain attrs
        _ -> pure ()
      pure $ WmHcAttachment (attrs `With` (Just you, attackDamaged))
    CardSpecific (isTarget attrs -> True) "attachHydra" (fromJSON -> Success (_you :: IdentityId, card :: EncounterCard)) -> do
      let mid = MinionId $ unCardId $ ecCardId card
      present <- selectAny $ MinionWithId mid
      when present do
        healAllDamage mid
        attachTo (attachmentId attrs) mid
      unless present $ removeFromPlay attrs
      pure a
    CardSpecific (isTarget attrs -> True) "attachPathos" (fromJSON -> Success (card :: EncounterCard)) -> do
      let sid = SideSchemeId $ unCardId $ ecCardId card
      present <- selectAny $ SideSchemeWithId sid
      if present then do
        attachToSideScheme (attachmentId attrs) sid
        placeThreatOn attrs sid 6
      else removeFromPlay attrs
      pure a
    RanAbility _ (isTarget attrs -> True) 1 _ _ | toCardCode attrs `elem` ["59002", "59003"] -> a <$ addToVictoryDisplay attrs
    RanAbility _ (isTarget attrs -> True) 1 _ _ | toCardCode attrs == "59039" -> do
      for_ (attachmentEnemy attrs) $ \_enemy -> do
        unless attackDamaged $ removeFromPlay attrs
      pure a
    CheckWindows windows | toCardCode attrs == "59039" -> do
      let starts = any (\case W.Window W.When (W.EnemyAttack enemy _) -> Just enemy == attachmentEnemy attrs; W.Window W.When (W.EnemyAttackAlly enemy _ _) -> Just enemy == attachmentEnemy attrs; _ -> False) windows
          hit = any (\case W.Window W.After (W.EnemyAttacksAndDamages enemy character n) -> Just enemy == attachmentEnemy attrs && n > 0 && (case character of IdentityCharacter _ -> True; AllyCharacter _ -> True; _ -> False); _ -> False) windows
      pure $ WmHcAttachment (attrs `With` (owner, if starts then False else attackDamaged || hit))
    _ -> (\attrs' -> WmHcAttachment (attrs' `With` (owner, attackDamaged))) <$> runMessage msg attrs
