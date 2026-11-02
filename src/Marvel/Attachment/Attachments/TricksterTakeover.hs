module Marvel.Attachment.Attachments.TricksterTakeover where

import Marvel.Identity.Types (Field(PlayerIdentityHand))
import Marvel.Hand (Hand(..))
import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Entity
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted hiding (ExhaustedIdentity)
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.TricksterTakeover.Cards qualified as Cards
import Marvel.TricksterTakeover.Helpers
import Marvel.Window qualified as W
import Marvel.Trait qualified as Trait
import Marvel.Attachment.Types

newtype TTAttachment = TTAttachment (Attrs Attachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass IsAttachment

tt55007a :: AttachmentCard TTAttachment
tt55007a = attachment TTAttachment Cards.tt55007a

tt55007b :: AttachmentCard TTAttachment
tt55007b = attachment TTAttachment Cards.tt55007b

tt55008a :: AttachmentCard TTAttachment
tt55008a = attachment TTAttachment Cards.tt55008a

tt55008b :: AttachmentCard TTAttachment
tt55008b = attachment TTAttachment Cards.tt55008b

tt55009a :: AttachmentCard TTAttachment
tt55009a = attachment TTAttachment Cards.tt55009a

tt55009b :: AttachmentCard TTAttachment
tt55009b = attachment TTAttachment Cards.tt55009b

tt55010a :: AttachmentCard TTAttachment
tt55010a = attachment TTAttachment Cards.tt55010a

tt55010b :: AttachmentCard TTAttachment
tt55010b = attachment TTAttachment Cards.tt55010b

tt55011a :: AttachmentCard TTAttachment
tt55011a = attachment TTAttachment Cards.tt55011a

tt55011b :: AttachmentCard TTAttachment
tt55011b = attachment TTAttachment Cards.tt55011b

tt55012 :: AttachmentCard TTAttachment
tt55012 = attachment TTAttachment Cards.tt55012

tt55013 :: AttachmentCard TTAttachment
tt55013 = attachment TTAttachment Cards.tt55013

tt55014 :: AttachmentCard TTAttachment
tt55014 = attachment TTAttachment Cards.tt55014

tt55015 :: AttachmentCard TTAttachment
tt55015 = attachment TTAttachment Cards.tt55015

tt55016 :: AttachmentCard TTAttachment
tt55016 = attachment TTAttachment Cards.tt55016

tt55034a :: AttachmentCard TTAttachment
tt55034a = attachment TTAttachment Cards.tt55034a

tt55034b :: AttachmentCard TTAttachment
tt55034b = attachment TTAttachment Cards.tt55034b

tt55035 :: AttachmentCard TTAttachment
tt55035 = attachment TTAttachment Cards.tt55035

tt55036 :: AttachmentCard TTAttachment
tt55036 = attachment TTAttachment Cards.tt55036

tt55062 :: AttachmentCard TTAttachment
tt55062 = attachment TTAttachment Cards.tt55062

instance HasModifiersFor TTAttachment where
  getModifiersFor _ target (TTAttachment a) = do
    hard <- expert
    count <- getPlayerCount
    pure $ if Just target == (IdentityRef <$> attachmentIdentity a) then case toCardCode a of
      "55015" -> [CannotBasicAttack, CannotPlayCards $ CardWithType EventType <> CardWithTrait Trait.Attack]
      "55035" -> [CannotReady]
      code | code `elem` gazeCodes -> [TraitModifier Trait.Defiant]
      code | code `elem` tranceCodes -> [TraitModifier Trait.Enthralled]
      _ -> []
    else if Just target == (enemyRef <$> attachmentEnemy a) then case toCardCode a of
      "55016" -> [KeywordModifier Stalwart | hard]
      "55034a" -> [AttackModifier 1, SchemeModifier 1, HitPointModifier $ 2 * fromIntegral count, KeywordModifier Steady]
      "55034b" -> [AttackModifier 1, SchemeModifier 1, HitPointModifier $ 3 * fromIntegral count, KeywordModifier Steady]
      "55036" -> [AttackModifier 1, KeywordModifier Stalwart]
      _ -> []
    else if Just target == (AllyRef <$> attachmentAlly a) && toCardCode a == "55062" then [CannotAttackVillain, CannotDefendAgainstVillain]
    else case target of
      TreacheryRef _ | toCardCode a == "55036" -> [CannotBeCanceled]
      _ -> []

instance HasAbilities TTAttachment where
  getAbilities (TTAttachment a) = case attachmentIdentity a of
    Just you ->
      [ability a 1 ForcedAction (SelfMatches $ IdentityWithId you) ExhaustAttachmentCost (runAbility a 1) | toCardCode a `elem` tranceCodes <> temptationCodes]
      <> [ability a 2 AlterEgoAction (SelfMatches $ IdentityWithId you) ExhaustAttachmentCost (DiscardTarget $ toRef a) | toCardCode a `elem` temptationCodes]
      <> [ability a 2 AlterEgoAction (SelfMatches $ IdentityWithId you) (MultiResourceCost [Just Energy, Just Mental]) (DiscardTarget $ toRef a) | toCardCode a == "55015"]
      <> [ability a 2 HeroAction (SelfMatches $ IdentityWithId you) (SameResourcesCost 2) (runAbility a 2) | toCardCode a == "55035"]
    Nothing -> case attachmentEnemy a of
      Just (EnemyVillainId vid) ->
        [windowAbility a 3 (W.VillainSchemed W.After $ VillainWithId vid) ForcedResponse NoCost (runAbility a 3) | toCardCode a == "55016"]
        <> [ability a 4 HeroAction NoCriteria (ExhaustAllIdentitiesAndDamageCost 1) (runAbility a 4) | toCardCode a == "55016"]
        <> [windowAbility a 5 (W.TreacheryResolvedBy You) HeroResponse (SameResourcesCost 2) (DiscardTarget $ toRef a) | toCardCode a == "55036"]
      _ -> [ability a 2 AlterEgoAction NoCriteria (ExhaustCharacterCost (AllyCharacters $ AllyWithId aid) <> ResourceCost (Just Mental)) (DiscardTarget $ toRef a) | toCardCode a == "55062", Just aid <- [attachmentAlly a]]

instance RunMessage TTAttachment where
  runMessage msg e@(TTAttachment a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> do
      case toCardCode a of
        code | code `elem` gazeCodes <> tranceCodes <> temptationCodes <> ["55015", "55035"] -> do
          attachToIdentity aid you
          when (code == "55035") $ exhaust you
          when (code `elem` tranceCodes) $ cardSpecific a "ttTranceRevealed" you
        "55062" -> do
          allies <- selectList (AllyControlledBy $ IdentityWithId you)
          if null allies then gainSurge a else chooseOne you [targeting ally (attachToAlly aid ally) | ally <- allies]
        _ -> do
          attachToTheVillain a
          when (toCardCode a == "55034a") $ do
            count <- getPlayerCount
            selectOne ActiveVillain >>= traverse_ (\v -> villainStep v $ VillainHitPointsChanged $ 2 * count)
            cardSpecific a "ttFocusRevealed" ()
      pure e
    CardSpecific (isTarget a -> True) "ttCharmFor" (fromJSON -> Success you) | attachmentIdentity a == Just you, toCardCode a `elem` gazeCodes -> do
      if attachmentCounters a + 1 >= 5 then do
        cardSpecific a "ttTranceRevealed" you
        pure $ TTAttachment a {attachmentCardDef = tranceDef $ toCardCode a, attachmentCounters = 0}
      else pure $ TTAttachment a {attachmentCounters = attachmentCounters a + 1}
    CardSpecific (isTarget a -> True) "ttTranceRevealed" (fromJSON -> Success you) -> do
      case toCardCode a of
        "55007b" -> do
          cards <- discardableControlledCardsMatching you AnyCard
          if null cards then discardTopOfDeck you 8 else chooseAndDiscardControlled you cards
        "55008b" -> do
          confused <- selectAny (IdentityWithId you <> ConfusedIdentity)
          if confused then damageIdentity you a (toDamage 2 FromAbility) else confuseIdentity you
        "55009b" -> do
          exhausted <- selectAny (IdentityWithId you <> ExhaustedIdentity)
          if exhausted then discardFrom you RandomFromHand 1 else exhaust you
        "55010b" -> do
          stunned <- selectAny (IdentityWithId you <> StunnedIdentity)
          if stunned then mainThreat a 2 else stunIdentity you
        "55011b" -> lookAtTopOfEncounterDeck 5 a
        _ -> pure ()
      pure e
    SearchFoundCards (isTarget a -> True) cards | toCardCode a == "55011b" -> do
      for_ (attachmentIdentity a) $ \you -> chooseOne you [cardLabeled (toCardCode c) (revealFoundEncounterCard you ec) | c@(EncounterCard ec) <- cards]
      shuffleEncounterDeck
      pure e
    CardSpecific (isTarget a -> True) "ttTotalFocus" _ -> do
      count <- getPlayerCount
      for_ (attachmentEnemy a) $ \case
        EnemyVillainId v -> villainStep v (VillainHitPointsChanged count)
        _ -> pure ()
      cardSpecific a "ttFocusRevealed" ()
      pure $ TTAttachment a {attachmentCardDef = Cards.tt55034b}
    CardSpecific (isTarget a -> True) "ttFocusRevealed" _ -> do
      hard <- expert
      when (toCardCode a == "55034b" || not hard) $ do
        count <- getPlayerCount
        discardTopOfEncounterDeck (2 * fromIntegral count)
        getAllPlayers >>= traverse_ (`discardTopOfDeck` 5)
      pure e
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      case toCardCode a of
        "55007b" -> selectOne ActiveVillain >>= traverse_ (\v -> dealBoost v >> readyByGame you)
        "55008b" -> do
          hand <- project PlayerIdentityHand you
          drawCards you 1
          cardSpecific a "ttGreedPlay" (you, map toCardId $ unHand hand)
        "55009b" -> mainThreat a 1 >> removeThreat you a 2 (NotScheme MainScheme)
        "55010b" -> do
          selectOne ActiveVillain >>= traverse_ (`heal` 1)
          drawCards you 1
          discardFromHand you 1
        "55011b" -> dealIndirectDamage you a 1 >> chooseDamage you a (toDamage 2 FromAbility) MinionEnemy
        "55012" -> charm you >> cardSpecific a "ttAlluringPlay" you
        "55013" -> charm you >> drawCards you 1 >> discardFromHand you 1
        "55014" -> charm you >> readyByGame you
        _ -> pure ()
      pure e
    CardSpecific (isTarget a -> True) "ttAlluringPlay" (fromJSON -> Success you) -> do
      present <- selectAny (IdentityWithId you)
      when present $ do
        playable <- playableCardsWithReductionAfterExhausting you (toRef a) AnyCard 1
        chooseOne you [cardLabeled (toCardCode c) (playCardWithReduction you 1 c) | c <- playable]
      pure e
    CardSpecific (isTarget a -> True) "ttGreedPlay" (fromJSON -> Success (you, before :: [CardId])) -> do
      present <- selectAny (IdentityWithId you)
      when present $ do
        hand <- unHand <$> project PlayerIdentityHand you
        let drawn = filter ((`notElem` before) . toCardId) hand
        playable <- filterM (\c -> canPlayCardWithReduction you 0 c) drawn
        if null playable then damageIdentity you a (toDamage 1 FromAbility)
          else traverse_ (playCardWithReduction you 0) playable
      pure e
    RanAbility _ (isTarget a -> True) 2 _ _ -> shatter 1 >> removeFromPlay a >> pure e
    RanAbility you (isTarget a -> True) 3 _ _ -> do
      who <- maybe (pure you) (getBoostedPlayer . EnemyVillainId) $ case attachmentEnemy a of Just (EnemyVillainId v) -> Just v; _ -> Nothing
      charm who
      pure e
    RanAbility _ (isTarget a -> True) 4 _ _ -> removeFromPlay a >> pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target, toCardCode a `elem` temptationCodes -> getBoostedPlayer enemy >>= revealBoost a >> pure e
    _ -> TTAttachment <$> runMessage msg a

gazeCodes, tranceCodes, temptationCodes :: [CardCode]
gazeCodes = ["55007a", "55008a", "55009a", "55010a", "55011a"]
tranceCodes = ["55007b", "55008b", "55009b", "55010b", "55011b"]
temptationCodes = ["55012", "55013", "55014"]

tranceDef :: CardCode -> CardDef
tranceDef = \case
  "55007a" -> Cards.tt55007b
  "55008a" -> Cards.tt55008b
  "55009a" -> Cards.tt55009b
  "55010a" -> Cards.tt55010b
  _ -> Cards.tt55011b
