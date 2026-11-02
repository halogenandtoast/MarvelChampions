module Marvel.Attachment.Attachments.SmModulars where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Types qualified as Ally ()
import Marvel.Attachment.Attachments.AllyAsMinion
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Effect.Types
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Window qualified as W
import Marvel.Query
import Marvel.Question
import Marvel.Trait (Trait (Persona))
import Marvel.Trait qualified as Trait
import Marvel.Resource
import Marvel.Status
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Hand
import Marvel.Projection
import Marvel.Keyword (Keyword (Overkill, Piercing, Retaliate))
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Hp ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats ()
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types

data SmModularAttachmentMeta = SmModularAttachmentMeta {gauntletPlayer :: Maybe IdentityId, gauntletDamaged :: Bool, gauntletAttacked :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype SmModularsAttachment = SmModularsAttachment (Attrs Attachment `With` SmModularAttachmentMeta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsAttachment SmModularsAttachment where
  toAttachmentAttrs (SmModularsAttachment (attrs `With` _)) = attrs

smModularsAttachment :: Attrs Attachment -> SmModularsAttachment
smModularsAttachment attrs = SmModularsAttachment (attrs `With` SmModularAttachmentMeta Nothing False False)

smAdvancedGlider :: AttachmentCard SmModularsAttachment
smAdvancedGlider = attachment smModularsAttachment Cards.smAdvancedGlider

smConcussiveBombs :: AttachmentCard SmModularsAttachment
smConcussiveBombs = attachment smModularsAttachment Cards.smConcussiveBombs

smIncendiaryBombs :: AttachmentCard SmModularsAttachment
smIncendiaryBombs = attachment smModularsAttachment Cards.smIncendiaryBombs

smSmokeBombs :: AttachmentCard SmModularsAttachment
smSmokeBombs = attachment smModularsAttachment Cards.smSmokeBombs

smArmCannon :: AttachmentCard SmModularsAttachment
smArmCannon = attachment smModularsAttachment Cards.smArmCannon

smIonicBoots :: AttachmentCard SmModularsAttachment
smIonicBoots = attachment smModularsAttachment Cards.smIonicBoots

smKineticArmor :: AttachmentCard SmModularsAttachment
smKineticArmor = attachment smModularsAttachment Cards.smKineticArmor

smNeocarbonScales :: AttachmentCard SmModularsAttachment
smNeocarbonScales = attachment smModularsAttachment Cards.smNeocarbonScales

smSpikedGauntlet :: AttachmentCard SmModularsAttachment
smSpikedGauntlet = attachment smModularsAttachment Cards.smSpikedGauntlet

smTrackingDisplay :: AttachmentCard SmModularsAttachment
smTrackingDisplay = attachment smModularsAttachment Cards.smTrackingDisplay

smInducedPanic :: AttachmentCard SmModularsAttachment
smInducedPanic = attachment smModularsAttachment Cards.smInducedPanic

smImprovisedWeapons :: AttachmentCard SmModularsAttachment
smImprovisedWeapons = attachment smModularsAttachment Cards.smImprovisedWeapons

smViolentTendencies :: AttachmentCard SmModularsAttachment
smViolentTendencies = attachment smModularsAttachment Cards.smViolentTendencies

smWebbedUp :: AttachmentCard SmModularsAttachment
smWebbedUp = attachment smModularsAttachment Cards.smWebbedUp

smDelusionOfCollusion :: AttachmentCard SmModularsAttachment
smDelusionOfCollusion = attachment smModularsAttachment Cards.smDelusionOfCollusion

smManipulatedMind :: AttachmentCard SmModularsAttachment
smManipulatedMind = attachment smModularsAttachment Cards.smManipulatedMind

smOldGrudge :: AttachmentCard SmModularsAttachment
smOldGrudge = attachment smModularsAttachment Cards.smOldGrudge

instance HasModifiersFor SmModularsAttachment where
  getModifiersFor _ target (SmModularsAttachment (attrs `With` _))
    | Just enemy <- attachmentEnemy attrs, target == enemyRef enemy, toCardCode attrs /= "27172" = pure $ case toCardCode attrs of
        "27137" -> [AttackModifier 1]
        "27138" -> [AttackModifier 1]
        "27139" -> [AttackModifier 1]
        "27147" -> [KeywordModifier Overkill, KeywordModifier Piercing]
        "27149" -> [KeywordModifier $ Retaliate 1]
        "27150" -> [AttackDamageReduction 1]
        "27151" -> [AttackModifier 1]
        "27152" -> [AttacksCannotBeDefended]
        _ -> []
  getModifiersFor _ (IdentityRef you) (SmModularsAttachment (attrs `With` _))
    | attachmentIdentity attrs == Just you, toCardCode attrs == "27166" = pure [ReplaceHeroAttackWithDiscardAndStun $ toRef attrs]
    | attachmentIdentity attrs == Just you, toCardCode attrs == "27153" = pure [CannotTriggerPrintedHeroAbilities]
  getModifiersFor _ (AllyRef aid) (SmModularsAttachment (attrs `With` _))
    | Just you <- attachmentIdentity attrs, toCardCode attrs == "27170" = do
        own <- selectAny (AllyWithId aid <> AllyControlledBy (IdentityWithId you))
        pure [CannotReady | own]
  getModifiersFor _ (SupportRef sid) (SmModularsAttachment (attrs `With` _))
    | Just you <- attachmentIdentity attrs, toCardCode attrs == "27170" = do
        own <- member sid <$> select (SupportMatches [SupportControlledBy (IdentityWithId you), SupportWithTrait Persona])
        pure [CannotReady | own]
  getModifiersFor _ (MinionRef mid) (SmModularsAttachment (attrs `With` _))
    | toCardCode attrs == "27172", attachmentEnemy attrs == Just (EnemyMinionId mid) = pure [HitPointModifier 1]
  getModifiersFor _ _ _ = pure []

instance HasAbilities SmModularsAttachment where
  getAbilities a@(SmModularsAttachment (attrs `With` _)) = case attachmentEnemy attrs of
    Just enemy -> case toCardCode attrs of
      "27136" ->
        [ limited (EachPlayer $ PerRound 1) $ windowAbility a 1 (W.EnemyActivates W.After (EnemyWithId enemy) You) ForcedResponse NoCost (runAbility a 1)
        , ability a 2 HeroAction NoCriteria (DiscardHandCardsWithTotalCost (CardWithTrait Trait.Attack) 3) (DiscardTarget $ toRef attrs)]
      code | code `elem` ["27137", "27138", "27139"] ->
        [windowAbility a 1 (W.EnemyAttacked W.After (EnemyWithId enemy) You) ForcedResponse (ExactCountersCost 1) (runAbility a 1) | attachmentCounters attrs > 0]
      "27147" -> [ability a 2 HeroAction NoCriteria (DiscardUpgradeCost $ UpgradeWithHighestCostAmong $ UpgradeControlledBy You) (DiscardTarget $ toRef attrs)]
      "27148" -> [windowAbility a 1 (W.EnemyAttackedAndDamaged (EnemyWithId enemy) $ IdentityCharacters You) ForcedResponse NoCost (runAbility a 1), resourceRemoval a attrs]
      "27149" -> [ability a 2 HeroAction NoCriteria (IndirectDamageCost 3) (DiscardTarget $ toRef attrs)]
      "27150" -> [ability a 2 HeroAction NoCriteria (GiveEnemyToughCost enemy <> GiveEnemyBoostCost enemy 1) (DiscardTarget $ toRef attrs)]
      "27151" -> [ability a 2 HeroAction NoCriteria NoCost (runAbility a 2)]
      "27152" -> [ability a 2 HeroAction NoCriteria (ExhaustCharacterCost (CharacterControlledBy You) <> DiscardRandomHandCardsCost 1) (DiscardTarget $ toRef attrs)]
      "27164" -> [resourceRemoval a attrs]
      "27165" -> [windowAbility a 1 (W.EnemyTookDamage (EnemyWithId enemy) AnyAttack) ForcedResponse NoCost (runAbility a 1)]
      _ -> []
    Nothing -> identityAbilities a attrs

identityAbilities :: SmModularsAttachment -> Attrs Attachment -> [Ability]
identityAbilities a attrs = case attachmentIdentity attrs of
    Just you | toCardCode attrs == "27153" ->
      [ability a 1 AlterEgoAction (SelfMatches $ IdentityWithId you) (DiscardRandomHandCardsMatchingCost 1 HeroSpecificCard) (DiscardTarget $ toRef attrs)]
    Just you | toCardCode attrs == "27170" ->
      [ability a 1 AlterEgoAction (SelfMatches $ IdentityWithId you) (DiscardAllyOrSupportCost AnyAlly $ SupportWithTrait Persona) (DiscardTarget $ toRef attrs)]
    _ -> []

resourceRemoval :: SmModularsAttachment -> Attrs Attachment -> Ability
resourceRemoval a attrs = ability a 2 HeroAction NoCriteria (MultiResourceCost [Just Energy, Just Mental, Just Physical]) (DiscardTarget $ toRef attrs)

instance RunMessage SmModularsAttachment where
  runMessage msg a@(SmModularsAttachment (attrs `With` meta)) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs, toCardCode attrs `elem` ["27136", "27137", "27138", "27139", "27147", "27148", "27149", "27150", "27151", "27152", "27164", "27165"] -> do
      withTheVillainIfAny you $ \case
        Nothing -> removeFromPlay attrs
        Just vid -> attachTo aid vid
      pure $ SmModularsAttachment ((attrs & countersL .~ (if toCardCode attrs `elem` ["27137", "27138", "27139"] then 2 else 0)) `With` meta)
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs, toCardCode attrs == "27166" -> a <$ attachToIdentity aid you
    RanAbility you (isTarget attrs -> True) 1 _ _ | toCardCode attrs == "27136" -> do
      for_ (attachmentEnemy attrs) $ \case
        EnemyVillainId vid -> villainActivatesAgainst vid you
        EnemyMinionId mid -> do
          hero <- identityMatches HeroIdentity you
          if hero then enemyAttacks mid you else minionSchemesAgainst mid you
        EnemyAttachmentId _ -> pure ()
      pure a
    RanAbility you (isTarget attrs -> True) 1 _ _ | toCardCode attrs == "27137" -> do
      upgrades <- selectList (UnexhaustedUpgrade <> UpgradeControlledBy (IdentityWithId you))
      supports <- selectList (SupportMatches [SupportControlledBy $ IdentityWithId you, UnexhaustedSupport])
      unless (null upgrades) $ chooseOrRunOne you [targeting u (exhaust u) | u <- upgrades]
      unless (null supports) $ chooseOrRunOne you [targeting sid (exhaust sid) | sid <- supports]
      when (attachmentCounters attrs == 0) $ removeFromPlay attrs
      pure a
    RanAbility you (isTarget attrs -> True) 1 _ _ | toCardCode attrs == "27138" -> do
      dealIndirectDamage you attrs 2
      when (attachmentCounters attrs == 0) $ removeFromPlay attrs
      pure a
    RanAbility you (isTarget attrs -> True) 1 _ _ | toCardCode attrs == "27139" -> do
      hand <- filter (cardMatch $ CardWithType EventType) <$> projectMap PlayerIdentityHand unHand you
      unless (null hand) $ do
        let cost c = fromMaybe 0 (cdCost $ getCardDef c)
            least = minimum (map cost hand)
        chooseOrRunOne you [targeting c (discardCard c) | c <- hand, cost c == least]
      when (attachmentCounters attrs == 0) $ removeFromPlay attrs
      pure a
    RanAbility _ (isTarget attrs -> True) 1 _ _ | toCardCode attrs == "27148" -> do
      placeThreatOnEach attrs 2 EncounterMainScheme
      pure a
    RanAbility you (isTarget attrs -> True) 2 _ _ | toCardCode attrs == "27151" -> do
      for_ (attachmentEnemy attrs) $ \enemy -> enemyAttacks enemy you
      cardSpecific attrs "gauntletAfterAttack" ()
      pure $ SmModularsAttachment (attrs `With` SmModularAttachmentMeta (Just you) False False)
    CheckWindows windows | isJust (gauntletPlayer meta), any (\case W.Window W.After (W.IdentityTookDamage you source damage) -> Just you == gauntletPlayer meta && Just source == ((toSource . enemyRef) <$> attachmentEnemy attrs) && damageSource damage `elem` [FromAttack, FromOverkill] && damageAmount damage > 0; _ -> False) windows ->
      pure $ SmModularsAttachment (attrs `With` meta {gauntletDamaged = True})
    CheckWindows windows | isJust (gauntletPlayer meta), any (\case W.Window W.After (W.EnemyAttack enemy _) -> Just enemy == attachmentEnemy attrs; _ -> False) windows ->
      pure $ SmModularsAttachment (attrs `With` meta {gauntletAttacked = True})
    CardSpecific (isTarget attrs -> True) "gauntletAfterAttack" _ -> do
      when (gauntletAttacked meta && not (gauntletDamaged meta)) $ removeFromPlay attrs
      pure $ SmModularsAttachment (attrs `With` SmModularAttachmentMeta Nothing False False)
    RanAbility _ (isTarget attrs -> True) 1 windows _ | toCardCode attrs == "27165" -> do
      for_ [damage | W.EnemyTookDamageFrom _ damage _ <- windows] $ \damage -> do
        traverse_ (dealBoost . enemyRef) (attachmentEnemy attrs)
        when (damageAmount damage >= 3) $ removeFromPlay attrs
      pure a
    Boost (RevealedAsBoost target enemy) | isTarget attrs target, toCardCode attrs == "27166" -> do
      you <- getBoostedPlayer enemy
      stunned <- identityMatches StunnedIdentity you
      if stunned then damageIdentity you attrs (toDamage 2 FromAbility) else giveStatus you StunnedStatus
      pure a
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs, toCardCode attrs `elem` ["27153", "27170"] ->
      a <$ attachToIdentity aid you
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs, toCardCode attrs == "27171" -> do
      allies <- filterM attachable =<< selectList (AllyControlledBy $ IdentityWithId you)
      costed <- for allies \ally -> (ally,) . fromMaybe 0 . (cdCost =<<) <$> getsGame (`abilityCardDef` AllyRef ally)
      let cheapest = minimum (map snd costed)
          tied = [ally | (ally, cost) <- costed, cost == cheapest]
      if null allies
        then removeFromPlay attrs >> surge you
        else chooseOrRunOne you
          [targeting ally do
              allyBecomesMinion ally
              attachTo aid (MinionId $ unAllyId ally)
              minionStep (MinionId $ unAllyId ally) MinionCheckHitPoints
          | ally <- tied]
      pure a
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs, toCardCode attrs == "27172" -> do
      minions <- getNemesisMinionsOutOfPlay you
      discard <- getEncounterDiscard
      aside <- getSetAsideEncounterCards
      if null minions
        then removeFromPlay attrs
        else chooseOrRunOne you
          [targeting card do
              revealEncounterCardFrom (if card `elem` discard then W.RevealedFromDiscardPile else if card `elem` aside then W.RevealedFromSetAside else W.RevealedFromEncounterDeck) you card
              cardSpecific attrs "attachOldGrudge" (MinionId $ unCardId $ ecCardId card)
          | card <- minions]
      shuffleEncounterDeck
      pure a
    CardSpecific (isTarget attrs -> True) "attachOldGrudge" (fromJSON -> Success mid) -> do
      present <- selectAny (MinionWithId mid)
      if present then attachTo (attachmentId attrs) mid else removeFromPlay attrs
      pure a
    Boost (RevealedAsBoost target enemy) | isTarget attrs target, toCardCode attrs == "27170" -> do
      you <- getBoostedPlayer enemy
      createEffect Cards.smDelusionOfCollusion (EnemyRef enemy) (IdentityEntity $ IdentityWithId you)
      pure a
    Boost (RevealedAsBoost target enemy) | isTarget attrs target, toCardCode attrs == "27172" -> do
      you <- getBoostedPlayer enemy
      damageAllCharacters attrs (toDamage 1 FromAbility) (CharacterControlledBy $ IdentityWithId you)
      pure a
    _ -> do
      when (toCardCode attrs == "27171") (revertsToAlly attrs msg)
      (\x -> SmModularsAttachment (x `With` meta)) <$> runMessage msg attrs
   where
    attachable ally = do
      modifiers <- getModifiers ally
      pure $ CannotHaveAttachedCards `notElem` modifiers && CannotHaveAttachedEncounterCards `notElem` modifiers

newtype DelusionOfCollusionBoostEffect = DelusionOfCollusionBoostEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

delusionOfCollusionBoostEffect :: CardEffect DelusionOfCollusionBoostEffect
delusionOfCollusionBoostEffect = effect DelusionOfCollusionBoostEffect Cards.smDelusionOfCollusion

instance RunMessage DelusionOfCollusionBoostEffect where
  runMessage msg e@(DelusionOfCollusionBoostEffect attrs) = runQueueT $ case msg of
    AllyMessage aid (AllyDefeatedByAttack enemy) | effectSource attrs == EnemyRef enemy -> do
      cost <- fromMaybe 0 . (cdCost =<<) <$> getsGame (`abilityCardDef` AllyRef aid)
      for_ [you | IdentityEntity (IdentityWithId you) <- [effectMatcher attrs]] \you ->
        when (cost > 0) $ dealIndirectDamage you attrs (fromIntegral cost)
      pure e
    ClearBoosts -> e <$ disableEffect (effectId attrs)
    _ -> DelusionOfCollusionBoostEffect <$> runMessage msg attrs
