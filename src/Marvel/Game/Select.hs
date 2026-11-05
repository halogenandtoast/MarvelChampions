{-# OPTIONS_GHC -Wno-orphans #-}

module Marvel.Game.Select where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List (find)
import Marvel.Ability
import Marvel.Ability qualified as Ability
import Marvel.Ally.Types (
  Attrs (allyUpgrades),
  getAllyController,
  getAllyDamage,
  getAllyUses,
 )
import Marvel.Ally.Types qualified as Ally
import Marvel.Area
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Attack
import Marvel.Card
import Marvel.Card.AttachmentHeroAbilities (
  hasPrintedHeroActionOrResponse,
 )
import Marvel.Count
import Marvel.EncounterSet
import Marvel.Entity
import Marvel.Environment.Types (Field (EnvironmentOwner))
import Marvel.Game.Base
import Marvel.Game.Class
import Marvel.Game.Query
import Marvel.Game.Source ()
import Marvel.GameValue (fromGameValue)
import Marvel.Hp
import Marvel.Id
import Marvel.Identity hiding (alliesL, minionsL, supportsL, upgradesL)
import Marvel.Keyword
import Marvel.MainScheme.Types (getMainSchemeThreat)
import Marvel.Matchers hiding (ExhaustedIdentity, MainScheme)
import Marvel.Matchers qualified as Matchers
import Marvel.Message.Lifted hiding (ExhaustedAlly, canMoveSchemeThreatFrom, createEffect)
import Marvel.Minion.Types (
  Attrs (
    minionAttachments,
    minionConfused,
    minionStunned,
    minionTough,
    minionUpgrades
  ),
  getMinionDamage,
  getMinionEngagedIdentity,
  getMinionPrintedHitPoints,
  minionAttackDetails,
 )
import Marvel.Minion.Types qualified as Minion
import Marvel.Modifier
import Marvel.Name
import Marvel.Prelude
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Types (getSideSchemeThreat, isCrisis)
import Marvel.Trait hiding (Finished)
import Marvel.Upgrade.Types (
  Attrs (
    upgradeAttachedAlly,
    upgradeAttachedEnemy,
    upgradeAttachedMainScheme,
    upgradeAttachedSideScheme,
    upgradeFaceDown
  ),
  IsUpgrade (upgradeSpecialCanResolve),
  Upgrade (Upgrade),
  getUpgradeController,
  getUpgradeUses,
 )
import Marvel.Upgrade.Types qualified as Upgrade
import Marvel.Villain.Types (
  villainAttackDetails,
  villainDamage,
  villainIsTough,
  villainUpgrades,
 )
import Marvel.Villain.Types qualified as Villain

gameSelectIdentity :: (HasGame m) => IdentityMatcher -> m (HashSet IdentityId)
gameSelectIdentity m = do
  everyone <- HashMap.elems <$> getsGame gamePlayers
  identities <- if named m then pure everyone else inYourAreaOnly (IdentityRef . toId) everyone
  result <- filterM (matchFilter m) identities
  pure $ HashSet.fromList $ map toId result
 where
  -- a player named, or the first player, is who they are in any area
  named = \case
    FirstPlayer -> True
    You -> True
    IdentityWithId _ -> True
    IdentityMatchAll xs -> any named xs
    _ -> False
  matchFilter x = case x of
    AnyIdentity -> pure . const True
    HeroIdentity -> pure . isHero
    AlterEgoIdentity -> pure . isAlterEgo
    UnexhaustedIdentity -> pure . not . isExhausted
    Matchers.ExhaustedIdentity -> pure . isExhausted
    IdentityCanReady -> \player -> do
      modifiers <- getModifiers player
      pure $ isExhausted player && CannotReady `notElem` modifiers && CannotBeReadiedByPlayerCards `notElem` modifiers
    ConfusedIdentity -> identityIsConfused
    IdentityCanBeHealed -> fmap (notElem CannotBeHealedByPlayerCards) . getModifiers
    IdentityWithoutToughStatus -> pure . not . playerIdentityTough . toAttrs
    StunnedIdentity -> identityIsStunned
    IdentityEngagedWith minionMatcher -> \ident -> do
      minions <- select minionMatcher
      pure
        . not
        . null
        $ playerIdentityMinions (toAttrs ident)
        `HashSet.intersection` minions
    IdentityWithId ident' -> pure . (== ident') . toId
    IdentityWithTrait trait -> fmap (member trait) . getTraits
    IdentityWithTitle t -> pure . (== t) . title . cdName . getCardDef
    IdentityIs def -> pure . (== toBaseCardCode (cdCardCode def)) . toBaseCardCode . toCardCode
    IdentityWithAtLeastBaseHitPoints -> \identity -> do
      (remaining, _, base) <- getRemainingHitPoints (toId identity)
      pure $ remaining >= base
    IdentityWithPrintedHitPoints matcher -> \identity -> do
      printed <- fromGameValue (unHp $ startingHP identity)
      gameValueMatches matcher (fromIntegral printed)
    IdentityWithDamage gameValueMatcher ->
      gameValueMatches gameValueMatcher . identityDamage
    IdentityWithCounters gameValueMatcher ->
      gameValueMatches gameValueMatcher . playerIdentityCounters . toAttrs
    IdentityMatchAll xs -> andM . traverse matchFilter xs
    IdentityCanThwart -> fmap (notElem CannotThwart) . getModifiers
    IdentityCanBasicAttack -> fmap (\ms -> CannotBasicAttack `notElem` ms && CannotAttack `notElem` ms) . getModifiers
    IdentityCanAttack -> fmap (notElem CannotAttack) . getModifiers
    IdentityCanDefendReady -> \identity -> do
      permitted <- identityMatches IdentityCanDefend (toId identity)
      free <- elem DefendWithoutExhausting <$> getModifiers identity
      basic <- notElem CannotBasicDefend <$> getModifiers identity
      pure $ permitted && basic && (free || not (isExhausted identity))
    IdentityCanDefend -> \identity -> do
      own <- notElem CannotDefend <$> getModifiers identity
      enemies <- selectList AttackingEnemy
      forbidden <- anyM (fmap (any (\case OnlyIdentityCanDefend who -> who /= toId identity; AttacksCannotBeDefended -> True; _ -> False)) . getModifiers . enemyRef) enemies
      pure $ own && not forbidden
    IdentityCanChangeMassForm -> fmap (\ms -> CannotChangeForm `notElem` ms && CannotChangeMassForm `notElem` ms) . getModifiers
    IdentityCanRecover -> fmap (notElem CannotRecover) . getModifiers
    IdentityCanChangeForm -> \identity -> do
      mods <- getModifiers identity
      pure $ CannotChangeForm `notElem` mods && (if isHero identity then CannotChangeToAlterEgoForm `notElem` mods else CannotChangeToHeroForm `notElem` mods)
    FirstPlayer -> \a -> do
      competitive <- getCompetition
      ps <- if isJust competitive then getPlayers else getAllPlayers
      pure $ case ps of
        fp : _ -> toId a == fp
        _ -> False
    You -> \a -> (== toId a) <$> getYou
    IdentityControllingAlly matcher -> \a -> selectAny (AllyControlledBy (IdentityWithId $ toId a) <> matcher)
    IdentityWithFewestRemainingHitPoints m' -> \a -> do
      candidates <- filterM (matchFilter m') . HashMap.elems =<< getsGame gamePlayers
      remaining <- for candidates \p -> (toId p,) . subtractNatural (identityDamage p) <$> getModifiedHp p
      pure $ case find ((== toId a) . fst) remaining of
        Just (_, mine) -> all ((>= mine) . snd) remaining
        Nothing -> False

gameSelectCharacter :: (HasGame m) => CharacterMatcher -> m (HashSet CharacterId)
gameSelectCharacter = \case
  EnemyCharacters matcher -> HashSet.fromList <$> selectMap toCharacter matcher
  CharacterWithAttachment matcher -> fromList . catMaybes <$> (traverse getAttachedCharacter =<< selectList matcher)
  CharacterWithId c -> HashSet.filter (== c) <$> gameSelectCharacter AnyCharacter
  CharacterCanBeHealed -> fromList <$> (filterM (fmap (notElem CannotBeHealedByPlayerCards) . getModifiers . characterRef) . toList =<< gameSelectCharacter AnyCharacter)
  CharacterWithDamage gameValueMatcher -> do
    villains <-
      map VillainCharacter
        <$> selectList (VillainWithDamage gameValueMatcher)
    minions <-
      map MinionCharacter
        <$> selectList (MinionWithDamage gameValueMatcher)
    identities <-
      map IdentityCharacter
        <$> selectList (IdentityWithDamage gameValueMatcher)
    allies <- map AllyCharacter <$> selectList (AllyWithDamage gameValueMatcher)
    pure . HashSet.fromList $ villains <> minions <> identities <> allies
  DamageableCharacter -> do
    let
      enemyToCharacter = \case
        EnemyMinionId mid -> MinionCharacter mid
        EnemyVillainId vid -> VillainCharacter vid
        EnemyAttachmentId aid -> AttachmentCharacter aid
    enemies <- selectMap enemyToCharacter DamageableEnemy
    identities <- selectMap IdentityCharacter AnyIdentity
    allies <- selectMap AllyCharacter AnyAlly
    pure . HashSet.fromList $ enemies <> identities <> allies
  AnyCharacter -> do
    villains <- selectMap VillainCharacter AnyVillain
    minions <- selectMap MinionCharacter AnyMinion
    identities <- selectMap IdentityCharacter AnyIdentity
    allies <- selectMap AllyCharacter AnyAlly
    pure . HashSet.fromList $ villains <> minions <> identities <> allies
  IdentityCharacters identityMatcher -> HashSet.fromList <$> selectMap IdentityCharacter identityMatcher
  AllyCharacters allyMatcher -> HashSet.fromList <$> selectMap AllyCharacter allyMatcher
  CharacterControlledBy identityMatcher -> do
    identities <- selectMap IdentityCharacter identityMatcher
    allies <- selectMap AllyCharacter (AllyControlledBy identityMatcher)
    pure . HashSet.fromList $ identities <> allies
  CharacterCanBeReadied -> do
    identities <- selectList Matchers.ExhaustedIdentity
    readyable <-
      filterM
        ( \ident -> do
            modifiers <- getModifiers ident
            hero <- selectAny (IdentityWithId ident <> HeroIdentity)
            pure $ CannotReady `notElem` modifiers && (not hero || CannotBeReadiedByPlayerCards `notElem` modifiers)
        )
        identities
    allies <- selectList (Matchers.ExhaustedAlly <> AllyCanReady)
    pure $ HashSet.fromList (map IdentityCharacter readyable <> map AllyCharacter allies)
  CharacterOtherThan character -> HashSet.delete character <$> gameSelectCharacter AnyCharacter
  CharacterWithTrait t -> do
    villains <- selectMap VillainCharacter (VillainWithTrait t)
    minions <- selectMap MinionCharacter (MinionWithTrait t)
    identities <- selectMap IdentityCharacter (IdentityWithTrait t)
    allies <- selectMap AllyCharacter (AllyWithTrait t)
    pure . HashSet.fromList $ villains <> minions <> identities <> allies
  CharacterCanGainTough -> do
    allCharacters <- gameSelectCharacter AnyCharacter
    fmap HashSet.fromList
      $ filterM
        ( \c -> do
            modifiers <- getModifiers (characterRef c)
            count <- getsGame $ \g -> case c of
              IdentityCharacter ident -> maybe 0 (\p -> let a = toAttrs p in (if playerIdentityTough a then 1 else 0) + playerIdentityAdditionalTough a) (lookup ident $ gamePlayers g)
              MinionCharacter mid -> maybe 0 (\m -> let a = toAttrs m in (if Minion.minionTough a then 1 else 0) + Minion.minionAdditionalTough a) (lookup mid $ gameMinions g)
              VillainCharacter vid -> maybe 0 (\v -> if villainIsTough v then 1 else 0) (lookup vid $ gameVillains g)
              AllyCharacter aid -> maybe 0 (\a -> if Ally.allyTough (toAttrs a) then 1 else 0) (lookup aid $ gameAllies g)
              AttachmentCharacter _ -> 0
            pure $ CanHaveAnyNumberOfToughStatusCards `elem` modifiers || count < 1 + sum [n | AdditionalToughStatusCards n <- modifiers]
        )
        (HashSet.toList allCharacters)
  CharacterWithStatus s -> getsGame \g ->
    let
      -- each character's stunned, confused and tough
      has :: (a -> (Bool, Bool, Bool)) -> (a -> CharacterId) -> HashMap k a -> [CharacterId]
      has statuses f = map f . filter (pick . statuses) . HashMap.elems
      pick (stunned, confused, tough) = case s of
        StunnedStatus -> stunned
        ConfusedStatus -> confused
        ToughStatus -> tough
     in
      HashSet.fromList
        $ has ((\a -> (a.playerIdentityStunned, a.playerIdentityConfused, a.playerIdentityTough)) . toAttrs) (IdentityCharacter . toId) (gamePlayers g)
        <> has ((\a -> (a.allyStunned, a.allyConfused, a.allyTough)) . toAttrs) (AllyCharacter . toId) (gameAllies g)
        <> has ((\a -> (a.villainStunned, a.villainConfused, a.villainTough)) . toAttrs) (VillainCharacter . toId) (gameVillains g)
        <> has ((\a -> (a.minionStunned, a.minionConfused, a.minionTough)) . toAttrs) (MinionCharacter . toId) (gameMinions g)
  HeroOrAllyWithFewestRemainingHitPoints -> do
    heroes <- traverse (\p -> (IdentityCharacter (toId p),) . (`subtractNatural'` (toAttrs p).playerIdentityDamage) <$> getModifiedHp p) . filter isHero . HashMap.elems =<< getsGame gamePlayers
    allies <- traverse (\a -> (AllyCharacter (toId a),) . (`subtractNatural'` getAllyDamage a) <$> Ally.getModifiedHitPoints (toAttrs a)) . HashMap.elems =<< getsGame gameAllies
    let characters = heroes <> allies
        fewest = minimum (map snd characters)
    pure . HashSet.fromList $ [c | not (null characters), (c, hp) <- characters, hp == fewest]
   where
    subtractNatural' hp dmg = subtractNatural dmg hp
  HeroOrAllyWithHighestAttack -> highestAttack (const $ pure True)
  AttachableHeroOrAllyWithHighestAttack -> highestAttack \a -> do
    modifiers <- getModifiers (AllyRef $ toId a)
    pure $ not $ any (`elem` modifiers) [CannotHaveAttachedCards, CannotHaveAttachedEncounterCards]
  CharacterOneOf matchers -> HashSet.unions <$> traverse gameSelectCharacter matchers
  CharacterMatches [] -> pure mempty
  CharacterMatches (x : xs) ->
    foldl' HashSet.intersection
      <$> gameSelectCharacter x
      <*> traverse gameSelectCharacter xs
 where
  highestAttack eligible = do
    heroes <- traverse (\p -> (IdentityCharacter (toId p),) <$> getIdentityHeroAttackDamage p) . filter isHero . HashMap.elems =<< getsGame gamePlayers
    allies <- traverse (\a -> (AllyCharacter (toId a),) <$> Ally.getModifiedAttack (toAttrs a)) =<< filterM eligible . HashMap.elems =<< getsGame gameAllies
    let characters = heroes <> allies
        highest = maximum (map snd characters)
    pure . HashSet.fromList $ [c | not (null characters), (c, atk) <- characters, atk == highest]

gameSelectEncounterCard ::
  (MonadRandom m, HasGame m) =>
  EncounterCardMatcher ->
  m (HashSet EncounterCard)
gameSelectEncounterCard m = case m of
  -- the nemesis set the player set aside during setup, as much of it as is still there
  NemesisSetFor identityId -> do
    mIdentity <- getsGame $ lookup identityId . entitiesPlayers . gameEntities
    key <- getDeckKeyFor identityId
    competitive <- isJust <$> getCompetition
    setAside <- if competitive then getSetAsideEncounterCardsForDeck key else getSetAsideEncounterCards
    pure $ case mIdentity of
      Nothing -> mempty
      Just iid ->
        HashSet.fromList
          $ filter ((== Just (getNemesisSet $ toCardCode iid)) . cdEncounterSet . getCardDef) setAside

gameSelectAlly :: (HasGame m) => AllyMatcher -> m (HashSet AllyId)
gameSelectAlly m = do
  allies <- inCompetitiveAreaOrExplicit (explicitAllyTargets m) (AllyRef . toId) . HashMap.elems =<< getsGame gameAllies
  result <- filterM (matchFilter m) allies
  pure $ HashSet.fromList $ map toId result
 where
  matchFilter x = case x of
    NotAlly m' -> fmap not . matchFilter m'
    AnyAlly -> pure . const True
    UnexhaustedAlly -> pure . not . isExhausted
    ExhaustedAlly -> pure . isExhausted
    AllyControlledBy identityMatcher -> \ally -> do
      identities <- select identityMatcher
      uncontrolled <- getsGame gameUncontrolledAllies
      pure $ toId ally `notElem` uncontrolled && member (getAllyController ally) identities
    AllyWithUses gameValueMatcher ->
      gameValueMatches gameValueMatcher . getAllyUses
    AllyWithDamage gameValueMatcher ->
      gameValueMatches gameValueMatcher . getAllyDamage
    AllyWithId ident' -> pure . (== ident') . toId
    AllyMatches xs -> andM . traverse matchFilter xs
    AllyWithTrait trait -> fmap (member trait) . getTraits
    AllySharingTraitWithController -> \ally ->
      getsGame (lookup (getAllyController ally) . gamePlayers) >>= \case
        Nothing -> pure False
        Just p -> not . null <$> liftA2 HashSet.intersection (getTraits ally) (getTraits p)
    AllyIs def -> pure . sameCard def . getCardDef
    AllyWithPrintedCostAtLeast n -> pure . (>= fromIntegral n) . max 0 . fromMaybe 0 . cdCost . getCardDef
    AllyThatCanLeavePlay -> \ally -> do
      modifiers <- getModifiers (AllyRef $ toId ally)
      pure
        $ Permanent
        `notElem` cdKeywords (getCardDef ally)
        && not (any (`elem` modifiers) [CannotLeavePlay, CannotLeavePlayByCardAbilities, CannotBeDiscarded])
    AllyThatWouldBeDiscardedByDefeat -> \ally -> do
      allowed <- matchFilter AllyThatCanLeavePlay ally
      pure $ allowed && not (any (\case Victory _ -> True; _ -> False) $ cdKeywords $ getCardDef ally)
    AllyCanReady -> \ally -> do
      modifiers <- getModifiers ally
      pure $ CannotReady `notElem` modifiers && CannotBeReadiedByPlayerCards `notElem` modifiers
    AllyWithTitle t -> pure . (== t) . title . cdName . getCardDef
    AllyNamedAs t -> pure . (\n -> title n == t || subtitle n == Just t) . cdName . getCardDef
    AllyWithAttackAtLeast n -> \ally -> (>= n) <$> Ally.getModifiedAttack (toAttrs ally)
    AllyWithThwartAtLeast n -> \ally -> (>= n) <$> Ally.getModifiedThwart (toAttrs ally)
    AllyWithRemainingHitPoints n -> \ally -> (== n) . subtractNatural (getAllyDamage ally) <$> Ally.getModifiedHitPoints (toAttrs ally)
    AllyWithUpgrade u -> \ally ->
      selectAny $ u <> UpgradeOneOf (map UpgradeWithId . toList . allyUpgrades $ toAttrs ally)

gameSelectSupport :: (HasGame m) => SupportMatcher -> m (HashSet SupportId)
gameSelectSupport matcher = do
  found <- gameSelectSupportUnscoped matcher
  fromList <$> inCompetitiveAreaOnly SupportRef (HashSet.toList found)

gameSelectUpgrade :: (HasGame m) => UpgradeMatcher -> m (HashSet UpgradeId)
gameSelectUpgrade m = do
  upgrades <- inCompetitiveAreaOnlyOrExplicit (explicitUpgradeTargets m) (UpgradeRef . toId) . HashMap.elems =<< getsGame gameUpgrades
  result <- filterM (matchFilter m) upgrades
  pure $ HashSet.fromList $ map toId result
 where
  matchFilter x = case x of
    UpgradeCanBeReadied -> \u -> do
      modifiers <- getModifiers u
      pure $ isExhausted u && CannotReady `notElem` modifiers
    UnexhaustedUpgrade -> pure . not . isExhausted
    UpgradeWithUses gameValueMatcher ->
      gameValueMatches gameValueMatcher . getUpgradeUses
    UpgradeCanFlipPsiEnergy -> \u ->
      if PsiEnergy `member` cdTraits (getCardDef u) then notElem CannotFlipPsiEnergy <$> getModifiers u else pure False
    UpgradeWithTrait t -> \u ->
      if member t (cdTraits $ getCardDef u)
        then pure True
        else case Upgrade.getUpgradeAttachmentTraits u of
          Just traits | member t traits -> not <$> isBlanked u
          _ -> pure False
    UpgradeWithKeyword k -> pure . member k . cdKeywords . getCardDef
    UpgradeWithResolvableSpecial -> \u -> do
      blanked <- isBlanked u
      if blanked
        then pure False
        else case u of
          Upgrade a -> upgradeSpecialCanResolve a
    UpgradeControlledBy identityMatcher -> \upgrade -> do
      identities <- select identityMatcher
      pure $ getUpgradeController upgrade `member` identities
    UpgradeMatches xs -> andM . traverse matchFilter xs
    UpgradeIs def -> pure . sameCard def . getCardDef
    UpgradeWithCard m -> pure . cardMatch m . getCardDef
    FacedownUpgrade -> pure . upgradeFaceDown . toAttrs
    UpgradeNamed name -> pure . (== name) . cdName . getCardDef
    UpgradeWithId upgradeId -> pure . (== upgradeId) . toId
    UpgradeOneOf xs -> orM . traverse matchFilter xs
    NotUpgrade m' -> fmap not . matchFilter m'
    UpgradeOnAlly allyMatcher -> \upgrade -> case upgradeAttachedAlly (toAttrs upgrade) of
      Nothing -> pure False
      Just aid -> selectAny (AllyWithId aid <> allyMatcher)
    UpgradeOnIdentity identityMatcher -> \upgrade -> do
      identities <- select identityMatcher
      let a = toAttrs upgrade
      pure
        $ getUpgradeController upgrade
        `member` identities
        && isNothing (upgradeAttachedAlly a)
        && isNothing (upgradeAttachedEnemy a)
        && isNothing (upgradeAttachedSideScheme a)
        && isNothing (upgradeAttachedMainScheme a)
    UpgradeThatCanLeavePlay -> pure . not . isPermanent
    UpgradeThatCanBeDiscarded -> \upgrade -> do
      modifiers <- getModifiers (toRef upgrade)
      pure $ not (isPermanent upgrade) && CannotBeDiscarded `notElem` modifiers
    UpgradeWithHighestCostAmong m' -> \upgrade -> do
      candidates <- filterM (matchFilter m') . HashMap.elems =<< getsGame gameUpgrades
      let cost = fromMaybe 0 . cdCost . getCardDef
      pure $ toId upgrade `elem` map toId candidates && cost upgrade == maximum (map cost candidates)

gameSelectAttachment ::
  (HasGame m) => AttachmentMatcher -> m (HashSet AttachmentId)
gameSelectAttachment m = do
  snapshot <- getGame
  let real =
        [ ( toId a
          , getCardDef a
          , Attachment.attachmentEnemy $ toAttrs a
          , Attachment.attachmentCounters $ toAttrs a
          , hasPrintedHeroActionOrResponse $ toCardCode $ getCardDef a
          , getAbilities a
          )
        | a <- HashMap.elems $ gameAttachments snapshot
        ]
  aliases <- fmap catMaybes $ for (HashMap.elems $ gameUpgrades snapshot) $ \u -> case Upgrade.getUpgradeAttachmentTraits u of
    Nothing -> pure Nothing
    Just traits -> do
      blanked <- isBlanked u
      let UpgradeId uuid = toId u
          abilities = getAbilities u
          hero = any ((`elem` [Ability.HeroAction, Ability.HeroResponse]) . abilityType) abilities
      pure
        $ if blanked
          then Nothing
          else
            Just
              ( AttachmentId uuid
              , (getCardDef u) {cdCardType = AttachmentType, cdTraits = cdTraits (getCardDef u) <> traits}
              , upgradeAttachedEnemy $ toAttrs u
              , 0
              , hero
              , abilities
              )
  attachments <-
    inCompetitiveAreaOnlyOrExplicit
      (explicitAttachmentTargets snapshot m)
      (\(aid, _, _, _, _, _) -> AttachmentRef aid)
      (real <> aliases)
  result <- filterM (matchFilter m) attachments
  pure $ HashSet.fromList [aid | (aid, _, _, _, _, _) <- result]
 where
  matchFilter x a@(aid, def, enemy, counters, hero, abilities) = case x of
    AttachmentWithId ident' -> pure $ ident' == aid
    AttachmentWithPrintedHeroActionOrResponse -> pure hero
    AttachmentOnVillain matcher -> case enemy of
      Just (EnemyVillainId vid) -> villainMatches matcher vid
      _ -> pure False
    DiscardableAttachment -> do
      modifiers <- getModifiers (AttachmentRef aid)
      pure $ Permanent `notElem` cdKeywords def && all (`notElem` modifiers) [CannotBeDiscarded, CannotLeavePlay, CannotLeavePlayByCardAbilities]
    AttachmentOnEnemyWithHeroAbility eid ->
      pure
        $ enemy
        == Just eid
        && any ((`elem` [Ability.HeroAction, Ability.HeroResponse]) . abilityType) abilities
    AttachmentIs expected -> pure $ sameCard expected def
    AttachmentWithTrait t -> pure $ member t $ cdTraits def
    AttachmentWithCounters matcher -> gameValueMatches matcher counters
    AttachmentOnEnemy eid -> pure $ enemy == Just eid
    AttachmentMatches xs -> allM (\matcher -> matchFilter matcher a) xs

gameSelectEnvironment ::
  (HasGame m) => EnvironmentMatcher -> m (HashSet EnvironmentId)
gameSelectEnvironment m = do
  environments <- inCompetitiveAreaOnly (EnvironmentRef . toId) . HashMap.elems =<< getsGame gameEnvironments
  pure . HashSet.fromList . map toId $ filter matches environments
 where
  matches e = case m of
    AnyEnvironment -> True
    EnvironmentIs def -> sameCard def (getCardDef e)
    EnvironmentWithTitle t -> title (cdName $ getCardDef e) == t
    EnvironmentWithTrait t -> t `member` cdTraits (getCardDef e)
    EnvironmentInPlayAreaOf ident -> field EnvironmentOwner e == Just ident

-- TODO: Chase them down looks at removed
gameSelectEnemy :: (HasGame m) => EnemyMatcher -> m (HashSet EnemyId)
gameSelectEnemy m = do
  villains <- inCompetitiveAreaOrExplicit (explicitEnemyTargets m) (VillainRef . toId) . HashMap.elems =<< getsGame gameVillains
  minions <- inCompetitiveAreaOrExplicit (explicitEnemyTargets m) (MinionRef . toId) . HashMap.elems =<< getsGame gameMinions
  removedVillains <-
    inCompetitiveAreaOnlyOrExplicit (explicitEnemyTargets m) (VillainRef . toId)
      =<< if includeRemoved
        then HashMap.elems <$> getsGame (view (removedEntitiesL . villainsL))
        else pure []
  removedMinions <-
    inCompetitiveAreaOnlyOrExplicit (explicitEnemyTargets m) (MinionRef . toId)
      =<< if includeRemoved
        then HashMap.elems <$> getsGame (view (removedEntitiesL . minionsL))
        else pure []
  villains' <-
    map (EnemyVillainId . toId)
      <$> filterM (`goVillain` m) (villains <> removedVillains)
  minions' <-
    map (EnemyMinionId . toId)
      <$> filterM (`goMinion` m) (minions <> removedMinions)
  ice <- attackableAttachmentTargets m
  pure $ HashSet.fromList (villains' <> minions' <> ice)
 where
  includeRemoved = case m of
    DefeatedEnemy _ -> True
    _ -> False
  goVillain e = \case
    DefeatedEnemy m' -> goVillain e m'
    AttackableByAlly aid -> do
      controller <- getsGame (fmap getAllyController . lookup aid . gameAllies)
      case controller of
        Nothing -> pure False
        Just who -> do
          active <- member (toId e) <$> gameSelectVillain ActiveVillain
          ignoresGuard <- elem IgnoresGuard <$> getModifiers (AllyRef aid)
          guarded <- if active && not ignoresGuard then selectAny (MinionWithKeyword Guard <> MinionEngagedWith (IdentityWithId who)) else pure False
          untouchable <- elem CannotBeAttacked <$> getModifiers (VillainRef $ toId e)
          barred <- barredFromAlly (IdentityWithId who) (EnemyVillainId $ toId e) e
          ownModifiers <- getModifiers (AllyRef aid)
          pure $ not (any (\case OnlyAttackAttachment _ -> True; _ -> False) ownModifiers || guarded || untouchable || barred || CannotAttackEnemy (EnemyVillainId $ toId e) `elem` ownModifiers)
    EnemyWithId enemyId -> pure $ case enemyId of
      EnemyVillainId villainId -> toId e == villainId
      EnemyMinionId _ -> False
      EnemyAttachmentId _ -> False
    AnyEnemy -> pure True
    EnemyInPlay -> getsGame $ HashMap.member (toId e) . gameVillains
    VillainEnemy -> pure True
    MinionEnemy -> pure False
    UniqueEnemy -> pure $ cdUnique (getCardDef e)
    EnemyCanTakeDamageFrom source -> not <$> cannotTakeDamageFrom (VillainRef $ toId e) source
    DamageableEnemy -> pure True
    -- guard keeps the player it is engaged with from attacking "the villain", the
    -- active one (TWC insert p.6); a 'CannotBeAttacked' villain is attacked by no one
    AttackableBy attacker -> do
      active <- member (toId e) <$> gameSelectVillain ActiveVillain
      attackers <- selectList attacker
      ignoreGuard <- anyM (fmap (elem IgnoresGuard) . getModifiers . IdentityRef) attackers
      guarded <- if active && not ignoreGuard then selectAny (MinionWithKeyword Guard <> MinionEngagedWith attacker) else pure False
      untouchable <- elem CannotBeAttacked <$> getModifiers (VillainRef $ toId e)
      barred <- barredFrom attacker (EnemyVillainId $ toId e) e
      pure $ not (guarded || untouchable || barred)
    UndefendedEnemy ->
      pure $ maybe False (not . attackDefended) (villainAttackDetails e)
    AttackingEnemy -> pure $ isJust (villainAttackDetails e)
    EnemyAttackingCharacter cm -> maybe (pure False) (\a -> member (attackCharacter a) <$> gameSelectCharacter cm) (villainAttackDetails e)
    NotEnemy m' -> not <$> goVillain e m'
    EnemyWithRemainingHitPoints value -> gameValueMatches value =<< getEnemyRemainingHitPoints (EnemyVillainId $ toId e)
    EnemyWithTitle name -> pure $ title (cdName $ getCardDef e) == name
    EnemyIs def -> pure $ sameCard def (getCardDef e)
    EnemyMatches xs -> allM (goVillain e) xs
    EnemyWithUpgrade u ->
      selectAny $ u <> UpgradeOneOf (map UpgradeWithId . toList . villainUpgrades $ toAttrs e)
    EnemyWithAttachment m' -> not . null . HashSet.intersection (Villain.villainAttachments $ toAttrs e) <$> select m'
    EnemyWithSchemeLessThanAllyThwart aid -> (<) <$> Villain.getModifiedScheme (toAttrs e) <*> allyThwart aid
    EnemyWithHighestAttack -> (==) <$> Villain.getModifiedAttack (toAttrs e) <*> highestAttack
    StunnedEnemy -> Villain.isStunned (toAttrs e)
    EnemyWithTrait t -> member t <$> getTraits e
    ConfusedEnemy -> Villain.isConfused (toAttrs e)
    EnemyCanBeConfused -> do
      stalwart <- elem Stalwart <$> Villain.getModifiedKeywords (toAttrs e)
      confused <- Villain.isConfused (toAttrs e)
      forbidden <- elem CannotBeConfused <$> getModifiers (VillainRef $ toId e)
      pure $ not (stalwart || confused || forbidden)
    EnemyCanBeStunned -> do
      stalwart <- elem Stalwart <$> Villain.getModifiedKeywords (toAttrs e)
      stunned <- Villain.isStunned (toAttrs e)
      forbidden <- elem CannotBeStunned <$> getModifiers (VillainRef $ toId e)
      pure $ not (stalwart || stunned || forbidden)
    EnemyWithLowestAttack -> (==) <$> Villain.getModifiedAttack (toAttrs e) <*> (minimum <$> attacks)
    EnemyWithHighestScheme -> (==) <$> Villain.getModifiedScheme (toAttrs e) <*> (maximum <$> schemes)
    EnemyWithLowestScheme -> (==) <$> Villain.getModifiedScheme (toAttrs e) <*> (minimum <$> schemes)
  goMinion e = \case
    DefeatedEnemy m' -> goMinion e m'
    EnemyWithId enemyId -> pure $ case enemyId of
      EnemyVillainId _ -> False
      EnemyMinionId minionId -> toId e == minionId
      EnemyAttachmentId _ -> False
    AnyEnemy -> pure True
    EnemyInPlay -> getsGame $ HashMap.member (toId e) . gameMinions
    VillainEnemy -> pure False
    MinionEnemy -> pure True
    UniqueEnemy -> pure $ cdUnique (getCardDef e)
    EnemyCanTakeDamageFrom source -> not <$> cannotTakeDamageFrom (MinionRef $ toId e) source
    DamageableEnemy -> pure True
    AttackableBy attacker -> not <$> barredFrom attacker (EnemyMinionId $ toId e) e
    AttackableByAlly aid -> do
      allyMods <- getModifiers (AllyRef aid)
      untouchable <- elem CannotBeAttacked <$> getModifiers (MinionRef $ toId e)
      let allowed = not (any (\case OnlyAttackAttachment _ -> True; _ -> False) allyMods) && not untouchable && CannotAttackMinions `notElem` allyMods && CannotAttackEnemy (EnemyMinionId $ toId e) `notElem` allyMods
      controller <- getsGame (fmap getAllyController . lookup aid . gameAllies)
      maybe (pure False) (\c -> (allowed &&) . not <$> barredFromAlly (IdentityWithId c) (EnemyMinionId $ toId e) e) controller
    UndefendedEnemy ->
      pure $ maybe False (not . attackDefended) (minionAttackDetails e)
    AttackingEnemy -> pure $ isJust (minionAttackDetails e)
    EnemyAttackingCharacter cm -> maybe (pure False) (\a -> member (attackCharacter a) <$> gameSelectCharacter cm) (minionAttackDetails e)
    NotEnemy m' -> not <$> goMinion e m'
    EnemyWithRemainingHitPoints value -> gameValueMatches value =<< getEnemyRemainingHitPoints (EnemyMinionId $ toId e)
    EnemyWithTitle name -> pure $ title (cdName $ getCardDef e) == name
    EnemyIs def -> pure $ sameCard def (getCardDef e)
    EnemyMatches xs -> allM (goMinion e) xs
    EnemyWithUpgrade u ->
      selectAny $ u <> UpgradeOneOf (map UpgradeWithId . toList . minionUpgrades $ toAttrs e)
    EnemyWithAttachment m' -> not . null . HashSet.intersection (minionAttachments $ toAttrs e) <$> select m'
    EnemyWithSchemeLessThanAllyThwart aid -> (<) <$> Minion.getModifiedScheme (toAttrs e) <*> allyThwart aid
    EnemyWithHighestAttack -> (==) <$> Minion.getModifiedAttack (toAttrs e) <*> highestAttack
    StunnedEnemy -> Minion.isStunned (toAttrs e)
    EnemyWithTrait t -> member t <$> getTraits e
    ConfusedEnemy -> Minion.isConfused (toAttrs e)
    EnemyCanBeConfused -> do
      stalwart <- elem Stalwart <$> Minion.getModifiedKeywords (toAttrs e)
      confused <- Minion.isConfused (toAttrs e)
      forbidden <- elem CannotBeConfused <$> getModifiers (MinionRef $ toId e)
      pure $ not (stalwart || confused || forbidden)
    EnemyCanBeStunned -> do
      stalwart <- elem Stalwart <$> Minion.getModifiedKeywords (toAttrs e)
      stunned <- Minion.isStunned (toAttrs e)
      forbidden <- elem CannotBeStunned <$> getModifiers (MinionRef $ toId e)
      pure $ not (stalwart || stunned || forbidden)
    EnemyWithLowestAttack -> (==) <$> Minion.getModifiedAttack (toAttrs e) <*> (minimum <$> attacks)
    EnemyWithHighestScheme -> (==) <$> Minion.getModifiedScheme (toAttrs e) <*> (maximum <$> schemes)
    EnemyWithLowestScheme -> (==) <$> Minion.getModifiedScheme (toAttrs e) <*> (minimum <$> schemes)

  allyThwart aid = getsGame (lookup aid . gameAllies) >>= maybe (pure 0) (Ally.getModifiedThwart . toAttrs)

  -- a player kept from attacking the enemy's title (Fear of Kang)
  barredFrom attacker target e = do
    players <- selectList attacker
    restricted <- anyM (fmap not . (`canPlayerDamageEnemy` target)) players
    unaffordable <- anyM (fmap not . (`attackTargetCostsAffordable` target)) players
    mods <- (<>) <$> concatMapM getModifiers players <*> getModifiers (enemyRef target)
    pure
      $ (case target of EnemyVillainId _ -> CannotAttackVillain `elem` mods; _ -> False)
      || CannotBeAttacked
      `elem` mods
      || restricted
      || unaffordable
      || title (cdName $ getCardDef e)
      `elem` [t | CannotAttackTitled t <- mods]
      || any (\case OnlyAttackAttachment _ -> True; CannotAttackEnemy enemy -> enemy == target; _ -> False) mods
  barredFromAlly attacker target e = do
    players <- selectList attacker
    restricted <- anyM (fmap not . (`canPlayerDamageEnemy` target)) players
    unaffordable <- anyM (fmap not . (`attackTargetCostsAffordable` target)) players
    mods <- concatMapM getModifiers players
    pure $ restricted || unaffordable || title (cdName $ getCardDef e) `elem` [t | CannotAttackTitled t <- mods]
  highestAttack = do
    villains <- traverse (Villain.getModifiedAttack . toAttrs) . HashMap.elems =<< getsGame gameVillains
    minions <- traverse (Minion.getModifiedAttack . toAttrs) . HashMap.elems =<< getsGame gameMinions
    pure $ foldl' max 0 (villains <> minions)
  -- every enemy's ATK, and SCH: an enemy is one of them, so neither is empty when asked
  attacks = do
    villains <- traverse (Villain.getModifiedAttack . toAttrs) . HashMap.elems =<< getsGame gameVillains
    minions <- traverse (Minion.getModifiedAttack . toAttrs) . HashMap.elems =<< getsGame gameMinions
    pure $ villains <> minions
  schemes = do
    villains <- traverse (Villain.getModifiedScheme . toAttrs) . HashMap.elems =<< getsGame gameVillains
    minions <- traverse (Minion.getModifiedScheme . toAttrs) . HashMap.elems =<< getsGame gameMinions
    pure $ villains <> minions

gameSelectVillain :: (HasGame m) => VillainMatcher -> m (HashSet VillainId)
gameSelectVillain m = do
  villains <- inCompetitiveAreaOrExplicit (explicitVillainTargets m) (VillainRef . toId) . HashMap.elems =<< getsGame gameVillains
  -- removedVillains <- toList <$> getsGame (view (removedEntitiesL . villainsL))
  -- result <- filterM (matchFilter m) (villains <> removedVillains)
  result <- filterM (matchFilter m) villains
  pure $ HashSet.fromList $ map toId result
 where
  matchFilter x = case x of
    -- "the villain": the active villain of "your" area
    ActiveVillain -> \v -> do
      aid <- areaId <$> getYourArea
      getsGame \g -> activeVillainIn g aid == Just (toId v)
    AnyVillain -> pure . const True
    VillainWithCardCode code -> pure . (== code) . toCardCode . getCardDef
    VillainWithId ident' -> pure . (== ident') . toId
    VillainWithDamage gameValueMatcher ->
      gameValueMatches gameValueMatcher . villainDamage
    VillainWithToughStatus -> pure . villainIsTough
    VillainWithTrait trait -> fmap (member trait) . getTraits
    VillainMatches xs -> andM . traverse matchFilter xs

gameSelectMinion :: (HasGame m) => MinionMatcher -> m (HashSet MinionId)
gameSelectMinion m = do
  minions <- inCompetitiveAreaOrExplicit (explicitMinionTargets m) (MinionRef . toId) . HashMap.elems =<< getsGame gameMinions
  removedMinions <-
    inCompetitiveAreaOnlyOrExplicit (explicitMinionTargets m) (MinionRef . toId) =<< case m of
      LeftPlayMinion _ -> HashMap.elems <$> getsGame (view (removedEntitiesL . minionsL))
      _ -> pure []
  result <- filterM (matchFilter m) (minions <> removedMinions)
  pure $ HashSet.fromList $ map toId result
 where
  matchFilter x = case x of
    AnyMinion -> pure . const True
    MinionCanBeDefeated -> fmap (notElem CannotBeDefeated) . getModifiers
    MinionWithFewerRemainingHitPointsThanAlly aid -> \minion ->
      (<) <$> getEnemyRemainingHitPoints (EnemyMinionId $ toId minion) <*> getCharacterRemainingHitPoints (AllyCharacter aid)
    MinionCanBeDiscarded -> \minion -> do
      modifiers <- getModifiers minion
      keywords <- Minion.getModifiedKeywords (toAttrs minion)
      pure
        $ Permanent
        `notElem` keywords
        && not
          ( any
              (`elem` modifiers)
              [CannotBeDiscarded, CannotLeavePlay, CannotLeavePlayByCardAbilities]
          )
    LeftPlayMinion m' -> matchFilter m'
    MinionWithId ident' -> pure . (== ident') . toId
    MinionWithTrait trait -> fmap (member trait) . getTraits
    MinionWithDamage gameValueMatcher ->
      gameValueMatches gameValueMatcher . getMinionDamage
    MinionEngagedWith identityMatcher -> \minion -> do
      identities <- select identityMatcher
      backup <- getsGame (map snd . gameAosBackup)
      pure $ toId minion `notElem` backup && isNothing (Minion.minionAttachedTo $ toAttrs minion) && getMinionEngagedIdentity minion `member` identities
    MinionIs cardDef -> pure . sameCard cardDef . getCardDef
    MinionWithHighestPrintedHitPoints -> \minion -> do
      minions <- HashMap.elems <$> getsGame gameMinions
      let highest = maximum $ map (unHp . getMinionPrintedHitPoints) minions
      pure . (== highest) . unHp $ getMinionPrintedHitPoints minion
    MinionWithHighestBaseHitPoints -> \minion -> do
      minions <- HashMap.elems <$> getsGame gameMinions
      highest <- maximum . (0 :) <$> traverse Minion.getMinionBaseHitPoints minions
      (== highest) <$> Minion.getMinionBaseHitPoints minion
    -- a keyword it gains counts, as guard from Defensive Programming
    MinionWithKeyword k -> fmap (elem k) . Minion.getModifiedKeywords . toAttrs
    MinionCanBeStunned -> \minion -> enemyMatches EnemyCanBeStunned (EnemyMinionId $ toId minion)
    StunnedMinion -> Minion.isStunned . toAttrs
    MinionMatches xs -> andM . traverse matchFilter xs
    NotMinion m' -> fmap not . matchFilter m'
    MinionWithAttachment attachmentMatcher -> \minion -> do
      attachments <- select attachmentMatcher
      pure . not . null $ minionAttachments (toAttrs minion) `HashSet.intersection` attachments
    MinionWithHighestPrintedHitPointsAmong m' -> \minion -> do
      minions <- HashMap.elems <$> getsGame gameMinions
      candidates <- filterM (matchFilter m') minions
      let highest = maximum $ map (unHp . getMinionPrintedHitPoints) candidates
      pure
        $ toId minion
        `elem` map toId candidates
        && unHp (getMinionPrintedHitPoints minion)
        == highest
    MinionWithMostRemainingHitPointsAmong m' -> \minion -> do
      minions <- HashMap.elems <$> getsGame gameMinions
      candidates <- filterM (matchFilter m') minions
      let remaining mn = subtractNatural (getMinionDamage mn) <$> Minion.getModifiedHitPoints (toAttrs mn)
      most <- maximum . (0 :) <$> traverse remaining candidates
      mine <- remaining minion
      pure $ toId minion `elem` map toId candidates && mine == most

gameSelectTreachery :: (HasGame m) => TreacheryMatcher -> m (HashSet TreacheryId)
gameSelectTreachery = \case
  AnyTreachery -> do
    treacheries <- HashMap.elems <$> getsGame gameTreacheries
    pure $ HashSet.fromList $ map toId treacheries

gameSelectScheme :: (HasGame m) => SchemeMatcher -> m (HashSet SchemeId)
gameSelectScheme m = fmap HashSet.fromList . inCompetitiveAreaOrExplicit (explicitSchemeTargets m) SchemeRef . HashSet.toList =<< gameSelectAnyScheme m

gameSelectCountScheme ::
  (HasGame m) => QueryCount SchemeMatcher -> SchemeMatcher -> m Natural
gameSelectCountScheme aggregate matcher = do
  schemes <- toList <$> gameSelectScheme matcher
  case aggregate of
    SchemeThreat -> do
      let
        toThreat = \case
          SchemeMainSchemeId sid -> do
            mMainScheme <- getsGame (lookup sid . gameMainSchemes)
            pure $ maybe 0 getMainSchemeThreat mMainScheme
          SchemeSideSchemeId sid -> do
            mSideScheme <- getsGame (lookup sid . gameSideSchemes)
            pure $ maybe 0 getSideSchemeThreat mSideScheme
      sum <$> traverse toThreat schemes

gameSelectSideScheme ::
  (HasGame m) => SideSchemeMatcher -> m (HashSet SideSchemeId)
gameSelectSideScheme m = do
  sideSchemes <- inCompetitiveAreaOrExplicit (explicitSideSchemeTargets m) (SideSchemeRef . toId) . HashMap.elems =<< getsGame gameSideSchemes
  removedSideSchemes <-
    inCompetitiveAreaOnlyOrExplicit (explicitSideSchemeTargets m) (SideSchemeRef . toId) =<< case m of
      LeftPlaySideScheme _ -> HashMap.elems <$> getsGame (view (removedEntitiesL . sideSchemesL))
      _ -> pure []
  result <- filterM (matchFilter m) (sideSchemes <> removedSideSchemes)
  pure $ HashSet.fromList $ map toId result
 where
  matchFilter = \case
    AnySideScheme -> pure . const True
    LeftPlaySideScheme m' -> matchFilter m'
    CrisisSideScheme -> \scheme -> if isCrisis scheme then (\blank mods -> not blank && SuppressSchemeIcons `notElem` mods) <$> isBlanked scheme <*> getModifiers scheme else pure False
    SideSchemeMatchAll matchers -> \scheme -> and <$> traverse (\matcher -> matchFilter matcher scheme) matchers
    SideSchemeWithCard matcher -> pure . cardMatch matcher
    SideSchemeIs def -> pure . sameCard def . getCardDef
    SideSchemeWithId ident -> pure . (== ident) . toId
    SideSchemeWithKeyword keyword -> pure . elem keyword . cdKeywords . getCardDef
    SideSchemeWithUpgrade upgradeMatcher -> \sideScheme -> do
      upgrades <- select upgradeMatcher
      attached <- getsGame $ filter ((== Just (toId sideScheme)) . upgradeAttachedSideScheme . toAttrs) . HashMap.elems . gameUpgrades
      pure $ any ((`member` upgrades) . toId) attached
    NotSideScheme m' -> fmap not . matchFilter m'
    SignatureSchemeOf villainMatcher -> \sideScheme -> do
      villains <- select villainMatcher
      getsGame \g -> any (`member` villains) [v | (v, sid) <- HashMap.toList (gameSignatureSchemes g), sid == toId sideScheme]

gameSelectCountIdentity ::
  (HasGame m) => QueryCount IdentityMatcher -> IdentityMatcher -> m Natural
gameSelectCountIdentity aggregate matcher = do
  identities <-
    filterM (identityMatches matcher . toId) . HashMap.elems =<< getsGame gamePlayers
  case aggregate of
    SustainedDamage -> pure . sum $ map identityDamage identities
    HeroAttackDamage -> sum <$> traverse getIdentityHeroAttackDamage identities
    HeroThwartValue -> sum <$> traverse getIdentityHeroThwart identities

-- | The allies' modified ATK or THW, summed.
gameSelectCountAlly ::
  (HasGame m) => QueryCount AllyMatcher -> AllyMatcher -> m Natural
gameSelectCountAlly aggregate matcher = do
  allies <- filterM (allyMatches matcher . toId) . HashMap.elems =<< getsGame gameAllies
  sum <$> for allies \ally -> case aggregate of
    AllyAttackValue -> Ally.getModifiedAttack (toAttrs ally)
    AllyThwartValue -> Ally.getModifiedThwart (toAttrs ally)
    AllyDamageValue -> pure (getAllyDamage ally)

{- | An identity's hero ATK while attacking a particular enemy. Event attacks that
use ATK (Quick Strike) use this rather than the out-of-attack stat.
-}
