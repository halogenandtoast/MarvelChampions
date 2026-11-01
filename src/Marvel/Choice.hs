module Marvel.Choice where

import Marvel.Prelude

import Marvel.Ability.Types hiding (Attack, Thwart)
import Marvel.Ability.Types qualified as Ability
import Marvel.Card.Def ( printedResources )
import Marvel.ActiveCost.Types
import Marvel.Cost (getAbilityCost, canAffordCost)
import Marvel.Cost.Types (Cost (NoCost))
import Marvel.Damage
import Marvel.Exception
import Marvel.Matchers (identityMatches)
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword (Keyword (Guard, Patrol, Overkill, Piercing, Ranged))
import Marvel.Matchers.Types
import Marvel.Message
import Marvel.Modifier.Types
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Window (Window (..), WindowTiming (..), windowMatches)
import Marvel.Window qualified as W

choiceMessages ::
  (HasQueue m, HasGame m, MonadThrow m, MonadRandom m) =>
  IdentityId ->
  Choice ->
  m [Message]
choiceMessages ident = asYou ident . resolveChoice ident

-- | What a choice does, with "you" the player who chose it.
resolveChoice ::
  (HasQueue m, HasGame m, MonadThrow m, MonadRandom m) =>
  IdentityId ->
  Choice ->
  m [Message]
resolveChoice ident = \case
  Run msgs -> pure msgs
  Label _ choices -> concatMapM (choiceMessages ident) choices
  TargetLabel _ choices -> concatMapM (choiceMessages ident) choices
  CardLabel _ choice -> choiceMessages ident choice
  EndTurn -> pure [IdentityMessage ident EndedTurn]
  CreateEffect def source targetChoice -> case targetChoice of
    ChooseAPlayer -> do
      targets <- getPlayers
      let f = CreatedEffect def source . IdentityEntity . IdentityWithId
      -- a choice is asked even of one: the player picks what their card does
      pure [Ask ident $ ChooseOne [TargetLabel (toRef x) [Run [f x]] | x <- targets]]
    TargetMatches entityMatcher -> case entityMatcher of
      EventsMatching _ -> pure [CreatedEffect def source entityMatcher]
      SourceEntity target -> pure [CreatedEffect def source (SourceEntity target)]
      IdentityEntity matcher -> do
        targets <- selectList matcher
        let f = CreatedEffect def source . IdentityEntity . IdentityWithId
        case targets of
          [] -> throwM NoChoices
          [x] -> pure [f x]
          xs ->
            pure
              [ Ask ident $
                  ChooseOne
                    [TargetLabel (toRef x) [Run [f x]] | x <- xs]
              ]
      MinionEntity matcher -> do
        targets <- selectList matcher
        let f = CreatedEffect def source . MinionEntity . MinionWithId
        case targets of
          [] -> throwM NoChoices
          [x] -> pure [f x]
          xs -> pure [Ask ident $ ChooseOne [TargetLabel (toRef x) [Run [f x]] | x <- xs]]
      AllyEntity matcher -> do
        targets <- selectList matcher
        let f = CreatedEffect def source . AllyEntity . AllyWithId
        case targets of
          [] -> throwM NoChoices
          [x] -> pure [f x]
          xs ->
            pure
              [ Ask ident $
                  ChooseOne [TargetLabel (toRef x) [Run [f x]] | x <- xs]
              ]
      SideSchemeEntity matcher -> do
        targets <- selectList matcher
        let f = CreatedEffect def source . SideSchemeEntity . SideSchemeWithId
        case targets of
          [] -> throwM NoChoices
          [x] -> pure [f x]
          xs -> pure [Ask ident $ ChooseOne [TargetLabel (toRef x) [Run [f x]] | x <- xs]]
      MainSchemeEntity sid -> pure [CreatedEffect def source (MainSchemeEntity sid)]
      VillainsAndEngagedMinions you -> pure [CreatedEffect def source (VillainsAndEngagedMinions you)]
      UpgradeEntity matcher -> do
        targets <- selectList matcher
        let f = CreatedEffect def source . UpgradeEntity . UpgradeWithId
        case targets of
          [] -> throwM NoChoices
          [x] -> pure [f x]
          xs -> pure [Ask ident $ ChooseOne [TargetLabel (toRef x) [Run [f x]] | x <- xs]]
      EnemyEntity matcher -> do
        targets <- selectList matcher
        let f = CreatedEffect def source . EnemyEntity . EnemyWithId
        case targets of
          [] -> throwM NoChoices
          [x] -> pure [f x]
          xs -> pure [Ask ident $ ChooseOne [TargetLabel (enemyRef x) [Run [f x]] | x <- xs]]
      EventEntity eid -> pure [CreatedEffect def source (EventEntity eid)]
      AttachmentEntity matcher -> do
        targets <- selectList matcher
        let f = CreatedEffect def source . AttachmentEntity . AttachmentWithId
        case targets of
          [] -> throwM NoChoices
          [x] -> pure [f x]
          xs ->
            pure
              [ Ask ident $
                  ChooseOne
                    [TargetLabel (toRef x) [Run [f x]] | x <- xs]
              ]
  UseAbility a -> do
    activeCostId <- getRandom
    cost <- getAbilityCost a
    occurrence <- getCurrentWindows
    triggering <- case abilityWindow a of
      Nothing -> pure occurrence
      Just matcher -> filterM (\window -> windowMatches matcher window (abilitySource a)) occurrence
    pure
      [ SetActiveCost $
          ActiveCost
            activeCostId
            ident
            (ForAbility a)
            cost
            NoPayment
            Nothing
            mempty
            (Just $ map windowType triggering)
      ]
  -- UseAbility a -> do
  --   rest <- concatMapM (choiceMessages ident) (abilityChoices a)
  --   costs <- costMessages ident a
  --   pure $ UsedAbility ident a : costs <> rest
  RunAbility target n -> do
    windows <- getCurrentWindows
    payment <- getCurrentPayment
    pure [RanAbility ident target n (map windowType windows) payment, ClearRemoved]
  ChangeForm -> do
    alterEgo <- identityMatches AlterEgoIdentity ident
    modifiers <- getModifiers ident
    let additional = mconcat [c | AdditionalChangeToHeroFormCost c <- modifiers, alterEgo]
    pure $ if additional == NoCost then [IdentityMessage ident ChooseOtherForm]
      else [CardSpecific (IdentityRef ident) "choosePaidHeroForm" Null]
  ChangeToForm x -> pure [IdentityMessage ident $ ChangedToForm x]
  PlayCard x mWindow -> pure [CardSpecific (IdentityRef ident) "playFromHand" (toJSON (x, mWindow))]
  PayWithCard c -> pure [IdentityMessage ident $ PaidWithCard c]
  FinishPayment -> do
    mcost <- getActiveCost
    case mcost of
      Just activeCostId -> pure [FinishedPayment activeCostId]
      Nothing -> error "no active cost"
  DeclareWild n r -> pure [DeclaredWild n r]
  ReadyIdentity -> pure [IdentityMessage ident ReadiedIdentity]
  Pay payment -> pure [Paid payment]
  DamageAllEnemies matcher source damage -> do
    enemies <- selectList $ DamageableEnemy <> matcher
    simultaneousDamage <$> traverse (\e -> choiceMessages ident (DamageEnemy (toRef e) source damage)) enemies
  DamageAllCharacters matcher source damage -> do
    characters <- selectList $ DamageableCharacter <> matcher
    simultaneousDamage <$> traverse (\c -> choiceMessages ident (DamageCharacter c source damage)) characters
  -- an attack, by the hero's basic power or an (attack) card, attacks each enemy it
  -- damages: the window after the hero attacks it opens for each
  DamageEnemy target source damage' -> do
    replacement <- case damageSource damage' of
      FromPlayerAttack who | source /= IdentityRef who -> do
        labels <- sourceAbilitySubTypes source
        hero <- selectAny (IdentityWithId who <> HeroIdentity)
        modifiers <- getModifiers (IdentityRef who)
        pure $ if hero && Ability.Attack `notElem` labels && any (\case ReplaceHeroAttackWithDiscardAndStun _ -> True; _ -> False) modifiers then Just who else Nothing
      _ -> pure Nothing
    case replacement of
      Just who -> do
        targets <- (\ms -> [t | ReplaceHeroAttackWithDiscardAndStun t <- ms]) <$> getModifiers (IdentityRef who)
        let choices = [TargetLabel target [Run [RemoveFromPlay target, IdentityMessage who IdentityStunned]] | target <- targets]
        pure $ case choices of
          [TargetLabel _ [Run messages]] -> messages
          [] -> []
          _ -> [Ask who $ ChooseOne choices]
      Nothing -> do
        attackKeywords <- case damageSource damage' of
          FromPlayerAttack attacker -> if source == IdentityRef attacker then getModifiers source else (<>) <$> getModifiers (IdentityRef attacker) <*> getModifiers source
          FromAllyAttack attacker -> getModifiers (AllyRef attacker)
          _ -> pure []
        -- each instance of damage the source deals may be increased, as by Embiggen!
        -- (excess damage carried on by overkill has had it already)
        bonus <-
          if damageSource damage' == FromOverkill
            then pure 0
            else sourceBonus source \case
              DamageDealtModifier n -> Just n
              _ -> Nothing
        -- "reduce the damage attached enemy takes from each attack" (Vibration Resistance)
        let
          enemyAsRef = case target of
            EnemyRef (EnemyVillainId vid) -> VillainRef vid
            EnemyRef (EnemyMinionId mid) -> MinionRef mid
            t -> t
          attackReduction = case enemyAsRef of
            VillainRef _ -> pure 0 -- applied after tough in Villain.reducedDamage
            MinionRef _ -> pure 0 -- applied after attack interrupts in Minion.Runner
            _ -> sum . mapMaybe (\case AttackDamageReduction n -> Just n; _ -> Nothing) <$> getModifiers enemyAsRef
          -- "cannot take more than n damage from a single attack" (Cutthroat Ambition)
          attackCap = case enemyAsRef of
            VillainRef _ -> pure Nothing -- reduced and capped together after tough
            MinionRef _ -> pure Nothing
            _ -> (\ms -> case [n | AttackDamageCap n <- ms] of [] -> Nothing; ns -> Just (minimum ns)) <$> getModifiers enemyAsRef
          fromAttack = case damageSource damage' of
            FromPlayerAttack _ -> True
            FromAllyAttack _ -> True
            _ -> False
        -- (Thanos's Armor's "reduce any amount of damage by 1" is the villain's, after his
        -- tough status card: 'Marvel.Villain.Types')
        targetModifiers <- getModifiers enemyAsRef
        sourceDef <- getsGame (\g -> abilityCardDef g $ refocus source)
        let taken = case enemyAsRef of
              MinionRef _ -> 0 -- final target modifiers follow attack interrupts
              _ -> sum [n | DamageTakenModifier n <- targetModifiers]
                + sum [n | DamageTakenFromPrintedResource resource n <- targetModifiers, maybe False (elem resource . printedResources) sourceDef]
                + if fromAttack then sum [n | AttackDamageTakenModifier n <- targetModifiers] else 0
            targetKeywords = [k | AttacksAgainstGainKeyword k <- targetModifiers, fromAttack]
            attackHas keyword = KeywordModifier keyword `elem` attackKeywords || keyword `elem` targetKeywords
        reduction <- if fromAttack then attackReduction else pure 0
        cap <- if fromAttack then attackCap else pure Nothing
        let
          incoming = damageAmount damage' + bonus + sum [n | AttackDamageToEnemyModifier e n <- attackKeywords, e == enemy, damageAmount damage' > 0]
          increased = if incoming == 0 then 0 else fromIntegral (max 0 (fromIntegral incoming + taken))
          reduced = subtractNatural reduction increased
          damage = damage'
            { damageAmount = maybe reduced (min reduced) cap
            , damagePiercing = damagePiercing damage' || attackHas Piercing
            , damageOverkill = damageOverkill damage' || attackHas Overkill
            , damageRanged = damageRanged damage' || attackHas Ranged
            }
          enemy = case target of
            VillainRef vid -> EnemyVillainId vid
            MinionRef mid -> EnemyMinionId mid
            EnemyRef e -> e
            _ -> error "can not damage target"
          attacked = case damageSource damage of
            -- an event's attack is answered by the event, after all of it if it is labeled one
            FromPlayerAttack attacker
              | EventRef eid <- source -> [EventMessage eid (EventAttacked attacker enemy (damageRanged damage))]
              | otherwise ->
                  [enemyRetaliates enemy (IdentityCharacter attacker) | not (damageRanged damage)]
                    <> [CheckWindows [Window After $ W.IdentityAttack attacker enemy]]
                    <> [CheckWindows [Window After $ W.PlayerAttackResolved source attacker [enemy]] | case source of IdentityRef _ -> False; _ -> True]
            _ -> []
        -- the window as he is dealt it: damage he cannot take (Loki while a side scheme is in
        -- play, the rest of an attack Master of Illusions prevents) is none he would take, so
        -- "would take damage" does not answer it, though "when he is attacked" does
        ignored <- case (damageSource damage, enemy) of
          (FromPlayerAttack who, EnemyVillainId _) -> do
            mods <- getModifiers (IdentityRef who)
            if IgnoresGuard `elem` mods then do
              minions <- selectList (MinionWithKeyword Guard <> MinionEngagedWith (IdentityWithId who))
              pure [CheckWindows [Window After $ W.IgnoredKeyword who mid | mid <- minions] | notNull minions]
            else pure []
          _ -> pure []
        begun <- sourceHasBegunLabeledAttack source
        targeted <- sourceHasAttackedTarget source enemy
        let attackingWindow = case damageSource damage of
              FromPlayerAttack who ->
                [CheckWindows windows | let windows = [Window When $ W.IdentityAttack who enemy | not begun] <> [Window When $ W.IdentityAttackTarget who enemy | not targeted], notNull windows]
              _ -> []
        let rangedWindow = case damageSource damage of
              FromPlayerAttack who | damageRanged damage -> [CheckWindows [Window When $ W.MakingRangedAttack who source enemy]]
              _ -> []
        let keywordWindow = case damageSource damage of
              FromPlayerAttack who ->
                [CheckWindows [Window When $ W.MakingKeywordAttack who source enemy
                  ([Overkill | damageOverkill damage] <> [Piercing | damagePiercing damage] <> [Ranged | damageRanged damage])
                  (not (damageOverkill damage' || damagePiercing damage' || damageRanged damage' || any (\k -> KeywordModifier k `elem` attackKeywords) [Overkill, Piercing, Ranged]))]]
              _ -> []
        let damageMetadata = case damageSource damage of
              FromPlayerAttack who -> [CardSpecific GameRef "playerAttackDamageBeforeTarget" (toJSON (who, source, enemy, damage { damageAmount = damageAmount damage' + bonus
                , damageOverkill = damageOverkill damage' || KeywordModifier Overkill `elem` attackKeywords
                , damagePiercing = damagePiercing damage' || KeywordModifier Piercing `elem` attackKeywords
                , damageRanged = damageRanged damage' || KeywordModifier Ranged `elem` attackKeywords }))]
              _ -> []
        pure $ ignored <> attackingWindow <> rangedWindow <> keywordWindow <> damageMetadata <>
          ( case enemy of
              EnemyVillainId vid ->
                [VillainMessage vid $ VillainDamaged source damage]
              EnemyMinionId mid -> [MinionMessage mid $ MinionDamaged source damage]
          )
            <> attacked
  -- threat removed is a thwart (RR "Thwart") only by a basic thwart or an ability labeled
  -- a thwart, as "Hero Action (thwart)"; "remove threat" otherwise is not one
  ThwartScheme target source n' -> do
    labeled <- elem Ability.Thwart <$> sourceAbilitySubTypes source
    removeThreatFrom False labeled target source n'
  BasicThwartScheme target source n' -> removeThreatFrom True True target source n'
  RemoveThreat source n schemeMatcher -> do
    labeled <- elem Ability.Thwart <$> sourceAbilitySubTypes source
    candidates <- selectList (schemeMatcher <> if labeled then SchemeWithoutThwartPrevention else AnyScheme)
    eligible <- filterM (canRemoveThreatFrom source) candidates
    controller <- getSourceController source
    forbidden <- maybe (pure False) (fmap (elem CannotThwartSideSchemes) . getModifiers . IdentityRef) controller
    let schemes = if labeled && forbidden then filter (\case SchemeSideSchemeId _ -> False; _ -> True) eligible else eligible
        f target = ThwartScheme target source n
    pure [Ask ident $ ChooseOne [TargetLabel target [f target] | x <- schemes, let target = schemeRef x]]
  PlaceThreat _source n schemeMatcher -> do
    schemes <- selectList schemeMatcher
    pure $ concatMap (`placeThreatOn` n) schemes
  ChooseHeal n characterMatcher -> do
    characters <- selectList characterMatcher
    let f character = Heal character n
    pure [Ask ident $ ChooseOne [TargetLabel (characterRef x) [f x] | x <- characters]]
  ChooseDamage source damage enemyMatcher -> do
    enemies <- filterM (canSourceDamageEnemy source) =<< selectList (enemyMatcher <> DamageableEnemy)
    let f target = DamageEnemy target source damage
    pure [Ask ident $ ChooseOne [TargetLabel target [f target] | x <- enemies, let target = enemyRef x], ClearRemoved]
  Stun target source -> case target of
    VillainRef vid -> pure [VillainMessage vid $ VillainStunned source]
    MinionRef mid -> pure [MinionMessage mid $ MinionStunned source]
    EnemyRef eid -> case eid of
      EnemyVillainId vid -> pure [VillainMessage vid $ VillainStunned source]
      EnemyMinionId mid -> pure [MinionMessage mid $ MinionStunned source]
    _ -> error "can not stun target"
  Confuse target source -> case target of
    VillainRef vid -> pure [VillainMessage vid $ VillainConfused source]
    MinionRef vid -> pure [MinionMessage vid $ MinionConfused source]
    EnemyRef eid -> case eid of
      EnemyVillainId vid -> pure [VillainMessage vid $ VillainConfused source]
      EnemyMinionId mid -> pure [MinionMessage mid $ MinionConfused source]
    _ -> error "can not confuse target"
  Recover -> do
    forbidden <- elem CannotRecover <$> getModifiers (IdentityRef ident)
    pure $ if forbidden then [] else [CheckWindows [Window When $ W.UsedRecovery ident], IdentityMessage ident $ SideMessage (RecoveredWithBonus 0), CheckWindows [Window After $ W.UsedRecovery ident]]
  Heal characterId n -> do
    forbidden <- elem CannotBeHealedByPlayerCards <$> getModifiers (characterRef characterId)
    pure $ if forbidden then [] else case characterId of
      IdentityCharacter ident' -> [IdentityMessage ident' $ IdentityHealed n]
      AllyCharacter ident' -> [AllyMessage ident' $ AllyHealed n]
      VillainCharacter ident' -> [VillainMessage ident' $ VillainHealed n]
      MinionCharacter ident' -> [MinionMessage ident' $ MinionHealed n]
  DamageCharacter characterId source damage -> do
    modifiers <- getModifiers (characterRef characterId)
    let isAttack = case damageSource damage of
          FromAttack -> True
          FromPlayerAttack _ -> True
          FromAllyAttack _ -> True
          _ -> False
        reduction = if isAttack then sum [n | AttackDamageReduction n <- modifiers] else 0
        reduced = damage {damageAmount = subtractNatural reduction (damageAmount damage)}
    case characterId of
      IdentityCharacter ident' -> do
        capped <- capIdentityAttackDamage ident' source reduced
        pure $ damageIdentity ident' source capped
      AllyCharacter ident' -> pure $ damageAlly ident' source reduced
      -- enemy reductions are applied by their own damage pipeline
      VillainCharacter ident' -> choiceMessages ident (DamageEnemy (VillainRef ident') source damage)
      MinionCharacter ident' -> choiceMessages ident (DamageEnemy (MinionRef ident') source damage)
  Attack -> pure [IdentityMessage ident $ SideMessage Attacked]
  Thwart -> pure [IdentityMessage ident $ SideMessage Thwarted]
  Defend enemyId ->
    pure [IdentityMessage ident $ SideMessage $ Defended enemyId]
  AllyAttack allyId -> pure [AllyMessage allyId AllyAttacked]
  AllyThwart allyId -> pure [AllyMessage allyId AllyThwarted]
  AllyDefend allyId enemyId -> pure [AllyMessage allyId $ AllyDefended enemyId]
  DiscardTarget target -> pure [RemoveFromPlay target]
  DiscardCard card -> pure [DiscardedCard card]
  ChooseEnemy matcher target -> do
    enemies <- selectList matcher
    pure
      [ Ask ident $
          ChooseOne
            [ TargetLabel (toRef e) [Run [ChoseEnemy e target]]
            | e <- enemies
            ]
      ]
  ChooseUpgrade matcher target -> do
    upgrades <- selectList matcher
    pure
      [ Ask ident $
          ChooseOne
            [ TargetLabel (toRef u) [Run [ChoseUpgrade u target]]
            | u <- upgrades
            ]
      ]
  ChoosePlayer matcher target -> do
    players <- selectList matcher
    pure
      [ Ask ident $
          ChooseOne
            [ TargetLabel (toRef p) [Run [ChosePlayer p target]]
            | p <- players
            ]
      ]
  ChooseDrawCards n identityMatcher -> do
    identities <- selectList identityMatcher
    let f iid = Run [IdentityMessage iid (DrawCards FromDeck n)]
    pure [Ask ident $ ChooseOne [TargetLabel (toRef x) [f x] | x <- identities]]
  ReturnTargetToHand target -> pure [ReturnToHand target]
  ChooseOneLabelChoice choicePairs ->
    pure [Ask ident $ ChooseOne $ map (\(t, c) -> Label t [c]) choicePairs]

-- | Open a labeled thwart's interrupt before calculating the amount removed.
-- Basic thwarts already open their window before choosing a scheme (and before
-- calculating a divided thwart's total), so they do not open another one here.
removeThreatFrom :: (HasQueue m, HasGame m, MonadThrow m, MonadRandom m) => Bool -> Bool -> Target -> Source -> Natural -> m [Message]
removeThreatFrom basic thwarting target source n = do
  controller <- getHeroSourceController source
  begun <- sourceHasBegunLabeledThwart source
  payer <- fromMaybe <$> getYou <*> getSourceController source
  let opensWindow = thwarting && not basic && not begun
      started = [CardSpecific GameRef "markLabeledThwart" (toJSON source) | opensWindow]
        <> [EventMessage eid EventThwarted | opensWindow, EventRef eid <- [source]]
      windows = [CheckWindows [Window When $ W.IdentityThwart who source] | opensWindow, Just who <- [controller]]
      continuation = started <> windows <> [ResolveThreatRemoval basic thwarting payer target source n]
      canonical = case target of SchemeRef sid -> schemeRef sid; other -> other
  modifiers <- getModifiers canonical
  let cost = if thwarting then mconcat [c | AdditionalThwartCost c <- modifiers] else NoCost
  if cost == NoCost then pure continuation else do
    payable <- canAffordCost payer (refocus target) cost
    acid <- getRandom
    pure [SetActiveCost $ ActiveCost acid payer (ForContinuation (refocus target) continuation) cost NoPayment Nothing mempty Nothing | payable]

-- | Resolve the removal after its interrupt window and additional cost. Reading
-- modifiers here makes an interrupt apply to the first instance as well as later
-- instances of the same event.
resolveThreatRemoval :: (HasGame m) => Bool -> Bool -> Target -> Source -> Natural -> m [Message]
resolveThreatRemoval basic thwarting target source n' = do
  modifiers <- getModifiers source
  let bonus = if ThreatRemovalBonusInTotal `elem` modifiers then 0 else sum [n | ThreatRemovedModifier n <- modifiers]
      n = n' + bonus
      side sid = SideSchemeMessage sid $ if thwarting then SideSchemeThwarted source n else SideSchemeThreatRemoved source n
      main mid = MainSchemeMessage mid $ if basic then MainSchemeBasicThwarted source n else if thwarting then MainSchemeThwarted source n else MainSchemeThreatRemoved source n
  heroController <- getHeroSourceController source
  let completed = if thwarting && not basic then case source of
        EventRef eid -> [EventMessage eid EventThwarted]
        IdentityRef ident -> [CheckWindows [Window After $ W.HeroThwartComplete ident]]
        UpgradeRef _ -> [CheckWindows [Window After $ W.HeroThwartComplete ident] | Just ident <- [heroController]]
        _ -> []
        else []
      continuation = (<> completed) $ case target of
        MainSchemeRef mid -> [main mid]
        SideSchemeRef sid -> [side sid]
        SchemeRef (SchemeMainSchemeId mid) -> [main mid]
        SchemeRef (SchemeSideSchemeId sid) -> [side sid]
        _ -> error $ "can not thwart target: " <> show target
  ignored <- case (heroController, target) of
    (Just who, t) | thwarting && (case t of MainSchemeRef _ -> True; SchemeRef (SchemeMainSchemeId _) -> True; _ -> False) -> do
      mods <- getModifiers (IdentityRef who)
      patrols <- if IgnoresPatrol `elem` mods then selectList (MinionWithKeyword Patrol <> MinionEngagedWith (IdentityWithId who)) else pure []
      crises <- if IgnoresCrisis `elem` mods then selectList CrisisSideScheme else pure []
      pure [CheckWindows (map (Window After . W.IgnoredKeyword who) patrols <> map (Window After . W.IgnoredCrisis who) crises) | notNull patrols || notNull crises]
    _ -> pure []
  allowed <- case target of
    MainSchemeRef sid -> canRemoveThreatFrom source (SchemeMainSchemeId sid)
    SideSchemeRef sid -> canRemoveThreatFrom source (SchemeSideSchemeId sid)
    SchemeRef sid -> canRemoveThreatFrom source sid
    _ -> pure False
  pure [message | allowed, message <- ignored <> continuation]

sourceBonus :: (HasGame m) => Source -> (Modifier -> Maybe Natural) -> m Natural
sourceBonus source f = sum . mapMaybe f <$> getModifiers source
