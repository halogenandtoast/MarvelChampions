module Marvel.Choice where

import Marvel.Prelude

import Marvel.Ability.Types hiding (Attack, Thwart)
import Marvel.Ability.Types qualified as Ability
import Marvel.Card.Def (CardDef (..))
import Marvel.ActiveCost.Types
import Marvel.Damage
import Marvel.Exception
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers.Types
import Marvel.Message
import Marvel.Modifier.Types
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Window (Window (..), WindowTiming (..))
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
      case targets of
        [] -> throwM NoChoices
        [x] -> pure [f x]
        xs ->
          pure
            [ Ask ident $
                ChooseOne
                  [TargetLabel (toRef x) [Run [f x]] | x <- xs]
            ]
    TargetMatches entityMatcher -> case entityMatcher of
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
    pure
      [ SetActiveCost $
          ActiveCost
            activeCostId
            ident
            (ForAbility a)
            (abilityCost a)
            NoPayment
            Nothing
            mempty
      ]
  -- UseAbility a -> do
  --   rest <- concatMapM (choiceMessages ident) (abilityChoices a)
  --   costs <- costMessages ident a
  --   pure $ UsedAbility ident a : costs <> rest
  RunAbility target n -> do
    windows <- getCurrentWindows
    payment <- getCurrentPayment
    pure [RanAbility ident target n (map windowType windows) payment, ClearRemoved]
  ChangeForm -> pure [IdentityMessage ident ChooseOtherForm]
  ChangeToForm x -> pure [IdentityMessage ident $ ChangedToForm x]
  PlayCard x mWindow -> pure [IdentityMessage ident $ PlayingCard x mWindow]
  PayWithCard c -> pure [IdentityMessage ident $ PaidWithCard c]
  FinishPayment -> do
    mcost <- getActiveCost
    case mcost of
      Just activeCostId -> pure [FinishedPayment activeCostId]
      Nothing -> error "no active cost"
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
    -- each instance of damage the source deals may be increased, as by Embiggen!
    bonus <- sourceBonus source \case
      DamageDealtModifier n -> Just n
      _ -> Nothing
    let
      damage = damage' {damageAmount = damageAmount damage' + bonus}
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
        _ -> []
    pure $
      ( case enemy of
          EnemyVillainId vid ->
            [ CheckWindows [Window When $ W.DamagedVillain vid damage]
            , VillainMessage vid $ VillainDamaged source damage
            ]
          EnemyMinionId mid -> [MinionMessage mid $ MinionDamaged source damage]
      )
        <> attacked
  -- threat removed is a thwart (RR "Thwart") only by a basic thwart or an ability labeled
  -- a thwart, as "Hero Action (thwart)"; "remove threat" otherwise is not one
  ThwartScheme target source n' -> do
    labeled <- getsGame \g -> (cdAbilitySubType =<< abilityCardDef g (toTarget source)) == Just Ability.Thwart
    removeThreatFrom labeled target source n'
  BasicThwartScheme target source n' -> removeThreatFrom True target source n'
  RemoveThreat source n schemeMatcher -> do
    schemes <- selectList schemeMatcher
    let f target = ThwartScheme target source n
    case schemes of
      [] -> pure []
      [x] -> choiceMessages ident (f $ toRef x)
      xs ->
        pure
          [ Ask ident $
              ChooseOne
                [ TargetLabel target [f target]
                | x <- xs
                , let target = toRef x
                ]
          ]
  PlaceThreat _source n schemeMatcher -> do
    schemes <- selectList schemeMatcher
    pure $ concatMap (`placeThreatOn` n) schemes
  ChooseHeal n characterMatcher -> do
    characters <- selectList characterMatcher
    let f character = Heal character n
    case characters of
      [] -> pure []
      [x] -> choiceMessages ident (f x)
      xs ->
        pure
          [ Ask ident $
              ChooseOne [TargetLabel (toRef x) [f x] | x <- xs]
          ]
  ChooseDamage source damage enemyMatcher -> do
    enemies <- selectList (enemyMatcher <> DamageableEnemy)
    let f target = DamageEnemy target source damage
    case enemies of
      [] -> pure []
      [x] -> do
        msgs <- choiceMessages ident (f $ toRef x)
        pure $ msgs <> [ClearRemoved]
      xs ->
        pure
          [ Ask ident $
              ChooseOne
                [ TargetLabel target [f target]
                | x <- xs
                , let target = toRef x
                ]
          , ClearRemoved
          ]
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
  Recover -> pure [IdentityMessage ident $ SideMessage Recovered]
  Heal characterId n -> case characterId of
    IdentityCharacter ident' ->
      pure [IdentityMessage ident' $ IdentityHealed n]
    AllyCharacter ident' -> pure [AllyMessage ident' $ AllyHealed n]
    VillainCharacter ident' -> pure [VillainMessage ident' $ VillainHealed n]
    MinionCharacter ident' -> pure [MinionMessage ident' $ MinionHealed n]
  DamageCharacter characterId source damage -> case characterId of
    IdentityCharacter ident' ->
      pure $ damageIdentity ident' source damage
    AllyCharacter ident' ->
      pure [AllyMessage ident' $ AllyDamaged source damage]
    VillainCharacter ident' ->
      pure [VillainMessage ident' $ VillainDamaged source damage]
    MinionCharacter ident' ->
      pure [MinionMessage ident' $ MinionDamaged source damage]
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
    case identities of
      [] -> pure []
      [x] -> choiceMessages ident (f x)
      xs ->
        pure
          [ Ask ident $
              ChooseOne
                [ TargetLabel target [f x]
                | x <- xs
                , let target = toRef x
                ]
          ]
  ReturnTargetToHand target -> pure [ReturnToHand target]
  ChooseOneLabelChoice choicePairs ->
    pure [Ask ident $ ChooseOne $ map (\(t, c) -> Label t [c]) choicePairs]

-- | What the source's modifiers add to each instance of what it does.
-- | The threat removed from the scheme, by thwarting or not.
removeThreatFrom :: (HasGame m) => Bool -> Target -> Source -> Natural -> m [Message]
removeThreatFrom thwarting target source n' = do
  -- each instance of threat the source removes may be increased, as by Shrink
  bonus <- sourceBonus source \case
    ThreatRemovedModifier n -> Just n
    _ -> Nothing
  let n = n' + bonus
      side sid = SideSchemeMessage sid $ if thwarting then SideSchemeThwarted source n else SideSchemeThreatRemoved source n
  pure case target of
    MainSchemeRef mid -> [MainSchemeMessage mid $ MainSchemeThwarted source n]
    SideSchemeRef sid -> [side sid]
    SchemeRef (SchemeMainSchemeId mid) -> [MainSchemeMessage mid $ MainSchemeThwarted source n]
    SchemeRef (SchemeSideSchemeId sid) -> [side sid]
    _ -> error $ "can not thwart target: " <> show target

sourceBonus :: (HasGame m) => Source -> (Modifier -> Maybe Natural) -> m Natural
sourceBonus source f = sum . mapMaybe f <$> getModifiers source
