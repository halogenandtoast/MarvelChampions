{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Ally.Runner
  ( module X
  ) where

import Marvel.Prelude

import Marvel.Ally.Types as X hiding (Field(..))
import Marvel.Message.Lifted as X
import Marvel.Question as X

import Data.HashSet qualified as HashSet
import Marvel.Game.Source (getModifiers, isBlanked, cannotTakeDamageFrom, abilityCardDef, getsGame, schemeHasKeyword)
import Marvel.Ability
import Marvel.Attack
import Marvel.Cost
import Marvel.Criteria
import Marvel.Card (Card (..))
import Marvel.Card.PlayerCard.Types (encounterCardInPlayerDeck)
import Marvel.Card.Def
import Marvel.Damage
import Marvel.Keyword
import Marvel.Id
import Marvel.Matchers hiding (ExhaustedAlly)
import Marvel.Query
import Marvel.Window qualified as W

instance HasAbilities Ally where
  getAbilities x@(Ally a) = getAbilities a <> basicAbilities
   where
    basicAbilities =
      [ ability
          x
          300
          Basic
          -- "you cannot thwart" (Baron Zemo) keeps the identity from thwarting, not
          -- its allies
          (if allyConfused (toAllyAttrs a) && allyHasThwart a then NoCriteria else SchemeExists (SchemeAnAllyCanThwart (toId x) False))
          ExhaustCost
          (AllyThwart $ toId x)
      ]
      <> [ ability
             x
             301
             Basic
             -- a stunned ally can attempt an attack with no target ("Stunned")
             (if allyStunned (toAllyAttrs a) then NoCriteria else EnemyExists (AttackableByAlly $ toId x))
             (ExhaustCost <> allyAdditionalAttackCost (toAllyAttrs a))
             (AllyAttack $ toId x)
         | allyHasAttack a
         ]

statusL :: StatusCard -> Lens' (Attrs Ally) Bool
statusL = \case
  StunnedStatus -> stunnedL
  ConfusedStatus -> confusedL
  ToughStatus -> toughL

instance RunMessage (Attrs Ally) where
  runMessage msg a = runQueueT $ case msg of
    CardSpecific (isTarget a -> True) "defeatedByConsequential" _ -> do
      present <- selectAny (AllyWithId $ allyId a)
      when present $ allyStep (allyId a) AllyDefeatedByDamage
      pure a {allyDefeatedByConsequential = present}
    RemovePlayerCardFromHeldZones cid -> pure a {allyHeldCards = filter ((/= cid) . pcCardId) (allyHeldCards a)}
    ReserveAllyHeldCard ident card | ident == allyId a ->
      pure $ a & heldCardsL %~ filter (/= card)
    AttachPlayableEventToAlly ident card | ident == allyId a ->
      pure $ a & heldCardsL <>~ [card]
    CardSpecific (isTarget a -> True) "controlledMinionSource" (fromJSON -> Success (source, n)) ->
      pure a {allyControlSource = Just source, allyAttackConsequentialDamage = n, allyThwartConsequentialDamage = n}
    CardSpecific (isTarget a -> True) "controlledMinionConsequential" (fromJSON -> Success n) ->
      pure a {allyAttackConsequentialDamage = n, allyThwartConsequentialDamage = n}
    PutCardIntoPlay _ card _ _ _ -> pure $ a & heldCardsL %~ filter (/= card)
    AllyMessage ident msg' | ident == allyId a -> case msg' of
      -- "attached character cannot ready" (Restrained)
      -- "allies cannot be readied by player card effects" (Unnatural Storm)
      ReadiedAlly -> do
        modifiers <- getModifiers a
        let readies = CannotReady `notElem` modifiers && CannotBeReadiedByPlayerCards `notElem` modifiers
        pure $ a & exhaustedL %~ (&& not readies)
      ReadiedAllyByGame -> do
        readies <- notElem CannotReady <$> getModifiers a
        pure $ a & exhaustedL %~ (&& not readies)
      ExhaustedAlly -> do
        pure $ a & exhaustedL .~ True
      AllyStunned -> do
        pure $ a & stunnedL .~ True
      AllyAttacked -> if allyStunned a
        then pure $ a {allyAdditionalStun = False} & stunnedL .~ False
        else do
          enemies <- selectList $ AttackableByAlly $ allyId a
          -- "[star] its basic attack gains piercing (ranged)": the keyword on the ally, or
          -- one it gains ("its attacks gain overkill")
          modifiers <- getModifiers a
          let keywords = toList (cdKeywords $ allyCardDef a) <> [k | KeywordModifier k <- modifiers]
              gains =
                [withPiercing | Piercing `elem` keywords]
                  <> [withRanged | Ranged `elem` keywords]
                  <> [withOverkill | Overkill `elem` keywords]
          dmg <- foldr ($) <$> (toDamage <$> getModifiedAttack a <*> pure (FromAllyAttack $ allyId a)) <*> pure gains
          if DividesBasicAttackEvenly `elem` modifiers
            then unless (null enemies) do
              let count = fromIntegral $ length enemies
                  (base, extra) = damageAmount dmg `divMod` count
              allyStep ident (AllyAssignEvenAttack dmg extra [(enemy, base) | enemy <- enemies] enemies [])
            else chooseOne (allyController a)
              [ targeting (enemyRef enemy) do
                  checkWindows [W.Window W.When $ W.AllyAttack (allyId a) enemy]
                  allyAttackLands (allyId a) enemy dmg
              | enemy <- enemies
              ]
          pure a
      -- an interrupt that defeats the enemy aborts the attack: no damage and no
      -- consequential damage (RR, Consequential Damage)
      AllyAttackLands enemy dmg -> do
        stillInPlay <- selectAny (EnemyWithId enemy)
        when stillInPlay do
          targetModifiers <- getModifiers (enemyRef enemy)
          modifiers <- getModifiers a
          let against = sum [n | AttackModifierAgainst target n <- modifiers, target == enemy]
          amount <- if AttackWithThwart `elem` (targetModifiers <> modifiers) then getModifiedThwart a else pure (damageAmount dmg + against)
          dealDamage enemy a dmg {damageAmount = amount}
          unless (damageRanged dmg) $ enemyRetaliates enemy (AllyCharacter $ allyId a)
          checkWindows [W.Window W.After $ W.AllyAttack (allyId a) enemy]
          checkWindows [W.Window W.After $ W.PlayerAttackResolved (toSource a) (allyController a) [enemy]]
          allyStep ident (AllyAttackConsequentialAgainst [enemy])
        pure a
      AllyAssignEvenAttack dmg extra allocations candidates seen -> do
        if extra == 0
          then do
            let attacked = [(enemy, n) | (enemy, n) <- allocations, n > 0]
            checkWindows [W.Window W.When $ W.AllyAttack ident enemy | (enemy, _) <- attacked, enemy `notElem` seen]
            allyStep ident (AllyEvenAttackLands dmg allocations)
          else chooseOne (allyController a)
            [ targeting (enemyRef enemy) $ allyStep ident
                (AllyAssignEvenAttack dmg (extra - 1)
                  [(target, n + if target == enemy then 1 else 0) | (target, n) <- allocations]
                  (filter (/= enemy) candidates) seen)
            | enemy <- candidates
            ]
        pure a
      AllyEvenAttackLands dmg allocations -> do
        -- Attack interrupts can change the total ATK after the initial allocation.
        -- Re-divide that total, retaining zero-share enemies as possible recipients.
        if damageAmount dmg /= sum (map snd allocations) && notNull allocations
          then do
            let enemies = map fst allocations
                (base, extra) = damageAmount dmg `divMod` fromIntegral (length enemies)
                seen = [enemy | (enemy, n) <- allocations, n > 0]
            allyStep ident (AllyAssignEvenAttack dmg extra [(enemy, base) | enemy <- enemies] enemies seen)
          else do
            alive <- filterM (selectAny . EnemyWithId . fst) [(enemy, n) | (enemy, n) <- allocations, n > 0]
            for_ alive $ \(enemy, n) -> dealDamage enemy a dmg {damageAmount = n}
            allyStep ident (AllyEvenAttackFinished dmg $ map fst alive)
        pure a
      AllyEvenAttackFinished dmg enemies -> do
        present <- selectAny (AllyWithId ident)
        when (present && not (null enemies)) do
          unless (damageRanged dmg) $ for_ enemies $ \enemy -> enemyRetaliates enemy (AllyCharacter ident)
          checkWindows [W.Window W.After $ W.AllyAttackCompletedAgainst ident enemies]
          checkWindows [W.Window W.After $ W.PlayerAttackResolved (toSource a) (allyController a) enemies]
          allyStep ident (AllyAttackConsequentialAgainst enemies)
        pure a
      AllyAttackMultiple enemies damage -> do
        modifiers <- getModifiers a
        amount <- if AttackWithThwart `elem` modifiers then getModifiedThwart a else pure (damageAmount damage)
        allyStep ident (AllyAttackMultipleNext enemies [] damage {damageAmount = amount})
        pure a
      AllyAttackMultipleNext remaining resolved damage -> do
        present <- selectAny (AllyWithId ident)
        when present do
          enemies <- filterM (selectAny . EnemyWithId) remaining
          if null enemies
            then allyStep ident (AllyAttackMultipleRetaliation resolved (damageRanged damage))
            else chooseOne (allyController a)
              [ targeting (enemyRef enemy) do
                  dealDamage enemy a damage
                  allyStep ident (AllyAttackMultipleNext (filter (/= enemy) enemies) (resolved <> [enemy]) damage)
              | enemy <- enemies
              ]
        pure a
      AllyAttackMultipleRetaliation resolved ranged -> do
        alive <- filterM (selectAny . EnemyWithId) resolved
        unless ranged $ for_ alive $ \enemy -> enemyRetaliates enemy (AllyCharacter ident)
        allyStep ident (AllyAttackMultipleFinished resolved)
        pure a
      AllyAttackMultipleFinished resolved -> do
        present <- selectAny (AllyWithId ident)
        when (present && not (null resolved)) do
          checkWindows [W.Window W.After $ W.AllyAttackCompletedAgainst ident resolved]
          checkWindows [W.Window W.After $ W.PlayerAttackResolved (toSource a) (allyController a) resolved]
          -- This is one use, hence consequential damage is suffered only once.
          allyStep ident (AllyAttackConsequentialAgainst resolved)
        pure a
      AllyAttackConsequentialAgainst enemies -> do
        present <- selectAny (AllyWithId ident)
        when present $ do
          modifiers <- getModifiers a
          let reduction = maximum (0 : [n | AttackConsequentialDamageReductionAgainst enemy n <- modifiers, enemy `elem` enemies])
          consequential <- subtractNatural reduction <$> getModifiedAttackConsequentialDamage a
          when (consequential > 0) $ damageCharacter ident a (toDamage consequential FromConsequential)
        pure a
      AllyAttackConsequential -> do
        present <- selectAny (AllyWithId ident)
        when present $ do
          consequential <- getModifiedAttackConsequentialDamage a
          when (consequential > 0) $ damageCharacter ident a (toDamage consequential FromConsequential)
        pure a
      AllyThwarted -> if allyConfused a
        then pure $ a {allyAdditionalConfuse = False} & confusedL .~ False
        else do
          -- "ignore any crisis icons in play" as it thwarts (Fandral)
          ignoresCrisis <- elem BasicThwartIgnoresCrisis <$> getModifiers a
          schemes <- selectList (SchemeAnAllyCanThwart (allyId a) ignoresCrisis)
          thw <- getModifiedThwart a
          atk <- getModifiedAttack a
          -- a scheme thwarted with ATK instead of THW, as The Red House allows
          choices <- for schemes \scheme -> do
            withAttack <- elem ThwartWithAttack <$> getModifiers (schemeRef scheme)
            assault <- schemeHasKeyword scheme Assault
            let thwartWith n = do
                  checkWindows [W.Window W.When $ W.AllyThwart (allyId a) scheme]
                  basicThwartOf scheme a n
                  checkWindows [W.Window W.After $ W.AllyThwart (allyId a) scheme]
                  allyStep ident (if assault then AllyAttackConsequential else AllyThwartConsequentialOn scheme)
            pure $ targeting (schemeRef scheme) $
              if assault then thwartWith atk else if withAttack && atk /= thw
                then
                  chooseOne (allyController a)
                    [labeled "$label.thwartWithThw" (thwartWith thw), labeled "$label.thwartWithAtk" (thwartWith atk)]
                else thwartWith thw
          chooseOne (allyController a) choices
          pure a
      AllyThwartConsequentialOn scheme -> do
        consequential <- getModifiedThwartConsequentialDamage a
        modifiers <- getModifiers a
        let reduction = case scheme of
              SchemeSideSchemeId _ -> sum [n | SideSchemeThwartConsequentialReduction n <- modifiers]
              _ -> 0
            n = subtractNatural reduction consequential
        when (n > 0) $ damageCharacter ident a (toDamage n FromConsequential)
        pure a
      AllyThwartConsequential -> do
        consequential <- getModifiedThwartConsequentialDamage a
        when (consequential > 0) $ damageCharacter ident a (toDamage consequential FromConsequential)
        pure a
      AllyDefended enemyId -> do
        forbidden <- elem AttacksCannotBeDefended <$> getModifiers (enemyRef enemyId)
        unless forbidden do
          exhaust ident
          enemyDefendedBy enemyId (AllyCharacter ident)
          checkWindows [W.Window W.When $ W.AllyDefendedAgainst ident enemyId]
        pure a
      AllyHealed n -> pure $ a & damageL %~ subtractNatural n
      AllyWasAttacked attack' -> do
        hitPoints <- getModifiedHitPoints a
        modifiers <- getModifiers a
        let
          reduced = subtractNatural (sum [n | AttackDamageReduction n <- modifiers]) (attackDamageDealt attack')
          overkill = subtractNatural
            (subtractNatural (allyDamage a) hitPoints)
            reduced
        -- an attack dealing indirect damage (Starshark) has it divided among the
        -- characters the ally's controller controls, overkill having none past it
        when (attackDamageDealt attack' > 0) $
          case attackThreatRemoval attack' of
            Just you -> removeThreat you (attackSource attack') reduced MainScheme
            Nothing -> if attackDividedEvenly attack'
              then divideAttackDamage (allyController a) (attackSource attack') ((attackDamageTo attack' (attackDamageDealt attack')) {damageOverkill = attackOverkill attack'})
              else if attackIndirect attack'
              then dealIndirectAttackDamage (allyController a) (attackSource attack') (attackDamageTo attack' (attackDamageDealt attack'))
              else damageCharacter ident (attackSource attack') (attackDamageTo attack' (attackDamageDealt attack'))
        -- the excess is the attack's damage to the controller: "when you would take
        -- damage" answers it, as it does the attack's own
        when (attackOverkill attack' && overkill > 0 && not (attackIndirect attack' || attackDividedEvenly attack')) do
          let excess = toDamage overkill FromAttack
          damageCharacter (allyController a) (attackSource attack') excess
        -- retaliate, if it is still in play after the attack, and the attack not ranged
        blanked <- isBlanked a
        let keywords = (if blanked then [] else toList $ cdKeywords $ allyCardDef a) <> [k | KeywordModifier k <- modifiers]
            retaliates = sum [n | Retaliate n <- keywords]
        ignoresRetaliate <- elem AttacksIgnoreRetaliate <$> getModifiers (attackSource attack')
        when (retaliates > 0 && not (attackRanged attack' || ignoresRetaliate)) $
          allyStep ident (AllyRetaliates (attackEnemy attack') retaliates)
        -- "after <ally> defends against an attack": the attack has resolved
        when (attackDefended attack') $
          checkWindows [W.Window W.After $ W.AllyDefendedAgainst ident (attackEnemy attack')]
        pure a
      AllyRetaliates enemy n -> do
        dealDamage enemy a (toDamage n FromRetaliate)
        pure a
      AllyDamaged source damage -> do
        immune <- cannotTakeDamageFrom (AllyRef ident) source
        if immune then pure a
        else if allyTough a && preventedByTough damage
          then pure $ removeOneAllyStatus ToughStatus a
          else do
            hitPoints <- getModifiedHitPoints a
            attackDealtDamageTo source damage ident
            when (damageAmount damage + allyDamage a >= hitPoints) $
              if damageSource damage == FromConsequential
                then cardSpecific a "defeatedByConsequential" ()
                else case sourceEnemy source of
                  Just enemy | damageSource damage == FromAttack -> allyStep ident (AllyDefeatedByAttack enemy)
                  _ -> allyStep ident AllyDefeatedByDamage
            when (damageOverkill damage && damageSource damage == FromAttack) do
              let excess = subtractNatural hitPoints (damageAmount damage + allyDamage a)
              when (excess > 0) $ damageIdentity (allyController a) source (toDamage excess FromOverkill)
            when (damageAmount damage > 0) $ checkWindows [W.Window W.After $ W.AllyTookDamage ident source damage]
            pure $ (if piercesTough damage then a {allyTough = False, allyAdditionalTough = 0} else a) & damageL +~ damageAmount damage
      AllyDefeated -> do
        protected <- elem CannotLeavePlayByCardAbilities <$> getModifiers a
        if protected then pure a else defeated Nothing
      AllyDefeatedByDamage -> defeated Nothing
      AllyDefeatedByAttack enemy -> defeated (Just enemy)
      SpendAllyUse -> pure $ a & countersL -~ 1
      AllyHoldsTreachery card -> pure a {allyHeldTreacheries = allyHeldTreacheries a <> [card]}
      AllyPaidWith payment -> pure a {allyPaidWith = payment}
      AllyOwnedBy owner -> pure a {allyOwner = owner}
      AllyChangedController new -> do
        when (new /= allyController a) do
          identityStep (allyController a) (AllyRemoved $ allyId a)
          for_ (allyUpgrades a) $ \uid -> upgradeStep uid (UpgradeControlledByPlayer new)
          identityStep new (AllyCreated $ allyId a)
        pure a {allyController = new}
      AllySetDamage n -> pure $ a & damageL .~ n
      -- the game replaces it with its other face ('FlipAlly' in "Marvel.Game")
      FlipAlly -> pure a
      AttachedUpgradeToAlly upgradeId ->
        pure $ a & upgradesL %~ HashSet.insert upgradeId
    -- "if an ability that says an ally gets +X hit points ceases to be in effect and
    -- causes that ally to have damage on it equal to or greater than its hit points,
    -- that ally is defeated"
    UpgradeMessage uid UpgradeDetached -> pure $ a & upgradesL %~ HashSet.delete uid
    UpgradeRemoved upgradeId | HashSet.member upgradeId (allyUpgrades a) -> do
      hitPoints <- getModifiedHitPoints a
      when (allyDamage a > 0 && allyDamage a >= hitPoints) $ allyStep (allyId a) AllyDefeatedByDamage
      pure $ a & upgradesL %~ HashSet.delete upgradeId
    -- counters on the ally, the uses its abilities spend (Jack Flag's ammo)
    PlaceCounters (AllyRef ident) n | ident == allyId a -> pure $ a & countersL +~ n
    RemoveCounters (AllyRef ident) n | ident == allyId a -> pure $ a & countersL %~ subtractNatural n
    GiveStatus (AllyRef ident) s | ident == allyId a -> pure $ a & statusL s .~ True
    -- "the first player gains control of <this ally>" (Odin): it follows the first
    -- player token as it passes, or as the first player is chosen or eliminated
    PassFirstPlayer -> followFirstPlayer
    SetPlayerOrder _ -> followFirstPlayer
    RemoveFromPlay (IdentityRef _) -> followFirstPlayer
    CardSpecific (isTarget a -> True) "followFirstPlayer" _ -> do
      selectOne FirstPlayer >>= traverse_ \first -> when (first /= allyController a) do
        allyStep (allyId a) (AllyChangedController first)
      pure a
    RemoveStatus (AllyRef ident) s | ident == allyId a -> pure $ removeOneAllyStatus s a
    _ -> pure a
   where
    -- once the game has seen to the player order
    followFirstPlayer = do
      follows <- elem ControlledByFirstPlayer <$> getModifiers a
      when follows $ cardSpecific a "followFirstPlayer" ()
      pure a
    -- leaving play discards it
    defeated byAttack = do
      let card = MkPlayerCard (CardId $ unAllyId $ allyId a) (allyCardDef a) (Just $ allyOwner a) (Just $ allyController a)
      checkWindows [W.Window W.When $ W.DefeatedAlly (allyId a) byAttack]
      -- Defeat is a game-rule departure, even if a card dealt its damage.
      allyLeavesByDefeat (allyId a) $ any (\case Victory _ -> True; _ -> False) (cdKeywords $ allyCardDef a)
      let physicalCard = case allyOriginalCard a of
            Just (EncounterCard encounter) -> encounterCardInPlayerDeck (allyController a) encounter
            Just (PlayerCard original) -> original
            Nothing -> card
      checkWindows $ W.Window W.After (W.AllyCardDefeated (allyController a) card)
        : [W.Window W.After (W.AllyCardDefeatedByConsequentialDamage (allyController a) physicalCard) | allyDefeatedByConsequential a]
      pure a {allyDefeatedByConsequential = False}

-- | Removing one physical status preserves another held through a type change.
removeOneAllyStatus :: StatusCard -> Attrs Ally -> Attrs Ally
removeOneAllyStatus StunnedStatus a
  | allyAdditionalStun a = a {allyAdditionalStun = False}
  | otherwise = a {allyStunned = False}
removeOneAllyStatus ConfusedStatus a
  | allyAdditionalConfuse a = a {allyAdditionalConfuse = False}
  | otherwise = a {allyConfused = False}
removeOneAllyStatus ToughStatus a
  | allyAdditionalTough a > 0 = a {allyAdditionalTough = allyAdditionalTough a - 1}
  | otherwise = a {allyTough = False}
