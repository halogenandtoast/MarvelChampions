{-# OPTIONS_GHC -Wno-orphans #-}

module Marvel.Game.Runner where

import Control.Monad ( replicateM_, foldM )
import Control.Monad.Reader
import Data.List ( find, nub, nubBy, partition )
import Marvel.Ability
import Marvel.ActiveCost
import Marvel.AgeOfApocalypse.MissionTypes
import Marvel.Ally
import Marvel.Ally.Allies.ControlledMinion ( ControlledMinion(..) )
import Marvel.Ally.Allies.MissionAlly ( MissionAlly(..) )
import Marvel.Ally.Types
    ( Attrs(allyUpgrades, allyOwner), getAllyController )
import Marvel.Area
import Marvel.Aspect ( Aspect(Pool, Neutral) )
import Marvel.Attachment
import Marvel.Attachment.Types ( Attrs(attachmentSideScheme) )
import Marvel.Attack
import Marvel.Boost ( BoostIcon(Star) )
import Marvel.Campaign
    ( CampaignStepKind(CampaignSetupThen, CampaignVictory,
                       CampaignChoices, CampaignSetup) )
import Marvel.CampaignPlayers.Helpers ( echoReservedReplay )
import Marvel.Card
import Marvel.Choice ( choiceMessages, resolveThreatRemoval )
import Marvel.Cost
import Marvel.Count
import Marvel.Damage
import Marvel.Debug
import Marvel.Deck
import Marvel.Difficulty
import Marvel.Discard
import Marvel.Effect.Lasting ( Lasting(..) )
import Marvel.Entity
import Marvel.Event.Types
    ( Attrs(eventController, eventAttacked),
      Event(Event),
      IsEvent(eventAbilitySubTypes) )
import Marvel.Exception
import Marvel.Game.App
import Marvel.Game.Base
import Marvel.Game.Class
import Marvel.Game.Query
import Marvel.Game.Select
import Marvel.Game.Setup
import Marvel.Game.Source ()
import Marvel.GameValue ( GameValue(..), fromGameValue )
import Marvel.Hand
import Marvel.Hero.Cards
import Marvel.Hp
import Marvel.Id
import Marvel.Identity hiding (alliesL, minionsL, supportsL, upgradesL)
import Marvel.Keyword
import Marvel.MainScheme
import Marvel.MainScheme.Types
    ( Attrs(mainSchemeOwner, mainSchemeAccelerationTokens),
      getMainSchemeThreat )
import Marvel.Matchers hiding (ExhaustedIdentity, MainScheme)
import Marvel.Message.Lifted hiding (ExhaustedAlly, createEffect, canMoveSchemeThreatFrom)
import Marvel.Minion
import Marvel.Minion.Types
    ( Attrs(minionEngagedIdentity, minionUpgrades, minionAttachments),
      facedownCardL,
      getMinionEngagedIdentity,
      minionAttackDetails,
      overMinion,
      Minion )
import Marvel.Modifier
import Marvel.Name
import Marvel.Obligation
import Marvel.Obligation.Types ( Attrs(obligationOwner) )
import Marvel.Payment
import Marvel.Phase
import Marvel.Prelude
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Scenario ( recommendedSetup, lookupScenario )
import Marvel.Scenario.Types
    ( Attrs(scenarioSetAsideCards, scenarioDecks, scenarioDifficulty,
            scenarioEncounterDeck, scenarioDiscard, scenarioMission),
      pileL,
      IsScenario(overScenarioAttrs),
      Scenario(Scenario),
      ScenarioSetup(difficulty) )
import Marvel.SideScheme
import Marvel.SideScheme.Types
    ( Attrs(sideSchemeAccelerationTokens) )
import Marvel.Stats ( Sch(unSch, Sch), Thw(Thw, unThw) )
import Marvel.Support
import Marvel.Support.Types
    ( Attrs(supportCardDef, supportOwner, supportHeldCards),
      getSupportController )
import Marvel.Trait hiding (Finished)
import Marvel.Upgrade
import Marvel.Upgrade.Types
    ( Attrs(upgradeExhausted, upgradeController,
            upgradeAttachedMainScheme, upgradeAttachedSideScheme, upgradeOwner,
            upgradeHeldCards, upgradeAttachedAlly, upgradeAttachedEnemy),
      getUpgradeController )
import Marvel.Villain
import Marvel.Villain.Types
    ( villainUpgrades, villainAttackDetails )
import Marvel.Window ( WindowTiming(After, When) )
import System.Random.Shuffle ( shuffleM )
import Marvel.Ability qualified as Ability
import Marvel.Villain.Villains.SmSinisterSix qualified as ActivationSix
import Marvel.Ally.Types qualified as Ally
import Marvel.Message.Ally qualified as AllyMsg
import Marvel.AgeOfApocalypse.Mission qualified as AoaMission
import Marvel.AgeOfApocalypse.Settings qualified as AoaSettings
import Marvel.Hero qualified as AosHero
import Marvel.Minion.Minions.AgentsOfShield qualified as AosMinion
import Marvel.Trait.Types qualified as AosTrait
import Marvel.Attachment.Types qualified as Attachment
import Marvel.CivilWar.Types qualified as CW
import Marvel.Campaign qualified as Campaign
import Data.List qualified as Data.List
import Marvel.Resource.Resources.Domino qualified as Domino
import Marvel.Effect.Types qualified as Effect
import Marvel.EncounterSet qualified as EncounterSet
import Marvel.Hercules.Cards qualified as HCDeck
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Marvel.Hero.Runner qualified as Hero
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Key qualified as Key
import Marvel.EncounterCard qualified as MGEncounter
import Marvel.PlayerCard qualified as MGPlayer
import Marvel.MainScheme.Types qualified as MS
import Marvel.Matchers qualified as Matchers
import Marvel.Minion.Types qualified as Minion
import Marvel.Minion.Runner qualified as MinionRunner
import Marvel.Obligation.Types qualified as Obligation
import Marvel.Campaign.AgentsOfShield qualified as ShieldCampaign
import Marvel.AgentsOfShield.PlayerHelpers qualified as ShieldPlayer
import Marvel.SideScheme.Types qualified as SideScheme
import Marvel.Support.Types qualified as Support
import Data.Text qualified as T
import Marvel.Treachery.Types qualified as Treachery
import Marvel.Upgrade.Types qualified as Upgrade
import Marvel.Villain.Types qualified as Villain
import Marvel.Window qualified as W

runPreGameMessage :: (HasGame m, HasQueue m, MonadThrow m, MonadRandom m) => Message -> Game -> m Game
runPreGameMessage msg g = runQueueT $ case msg of
  CheckWindows windows -> do
    startedBlanking <- attackBlankingStarts windows
    let pending = [(aid,enemy) | W.Window W.When (W.DefeatedAlly aid (Just enemy)) <- windows]
        completed = [(AllyId $ unCardId $ pcCardId card, card) | W.Window W.After (W.AllyCardDefeated _ card) <- windows]
        attacks = gamePendingAttackDefeats g <> pending
        completedIds = map fst completed
    captured <- foldM (\current (aid,card) -> case Data.List.lookup aid attacks of
      Nothing -> pure current
      Just enemy -> do
        let cages = [s | s <- HashMap.elems (gameSideSchemes current), toCardCode (getCardDef s) == "32104"]
        schemes <- for cages \scheme -> do
          updated <- runMessage (CardSpecific (toRef scheme) "mgHoldCard" $ toJSON card) scheme
          pure (toId scheme, updated)
        for_ [(n,magnet) | (e,n,magnet) <- gameBoostAllyDefeatEffects current, e == enemy] \(n,magnet) -> do
          when (n > 0) $ placeThreatOnEach GameRef n Matchers.MainScheme
          when magnet $ selectList EncounterMainScheme >>= traverse_ (\case SchemeMainSchemeId m -> placeCounters m 1; _ -> pure ())
        let unheld = if null cages then id else entitiesL . playersL . each %~ \(PlayerIdentity attrs) -> PlayerIdentity attrs
              {playerIdentityHand = Hand $ filter ((/= pcCardId card) . pcCardId) $ unHand $ playerIdentityHand attrs
              ,playerIdentityDeck = Deck $ filter ((/= pcCardId card) . pcCardId) $ unDeck $ playerIdentityDeck attrs
              ,playerIdentityDiscard = Discard $ filter ((/= pcCardId card) . pcCardId) $ unDiscard $ playerIdentityDiscard attrs}
        pure $ unheld current & entitiesL . sideSchemesL %~ \old -> foldr (uncurry HashMap.insert) old schemes
      ) g completed
    -- This hook also owns the ordinary window stack. Keeping that bookkeeping
    -- here is essential: offered abilities capture their triggering occurrence
    -- before costs, even when a cost returns/discards their source.
    endCheckWindows
    let attacksDone = [you | W.Window W.After event <- windows, you <- case event of
          W.IdentityAttack ident _ -> [ident]
          W.FriendlyPlayerAttack ident _ -> [ident]
          W.FriendlyCharacterAttack (IdentityCharacter ident) _ -> [ident]
          _ -> []]
        targets = [(attacker, enemy) | W.Window W.After (W.PlayerAttackResolved source you enemies) <- windows,
          let attacker = case source of AllyRef aid -> AllyRef aid; _ -> IdentityRef you, enemy <- enemies]
          <> [(case attacker of EnemyMinionId mid -> MinionRef mid; EnemyVillainId vid -> VillainRef vid; EnemyAttachmentId aid -> AttachmentRef aid, target) | W.Window W.After (W.EnemyAttackOnEnemy attacker target) <- windows]
        thwartsDone = [you | W.Window W.After event <- windows, you <- case event of
          W.HeroThwartComplete ident -> [ident]
          W.MadeBasicThwart ident -> [ident]
          _ -> []]
    pure $ captured {gameBlankedAttackTargets = gameBlankedAttackTargets captured <> startedBlanking, gamePendingAttackDefeats = filter ((`notElem` completedIds) . fst) attacks,
      gameAttacksThisTurn = nub (gameAttacksThisTurn captured <> targets),
      gameHeroAttackedThisPhase = nub (gameHeroAttackedThisPhase captured <> attacksDone),
      gameHeroThwartedThisPhase = nub (gameHeroThwartedThisPhase captured <> thwartsDone)}
      & windowDepthL +~ 1 & windowsL %~ (windows :) & playedInWindowsL %~ ([] :)
  VillainMessage vid VillainEndAttack -> pure (captureEnemyAttackCompletion (EnemyVillainId vid) g) {gameBoostAllyDefeatEffects = filter (\(e,_,_) -> e /= EnemyVillainId vid) (g.gameBoostAllyDefeatEffects)}
  MinionMessage mid MinionEndAttack -> pure (captureEnemyAttackCompletion (EnemyMinionId mid) g) {gameBoostAllyDefeatEffects = filter (\(e,_,_) -> e /= EnemyMinionId mid) (g.gameBoostAllyDefeatEffects)}
  RemoveFromPlay (AllyRef aid) | Just ally <- lookup aid (gameAllies g), toCardCode (getCardDef ally) == "32066" -> g <$ gameOver Lost
  RemoveFromGame (AllyRef aid) | Just ally <- lookup aid (gameAllies g), toCardCode (getCardDef ally) == "32066" -> g <$ gameOver Lost
  ReturnToHand (AllyRef aid) | Just ally <- lookup aid (gameAllies g), toCardCode (getCardDef ally) == "32066" -> g <$ gameOver Lost
  AddToVictoryDisplay (AllyRef aid) | Just ally <- lookup aid (gameAllies g), toCardCode (getCardDef ally) == "32066" -> g <$ gameOver Lost
  TreacheryMessage tid (RevealTreachery _)
    | Just treachery <- HashMap.lookup tid (gameTreacheries g) -> do
        marked <- runMessage (TreacheryMessage tid TreacheryMarkResolved) treachery
        pure $ g & entitiesL . treacheriesL . at tid ?~ marked
  -- SP//dr's two physical cards keep their own exhaustion. The logical identity
  -- holds their shared dial and the counters/attachments transferred by both flips.
  IdentityMessage ident PutSetupCardsIntoPlay
    | Just p <- HashMap.lookup ident (gamePlayers g)
    , toCardCode p == "31002a"
    , null [u | u <- HashMap.elems (gameSupports g), toCardCode (Support.supportCardDef $ toAttrs u) == "31001b", Support.supportController (toAttrs u) == ident] -> do
        sid <- getRandom
        let inactive = lookupSupport "31001b" ident sid
        pure $ g & (entitiesL . supportsL . at sid ?~ inactive)
          & (entitiesL . playersL . ix ident %~ \(PlayerIdentity a) -> PlayerIdentity a {playerIdentitySupports = HashSet.insert sid (playerIdentitySupports a)})
  IdentityMessage ident (ChangedToForm side)
    | Just p <- HashMap.lookup ident (gamePlayers g)
    , toCardCode p `elem` ["31001a", "31002a"]
    , playerIdentitySide (toAttrs p) /= side -> do
        allowed <- identityMatches IdentityCanChangeForm ident
        if not allowed then pure g else case side of
          A -> case [u | u <- HashMap.elems (gameSupports g), toCardCode (Support.supportCardDef $ toAttrs u) == "31001b", Support.supportController (toAttrs u) == ident] of
            inactive : _ -> do
              let sid = toId inactive
                  uid = UpgradeId $ unSupportId sid
                  pilot = lookupUpgrade "31002b" ident uid
                  old = toAttrs p
              pilot' <- if playerIdentityExhausted old then runMessage (UpgradeMessage uid ExhaustedUpgrade) pilot else pure pilot
              let player = PlayerIdentity old
                    { playerIdentityExhausted = Support.supportExhausted (toAttrs inactive)
                    , playerIdentitySupports = HashSet.delete sid (playerIdentitySupports old)
                    , playerIdentityUpgrades = HashSet.insert uid (playerIdentityUpgrades old)
                    }
              pure $ g & (entitiesL . supportsL %~ HashMap.delete sid)
                & (entitiesL . upgradesL . at uid ?~ pilot')
                & (entitiesL . playersL . at ident ?~ player)
            [] -> pure g
          B -> case [u | u <- HashMap.elems (gameUpgrades g), toCardCode (getCardDef u) == "31002b", upgradeController (toAttrs u) == ident] of
            pilot : _ -> do
              let uid = toId pilot
                  sid = SupportId $ unUpgradeId uid
                  inactive = lookupSupport "31001b" ident sid
                  old = toAttrs p
              inactive' <- if playerIdentityExhausted old then runMessage (SupportMessage sid ExhaustedSupport) inactive else pure inactive
              let player = PlayerIdentity old
                    { playerIdentityExhausted = upgradeExhausted (toAttrs pilot)
                    , playerIdentitySupports = HashSet.insert sid (playerIdentitySupports old)
                    , playerIdentityUpgrades = HashSet.delete uid (playerIdentityUpgrades old)
                    }
              pure $ g & (entitiesL . upgradesL %~ HashMap.delete uid)
                & (entitiesL . supportsL . at sid ?~ inactive')
                & (entitiesL . playersL . at ident ?~ player)
            [] -> pure g
          _ -> pure g
  -- begun before the scenario takes the card from where it was, so where that was is
  -- known; a card still to be revealed for a player eliminated meanwhile is discarded
  CardSpecific GameRef "grantedSurge" (fromJSON -> Success (you :: IdentityId, _ :: CardId)) -> do
    times <- whenRevealedTimes you
    replicateM_ times $ surge you
    pure g
  RevealEncounterCard ident card | not (HashMap.member ident (gamePlayers g)) -> do
    discardCard card
    pure g
  RevealEncounterCardFrom _ ident card | not (HashMap.member ident (gamePlayers g)) -> do
    discardCard card
    pure g
  RevealEncounterCardFrom source ident card -> do
    -- Naming a card type must happen before the card is turned faceup or focused.
    checkWindows [W.Window W.Would $ W.EncounterCardRevealed source ident card]
    cardSpecific GameRef "revealAfterWould" (source, ident, card)
    pure g
  CardSpecific GameRef "revealAfterWould" (fromJSON -> Success (source, ident, card)) -> do
    unless (cdCardType (getCardDef card) == EventType) $
      focusing [EncounterCard card] $
        checkWindows [W.Window When $ W.EncounterCardRevealed source ident card]
    push $ RevealedEncounterCardFrom source ident card
    pure g
  CardSpecific GameRef "revealAfterWouldWithoutWhenRevealed" (fromJSON -> Success (source, ident, card)) -> do
    unless (cdCardType (getCardDef card) == EventType) $
      focusing [EncounterCard card] $
        checkWindows [W.Window When $ W.EncounterCardRevealed source ident card]
    push $ RevealedEncounterCardWithoutWhenRevealedFrom source ident card
    pure g
  RevealEncounterCard ident card -> do
    let
      scenario = toAttrs (g.gameScenario)
      source
        | card `elem` (scenarioDiscard scenario <> concatMap pileDiscard (HashMap.elems $ scenarioDecks scenario)) = W.RevealedFromDiscardPile
        | EncounterCard card `elem` scenarioSetAsideCards scenario = W.RevealedFromSetAside
        | card `elem` gameRemovedFromGameEncounterCards g = W.RevealedFromRemovedFromGame
        | otherwise = W.RevealedFromEncounterDeck
    revealEncounterCardFrom source ident card
    pure g
  -- We want to empty the queue for triggering a resolution
  EndCheckWindows ->
    pure $
      clearCompletedAttackBlanking g
        & (windowDepthL -~ 1)
        & (windowsL %~ drop 1)
        & (playedInWindowsL %~ drop 1)
        & ( usedAbilitiesL
              . each
              %~ filter
                ( \(a, d) ->
                    (case baseLimit (abilityLimit a) of
                      PerWindow _ -> False
                      SharedPerWindow _ _ -> False
                      _ -> True) || d < gameWindowDepth g
                )
          )
  _ -> pure g

runGameMessage :: (HasGame m, HasQueue m, MonadThrow m, MonadRandom m) => Message -> Game -> m Game
runGameMessage msg g@Game {..} = runQueueT $ case msg of
  -- Owner-only debug requests use a small allowlist, preserving card metadata and
  -- bypassing normal damage/threat/status windows. The saved step remains undoable.
  CardSpecific target "debugSetValues" (Object values) -> pure $ case target of
    IdentityRef ident -> g & entitiesL . playersL . ix ident %~ debugPatch "" values
    AllyRef ident -> g & entitiesL . alliesL . ix ident %~ debugPatch "ally" values
    VillainRef ident -> g & entitiesL . villainsL . ix ident %~ debugPatch "villain" values
    MinionRef ident -> g & entitiesL . minionsL . ix ident %~ debugPatch "minion" values
    MainSchemeRef ident -> g & entitiesL . mainSchemesL . ix ident %~ debugPatch "mainScheme" values
    SideSchemeRef ident -> g & entitiesL . sideSchemesL . ix ident %~ debugPatch "sideScheme" values
    SupportRef ident -> g & entitiesL . supportsL . ix ident %~ debugPatch "support" values
    UpgradeRef ident -> g & entitiesL . upgradesL . ix ident %~ debugPatch "upgrade" values
    AttachmentRef ident -> g & entitiesL . attachmentsL . ix ident %~ debugPatch "attachment" values
    EnvironmentRef ident -> g & entitiesL . environmentsL . ix ident %~ debugPatch "environment" values
    _ -> g
  EventMessage eid ResolvedEvent ->
    pure g {gameResolvingEvents = Just $ filter (/= eid) $ fromMaybe [] gameResolvingEvents}
  CardSpecific GameRef "resolveKeywordRetaliate" (fromJSON -> Success (enemy,attacker)) -> do
    amount <- retaliationAmount enemy
    when (amount > 0) $ retaliate (characterRef enemy) attacker amount
    pure g
  CardSpecific GameRef "icemanDelayedFrostbite" (fromJSON -> Success (you :: IdentityId, enemy :: EnemyId)) -> do
    cards <- filter ((== "46002") . toCardCode) <$> getSetAsidePlayerCards
    present <- selectAny (EnemyWithId enemy)
    when present $ for_ (headMay cards) $ \card -> putIntoPlayOn you card (enemyRef enemy)
    pure g
  CardSpecific GameRef "ncAttackCompletion" (fromJSON -> Success (you :: IdentityId, enemy :: EnemyId, before :: Natural, retaliation :: Bool, delayed :: [(Source,Natural)])) -> do
    for_ (lookup you $ gamePlayers g) $ \player ->
      cardSpecific GameRef "ncResolveAttackCompletion"
        (you, enemy, retaliation, [effect | effect <- delayed, playerIdentityAttackDamageTaken (toAttrs player) <= before])
    pure g
  CardSpecific GameRef "resolveDelayedAttackDamage" (fromJSON -> Success (enemy :: EnemyId,source :: Source,amount :: Natural)) -> do
    present <- inAnyArea $ selectAny $ EnemyWithId enemy
    when present $ dealDamage enemy source (toDamage amount FromAbility)
    pure g
  CardSpecific GameRef "ncResolveAttackCompletion" (fromJSON -> Success (you :: IdentityId, enemy :: EnemyId, retaliation :: Bool, effects :: [(Source,Natural)])) -> do
    pending <- isQueued (attackCompletionWindowFor enemy)
    n <- if retaliation then retaliationAmount (IdentityCharacter you) else pure 0
    if pending then do
      let completion = [W.Window W.After $ W.RetaliationPending (IdentityCharacter you) (toCharacter enemy) | n > 0]
            <> [W.Window W.After $ W.DelayedAttackDamagePending you enemy source amount | (source,amount) <- effects]
      replaceMatchingMessage (\case CheckWindows windows -> [CheckWindows $ nub $ completion <> windows]; other -> [other]) (attackCompletionWindowFor enemy)
      pure g
    else do
      case lookup you (gamePlayers g) of
        Nothing -> pure g
        Just player -> do
          n <- if retaliation then sum . map (\case Retaliate amount -> amount; _ -> 0) <$> getModifiedKeywords player else pure 0
          let continue ret rest = cardSpecific GameRef "ncResolveAttackCompletion" (you, enemy, ret, rest :: [(Source,Natural)])
          let choices =
                [labeled "$label.resolveRetaliate" (retaliate (IdentityRef you) (toCharacter enemy) n >> continue False effects) | n > 0]
                <> [targeting source (dealDamage enemy source (toDamage amount FromAbility) >> continue retaliation rest)
                   | ((source,amount),rest) <- removeEach effects]
          unless (null choices) $ chooseOrRunOne you choices
          pure g

  CardSpecific GameRef key (fromJSON -> Success (mid :: MinionId, you :: IdentityId))
    | key `elem` ["ncDealMinionAndPass", "ncDealMinionAsEncounter"] -> do
    case lookup mid (gameMinions g) of
      Nothing -> pure g
      Just minion -> do
        let attrs = toAttrs minion
        frameworkDiscard $ for_ (minionUpgrades attrs) removeFromPlay
        frameworkDiscard $ for_ (minionAttachments attrs) removeFromPlay
        for_ (Minion.minionHeldCards attrs) discardCard
        for_ (Minion.minionHeldTreacheries attrs) discardCard
        minionDisengages (minionEngagedIdentity attrs) mid
        case toCard minion of
          EncounterCard card -> do
            identityStep you (DealtEncounterCard card)
            when (key == "ncDealMinionAndPass") $ cardSpecific GameRef "ncPassDealtEncounter" (you, card)
          _ -> pure ()
        pure $ g & (entitiesL . minionsL %~ deleteMap mid) & (removedEntitiesL . minionsL %~ insertMap mid minion)
  CardSpecific GameRef "ncDealFoundEncounter" (fromJSON -> Success (you :: IdentityId, card :: EncounterCard)) -> do
    removeFromGame card
    cardSpecific GameRef "ncFinishDealingFound" (you, card)
    pure g
  CardSpecific GameRef "ncFinishDealingFound" (fromJSON -> Success (you :: IdentityId, card :: EncounterCard)) -> do
    identityStep you (DealtEncounterCard card)
    pure g {gameRemovedFromGameEncounterCards = filter (/= card) gameRemovedFromGameEncounterCards}
  CardSpecific GameRef "ncPassDealtEncounter" (fromJSON -> Success (you :: IdentityId, card :: EncounterCard)) -> do
    players <- getPlayers
    let next = headMay $ drop 1 $ dropWhile (/= you) (players <> players)
    case next of
      Just recipient | length players > 1 ->
        pure $ g & entitiesL . playersL %~ fmap (\(PlayerIdentity attrs) -> PlayerIdentity $
          if playerIdentityId attrs == you then attrs {playerIdentityEncounterCards = filter (/= card) $ playerIdentityEncounterCards attrs}
          else if playerIdentityId attrs == recipient then attrs {playerIdentityEncounterCards = playerIdentityEncounterCards attrs <> [card]}
          else attrs)
      _ -> pure g

  -- Move threat while the departing character and its player still exist, so that
  -- placement interrupts resolve before elimination and its cleanup.
  departing | Just character <- departingThreatCharacter departing
    , fromMaybe 0 (Data.List.lookup character gameCharacterThreat) > 0
    , any ((== "39025b") . toCardCode . getCardDef) (HashMap.elems $ gameMainSchemes g) -> do
      modifiers <- getModifiers (characterRef character)
      if CannotLeavePlay `elem` modifiers then pure g else do
        moveCharacterThreatToMainScheme character
        push departing
        pure g

  PlaceCharacterThreat character amount -> do
    let old = fromMaybe 0 (Data.List.lookup character gameCharacterThreat)
    pure g {gameCharacterThreat = (character, old + amount) : filter ((/= character) . fst) gameCharacterThreat}
  RemoveCharacterThreat character amount -> do
    let old = fromMaybe 0 (Data.List.lookup character gameCharacterThreat)
    pure g {gameCharacterThreat = (character, subtractNatural amount old) : filter ((/= character) . fst) gameCharacterThreat}
  MoveCharacterThreatToMainScheme character -> do
    let amount = fromMaybe 0 (Data.List.lookup character gameCharacterThreat)
    when (amount > 0) $ placeThreatOnEach GameRef amount Matchers.MainScheme
    pure g {gameCharacterThreat = filter ((/= character) . fst) gameCharacterThreat}


  CardSpecific GameRef "cableDefeatedSchemeVictory" (fromJSON -> Success sid) -> do
    let found = lookup sid (gameSideSchemes g) <|> lookup sid (g ^. removedEntitiesL . sideSchemesL)
    case found of
      Nothing -> pure g
      Just scheme -> case toCard scheme of
        PlayerCard card -> do
          removeFromGame card
          pure $ g & playerVictoryDisplayL %~ (\cards -> if card `elem` cards then cards else cards <> [card])
        EncounterCard card -> do
          removeFromGame card
          pure $ g & victoryDisplayL %~ (\cards -> if card `elem` cards then cards else cards <> [card])
  CardSpecific GameRef "cableVictorySchemeIntoPlay" (fromJSON -> Success (you, card)) -> do
    case card of
      PlayerCard player -> putIntoPlay (fromMaybe you $ pcOwner player) player
      EncounterCard encounter -> putEncounterCardIntoPlay you encounter
    pure $ g & playerVictoryDisplayL %~ filter ((/= toCardId (card :: Card)) . pcCardId)
      & victoryDisplayL %~ filter ((/= toCardId card) . ecCardId)
  CardSpecific GameRef "cableTemporalLeapMove" (fromJSON -> Success (main, cid)) -> do
    let sid = SideSchemeId $ unCardId cid
        current = maybe 0 getMainSchemeThreat (lookup main $ gameMainSchemes g)
        n = min 4 current
    present <- selectAny $ SideSchemeWithId sid
    when present $ do
      cardSpecific (MainSchemeRef main) "moveThreatOut" n
      cardSpecific (SideSchemeRef sid) "moveThreatIn" n
    pure g
  CardSpecific GameRef "cableUpgradeFromDiscard" (fromJSON -> Success (you, aid, card)) -> do
    valid <- canAttachPlayerUpgradeToAlly aid card
    when valid $ push $ PutCardIntoPlay you card (TargetPayment $ AllyRef aid) Nothing False
    pure g

  CardSpecific GameRef "deferEnemyActivation" (fromJSON -> Success (enemy, messages)) -> do
    let completed windows = any (\case W.Window W.After (W.EnemyActivation _ _) -> True; _ -> False) windows
        afterFrame = headMay [depth | (depth, windows) <- zip [0 :: Int ..] gameWindows, completed windows]
        -- Unopened nested windows in the queue have their own EndCheckWindows;
        -- skip them while locating the end of an already open ancestor frame.
        endOfFrame depth = findEnd (0 :: Int) depth
        findEnd _ _ [] = Nothing
        findEnd nested depth (message : rest) = case message of
          CheckWindows _ -> (1 +) <$> findEnd (nested + 1) depth rest
          EndCheckWindows | nested > 0 -> (1 +) <$> findEnd (nested - 1) depth rest
          EndCheckWindows | depth == 0 -> Just 0
          EndCheckWindows -> (1 +) <$> findEnd 0 (depth - 1) rest
          _ -> (1 +) <$> findEnd nested depth rest
        completionIndex = \case
          [] -> Nothing
          CheckWindows windows : _ | completed windows -> Just 0
          _ : rest -> (1 +) <$> completionIndex rest
        boundary queue = maybe (completionIndex queue) (\depth -> endOfFrame depth queue) afterFrame
        pending = [(enemy :: EnemyId, messages :: [Message])]
        enqueue queue = case boundary queue of
          Nothing -> messages <> queue
          Just index -> let (before, after) = splitAt (index + 1) queue in before <> case after of
            CardSpecific GameRef "pendingEnemyActivations" (fromJSON -> Success existing) : rest ->
              CardSpecific GameRef "pendingEnemyActivations" (toJSON $ existing <> pending) : rest
            _ -> CardSpecific GameRef "pendingEnemyActivations" (toJSON pending) : after
    withQueue_ enqueue
    pure g
  CardSpecific GameRef "pendingEnemyActivations" (fromJSON -> Success pending) -> do
    present <- filterM (\(enemy, _) -> selectAny $ EnemyWithId enemy) (pending :: [(EnemyId,[Message])])
    case present of
      [] -> pure ()
      [(_, messages)] -> enqueueContinuation messages
      _ -> do
        first <- selectJust FirstPlayer
        chooseOneAtATimePrompted first "$label.chooseActivationOrder"
          [targeting (enemyRef enemy) (whenM (selectAny $ EnemyWithId enemy) $ enqueueContinuation messages) | (enemy, messages) <- present]
    pure g
  CardSpecific GameRef "resolveLabeledAbility" (fromJSON -> Success (you, ab, windows, payment)) -> do
    let labels = labeledAbilityTypes g ab
        continuations = filter isAbilityWindowContinuation (abilityChoices ab)
    canceled <- cancelLabeledAbility you labels
    if canceled
      then traverse_ (resolveAs you) continuations
      else do
        replaced <- if Ability.Attack `elem` labels then replaceHeroAttackWithDiscardAndStun you else pure False
        if replaced
          then traverse_ (resolveAs you) continuations
          else do
            when (Ability.Defense `elem` labels) $ beginLabeledDefense you
            case abilitySource ab of
              IdentityRef _ | abilityType ab /= Basic && abilityIndex ab /= 100 ->
                checkWindows [W.Window W.When $ W.UsedCardAbility you (abilitySource ab) (abilityIndex ab)]
              _ -> pure ()
            cardSpecific GameRef "finishLabeledAbility" (you, ab, windows :: [W.WindowType], payment :: Payment)
    pure g
  CardSpecific GameRef "finishLabeledAbility" (fromJSON -> Success (you, ab, windows, payment)) -> do
    for_ (abilityChoices ab) (resolveAs you . abilityChoiceWithPayment you windows payment)
    abilityResolved (abilitySource ab)
    let def = case abilitySource ab of
          IdentityRef _ | abilityType ab == Basic || abilityIndex ab == 100 -> Nothing
          source -> abilityCardDef g (refocus source)
    for_ def \d -> checkWindows [W.Window W.After $ W.ResolvedAbility you d]
    case abilitySource ab of
      IdentityRef _ | abilityType ab /= Basic && abilityIndex ab /= 100 -> checkWindows [W.Window W.After $ W.UsedCardAbility you (abilitySource ab) (abilityIndex ab)]
      _ -> pure ()
    pure g {gameResolvingAbilitySubTypes = (abilitySource ab, labeledAbilityTypes g ab, False) : gameResolvingAbilitySubTypes}
  AbilityResolved source -> do
    let pop [] = []
        pop (scope@(active, _, _) : rest)
          | active == source = rest
          | otherwise = scope : pop rest
    pure g {gameResolvingAbilitySubTypes = pop $ gameResolvingAbilitySubTypes}
  CardSpecific GameRef "markLabeledThwart" (fromJSON -> Success source) -> do
    let mark [] = []
        mark ((active, labels, begun) : rest)
          | active == source = (active, labels, True) : rest
          | otherwise = (active, labels, begun) : mark rest
    pure g {gameResolvingAbilitySubTypes = mark $ gameResolvingAbilitySubTypes}
  CardSpecific target "applyToughness" _ -> do
    for_ (cardInPlay g target) $ \card -> do
      modifiers <- getModifiers target
      blanked <- isBlanked target
      let base = if blanked then [] else toList $ cdKeywords $ getCardDef card
          keywords = filter (\k -> LoseKeyword k `notElem` modifiers) (base <> [k | KeywordModifier k <- modifiers])
      when (Toughness `elem` keywords) $ case target of
        AllyRef aid -> giveStatus aid ToughStatus
        MinionRef mid -> giveStatus mid ToughStatus
        _ -> pure ()
    pure g
  CardSpecific GameRef "mgSetupSenator" _ -> do
    first <- selectJust FirstPlayer
    case [a | a <- HashMap.elems (gameAllies g), toCardCode (getCardDef a) == "32066"] of
      [] -> do
        aid <- getRandom
        let ally = lookupAlly "32066" first aid
        pure $ g & (entitiesL . alliesL . at aid ?~ ally)
          & \x -> x {gameUncontrolledAllies = x.gameUncontrolledAllies <> [aid]}
      _ -> pure g
  CardSpecific GameRef "mgRescueSenator" _ -> do
    first <- selectJust FirstPlayer
    let senators = [a | a <- HashMap.elems (gameAllies g), toCardCode (getCardDef a) == "32066"]
    for_ senators \a -> do
      allyStep (toId a) (AllyChangedController first)
      identityStep first (AllyCreated $ toId a)
    pure $ g {gameUncontrolledAllies = filter (`notElem` map toId senators) (g.gameUncontrolledAllies)}
  CardSpecific GameRef "mgSetAsideCaptives" _ -> do
    aside <- getSetAsidePlayerCards
    let codes = map toCardCode aside <> map (toCardCode . getCardDef) (HashMap.elems $ gameAllies g)
    for_ ["32089","32090","32091","32092"] \code -> unless (code `elem` codes) do
      cid <- getRandom
      setAside [PlayerCard $ MkPlayerCard cid (MGPlayer.lookupPlayerCard code) Nothing Nothing]
    pure g
  CardSpecific GameRef "mgPutMagnetoAlly" _ -> do
    -- The Sentinel Factory's printed Setup applies in campaign play as well.
    unless (any ((== "32172b") . toCardCode . getCardDef) $ HashMap.elems $ gameAllies g) do
      first <- selectJust FirstPlayer
      cid <- getRandom
      putIntoPlay first (MkPlayerCard cid (MGPlayer.lookupPlayerCard "32172b") (Just first) (Just first))
    pure g
  CardSpecific GameRef "mgRescueJubilee" _ -> do
    first <- selectJust FirstPlayer
    for_ [a | a <- HashMap.elems (gameAllies g), title (cdName $ getCardDef a) == "Jubilee"] removeFromPlay
    cid <- getRandom
    putIntoPlay first (MkPlayerCard cid (MGPlayer.lookupPlayerCard "32088b") (Just first) (Just first))
    pure g
  CardSpecific GameRef "mgNewMansionVillain" (fromJSON -> Success code) -> do
    let villains = [v | v <- HashMap.elems (gameVillains g), toCardCode (getCardDef v) == (code :: CardCode)]
    for_ villains \villain -> do
      let matches = [m | m <- HashMap.elems (gameMinions g), title (cdName $ getCardDef m) == title (cdName $ getCardDef villain)]
      for_ matches \minion -> do
        removeFromPlay minion
        villainActivatesAgainst (toId villain) (minionEngagedIdentity $ toAttrs minion)
    pure g
  CardSpecific GameRef "mgAttachTopSentinel" (fromJSON -> Success (mid,card)) -> do
    -- This attachment is put into play attached, without resolving its reveal text.
    let aid = AttachmentId $ unCardId $ ecCardId card
        attachment = lookupAttachment (toCardCode card) aid
    removeFromGame (CardIdRef $ ecCardId card)
    attachTo aid (mid :: MinionId)
    pure $ g & (entitiesL . attachmentsL . at aid ?~ attachment)

  CardSpecific GameRef "mgDealMarkedBoost" (fromJSON -> Success (target,_ :: IdentityId)) -> do
    let marked = nub $ mapMaybe (Attachment.attachmentIdentity . toAttrs)
          [a | a <- HashMap.elems (gameAttachments g), toCardCode (getCardDef a) == "32107"]
    first <- selectJust FirstPlayer
    mcard <- getBoostCard target
    for_ mcard \card -> unless (null marked) $
      chooseOrRunOne first [targeting ident (consumeBoostCard card >> identityStep ident (DealtEncounterCard card)) | ident <- marked]
    pure g
  CardSpecific GameRef "mgGiveDiscardedBoost" (fromJSON -> Success card) -> do
    villain <- selectOne Matchers.ActiveVillain
    for_ villain (\vid -> villainStep vid $ VillainDealtBoost card)
    removeFromGame (CardIdRef $ ecCardId card)
    pure g
  CardSpecific GameRef "mgAfterBoostAllyDefeatThreat" (fromJSON -> Success (enemy,n)) -> do
    let attacking = case enemy of
          EnemyVillainId vid -> isJust (villainAttackDetails =<< lookup vid (gameVillains g))
          EnemyMinionId mid -> isJust (minionAttackDetails =<< lookup mid (gameMinions g))
          EnemyAttachmentId _ -> False
    pure $ if attacking then g {gameBoostAllyDefeatEffects = g.gameBoostAllyDefeatEffects <> [(enemy,n,False)]} else g
  CardSpecific GameRef "mgAfterBoostAllyDefeatMagnet" (fromJSON -> Success enemy) -> do
    let attacking = case enemy of
          EnemyVillainId vid -> isJust (villainAttackDetails =<< lookup vid (gameVillains g))
          EnemyMinionId mid -> isJust (minionAttackDetails =<< lookup mid (gameMinions g))
          EnemyAttachmentId _ -> False
    pure $ if attacking then g {gameBoostAllyDefeatEffects = g.gameBoostAllyDefeatEffects <> [(enemy,0,True)]} else g
  CardSpecific GameRef "mgTuckObligation" (fromJSON -> Success (oid,sid)) -> do
    for_ (lookup oid $ gameObligations g) \obligation -> do
      let card = MkPlayerCard (CardId $ unObligationId oid) (getCardDef obligation) Nothing (obligationOwner $ toAttrs obligation)
      removeFromGame (ObligationRef oid)
      cardSpecific (SideSchemeRef sid) "mgHoldCard" card
    pure g
  CardSpecific GameRef "mgAddBombCounter" (fromJSON -> Success enemy) -> do
    let source = maybe GameRef toSource $ headMay [a | a <- HashMap.elems (gameAllies g), toCardCode (getCardDef a) == "32090"]
        existing = fromMaybe 0 $ headMay [n | (target,_,n) <- g.gameBombCounters, target == enemy]
    pure $ g {gameBombCounters = filter (\(target,_,_) -> target /= enemy) (g.gameBombCounters) <> [(enemy,source,existing+1)]}
  CardSpecific GameRef "findAndAttachTouched" (fromJSON -> Success (you, host)) -> do
    when (host /= IdentityCharacter you) $ for_ (findTouchedCard you g) \card -> do
      let uid = UpgradeId $ unCardId $ pcCardId card
      unless (HashMap.member uid $ gameUpgrades g) $ do
        removePlayerCardFromHeldZones card
        removeFromGame card
        putIntoPlay you card
      detachUpgrade uid
      cardSpecific (UpgradeRef uid) "attachTouched" host
      for_ [toId p | p <- HashMap.elems (gamePlayers g), card `elem` unDeck (playerIdentityDeck $ toAttrs p)] shuffleDeck
    pure $ g & collectionL %~ filter (\c -> not (toCardCode c == "38002" && case c of PlayerCard pc -> pcOwner pc == Just you; _ -> False))
  CardSpecific GameRef "setAsideTouched" (fromJSON -> Success you) -> do
    for_ (findTouchedCard you g) \card -> do
      removePlayerCardFromHeldZones card
      removeFromGame card
      setAside [PlayerCard card]
      for_ [toId p | p <- HashMap.elems (gamePlayers g), card `elem` unDeck (playerIdentityDeck $ toAttrs p)] shuffleDeck
    pure $ g & collectionL %~ filter (\c -> not (toCardCode c == "38002" && case c of PlayerCard pc -> pcOwner pc == Just you; _ -> False))
  CardSpecific GameRef "medLabRemoveCapturedCard" (fromJSON -> Success cid) ->
    pure $ g & collectionL %~ filter ((/= cid) . toCardId)
      & victoryDisplayL %~ filter ((/= cid) . ecCardId)
  CardSpecific GameRef "mgShapeshifterEnteredHand" (fromJSON -> Success (you,card)) -> do
    case toCardCode (card :: PlayerCard) of
      "38027" -> forcedResponse card $ resolveAs you (PlaceThreat (toSource card) 2 EncounterMainScheme)
      "32082" -> forcedResponse card do
        supports <- selectList (SupportControlledBy $ IdentityWithId you)
        allies <- selectList (AllyControlledBy $ IdentityWithId you)
        let targets = map SupportRef supports <> map AllyRef allies
        unless (null targets) $ chooseOne you [targeting target (removeFromPlay target) | target <- targets]
      "32083" -> forcedResponse card do
        let mystiques = [m | m <- HashMap.elems (gameMinions g), toCardCode (getCardDef m) == "32080"]
        if null mystiques
          then searchAndRevealEncounterCard you (CardWithTitle "Mystique" <> CardWithType MinionType)
          else for_ mystiques \minion -> do
            hero <- selectAny (IdentityWithId you <> HeroIdentity)
            if hero then enemyAttacks (toId minion) you else minionSchemes (toId minion)
      _ -> pure ()
    pure g
  CardSpecific GameRef "psylockeRedirectAttackWindows" (fromJSON -> Success (you :: IdentityId, source :: Source, enemy :: EnemyId, friend :: CharacterId, keywords :: [Keyword])) -> do
    let adjust window = case W.windowType window of
          W.IdentityAttackTarget who target | who == you && target == enemy -> Nothing
          W.IdentityAttack who target | who == you && target == enemy && W.windowTiming window == W.When ->
            Just window {W.windowType = W.FriendlyPlayerAttack you friend}
          W.MakingKeywordAttack who from target _ _ | who == you && from == source && target == enemy ->
            Just window {W.windowType = W.MakingFriendlyKeywordAttack you source friend keywords}
          _ -> Just window
    pure g {gameWindows = case g.gameWindows of [] -> []; current : rest -> mapMaybe adjust current : rest}
  CardSpecific GameRef "psylockeRedirectedAttackDamage" (fromJSON -> Success (_you :: IdentityId, source :: Source, friend :: CharacterId, damage :: Damage)) -> do
    modifiers <- getModifiers (characterRef friend)
    let def = abilityCardDef g (refocus source)
        taken = sum [n | DamageTakenModifier n <- modifiers]
          + sum [n | AttackDamageTakenModifier n <- modifiers]
          + sum [n | DamageTakenFromPrintedResource resource n <- modifiers, maybe False (elem resource . printedResources) def]
        amount = if damageAmount damage == 0 then 0 else fromIntegral (max 0 (fromIntegral (damageAmount damage) + taken))
    damageCharacter friend source damage {damageAmount = amount}
    pure g
  CardSpecific GameRef "psylockeFriendlyAttackResolved" (fromJSON -> Success (you :: IdentityId, source :: Source, friend :: CharacterId, ranged :: Bool)) -> do
    present <- selectAny (CharacterWithId friend)
    modifiers <- getModifiers (characterRef friend)
    keywords <- case friend of
      IdentityCharacter who -> getsGame (lookup who . gamePlayers) >>= maybe (pure []) getModifiedKeywords
      AllyCharacter aid -> do
        blanked <- isBlanked (AllyRef aid)
        let printed = if blanked then [] else maybe [] (toList . cdKeywords . getCardDef) (lookup aid $ gameAllies g)
        pure $ filter (\k -> LoseKeyword k `notElem` modifiers) (printed <> [k | KeywordModifier k <- modifiers])
      _ -> pure []
    attackerModifiers <- getModifiers (IdentityRef you)
    when (present && not ranged && AttacksIgnoreRetaliate `notElem` attackerModifiers) do
      let n = sum [amount | Retaliate amount <- keywords]
      when (n > 0) $ retaliate (characterRef friend) (IdentityCharacter you) n
    checkWindows [W.Window W.After $ W.FriendlyPlayerAttack you friend]
    case source of
      EventRef eid -> do
        let attacked = maybe [] (eventAttacked . toAttrs) (lookup eid $ gameEvents g)
        when (null attacked) $ checkWindows [W.Window W.After $ W.PlayerAttackResolved source you []]
      _ -> pure ()
    pure g
  CardSpecific GameRef "mgDivideHealing" (fromJSON -> Success (you :: IdentityId, source :: Source, n :: Natural, matcher)) -> do
    cardSpecific GameRef "mgAssignHealing" (you, source, n, matcher :: CharacterMatcher, [] :: [(CharacterId, Natural)])
    pure g
  CardSpecific GameRef "mgAssignHealing" (fromJSON -> Success (you :: IdentityId, source :: Source, n :: Natural, matcher :: CharacterMatcher, assigned :: [(CharacterId, Natural)])) -> do
    let given c = sum [amount | (target, amount) <- assigned, target == c]
    characters <- filterM (\c -> do
      damage <- characterStoredDamage c
      forbidden <- elem CannotBeHealedByPlayerCards <$> getModifiers (characterRef c)
      pure $ damage > given c && not forbidden) =<< selectList matcher
    if n == 0 || null characters then
      for_ (HashMap.toList $ HashMap.fromListWith (+) assigned) (\(c, amount) -> heal c amount)
    else chooseOne you [targeting (characterRef c) (cardSpecific GameRef "mgAssignHealing" (you, source, n - 1, matcher, (c,1) : assigned)) | c <- characters]
    pure g
  CardSpecific GameRef "mgDivideRemoval" (fromJSON -> Success (you :: IdentityId, source :: Source, n :: Natural, matcher)) -> do
    cardSpecific GameRef "mgAssignRemoval" (you, source, n, matcher :: SchemeMatcher, [] :: [(SchemeId, Natural)])
    pure g
  CardSpecific GameRef "mgAssignRemoval" (fromJSON -> Success (you :: IdentityId, source :: Source, n :: Natural, matcher :: SchemeMatcher, assigned :: [(SchemeId, Natural)])) -> do
    let given scheme = sum [amount | (target, amount) <- assigned, target == scheme]
    schemes <- filterM (\scheme -> (> given scheme) <$> selectCount SchemeThreat (SchemeWithId scheme)) =<< selectList matcher
    if n == 0 || null schemes then
      for_ (HashMap.toList $ HashMap.fromListWith (+) assigned) $ \(scheme, amount) -> case scheme of
        SchemeMainSchemeId sid -> mainSchemeStep sid (MainSchemeThreatRemoved source amount)
        SchemeSideSchemeId sid -> sideSchemeStep sid (SideSchemeThreatRemoved source amount)
    else chooseOne you [targeting (schemeRef scheme) (cardSpecific GameRef "mgAssignRemoval" (you, source, n - 1, matcher, (scheme,1) : assigned)) | scheme <- schemes]
    pure g
  CardSpecific GameRef "useAttackForDefense" (fromJSON -> Success (you, enemy)) ->
    pure $ modifyEnemyAttack enemy (\a -> if attackCharacter a == IdentityCharacter you && attackDefended a then a {attackUsesAttackForDefense = Just you} else a) g
  CardSpecific GameRef "mgAttackThreatReplacement" (fromJSON -> Success (enemy, you)) ->
    pure $ modifyEnemyAttack enemy (\a -> a {attackThreatRemoval = Just you}) g
  CardSpecific GameRef "capAttackDamageToEnemy" (fromJSON -> Success (attacker, target, n :: Natural)) ->
    pure $ modifyEnemyAttack attacker (\a -> a {attackEnemyDamageCaps = (target,n,0) : attackEnemyDamageCaps a}) g
  CardSpecific GameRef "recordEnemyAttackDamageTaken" (fromJSON -> Success (attacker, target, n :: Natural)) ->
    pure $ modifyEnemyAttack attacker (\a -> a {attackEnemyDamageCaps = [(who,cap,taken + if who == target then n else 0) | (who,cap,taken) <- attackEnemyDamageCaps a]}) g
  CardSpecific GameRef "aoaCapAttackDamage" (fromJSON -> Success (enemy, you, n :: Natural)) ->
    pure $ modifyEnemyAttack enemy (\a -> a {attackIdentityDamageCaps = (you, n, 0) : attackIdentityDamageCaps a}) g
  CardSpecific GameRef "aoaRecordAttackDamage" (fromJSON -> Success (enemy, you, n :: Natural)) ->
    pure $ modifyEnemyAttack enemy (\a -> a {attackIdentityDamageCaps = [(who, cap, taken + if who == you then n else 0) | (who, cap, taken) <- attackIdentityDamageCaps a]}) g
  CardSpecific GameRef "mgPartialAttackPrevention" (fromJSON -> Success (enemy, n)) ->
    pure $ modifyEnemyAttack enemy (\a -> a {attackPrevention = attackPrevention a + n}) g
  CardSpecific GameRef "aosTuckAlly" (fromJSON -> Success (aid,sid :: SideSchemeId,card :: PlayerCard)) -> do
    for_ (lookup aid $ gameAllies g) $ \ally -> do
      frameworkDiscard $ for_ (allyUpgrades $ toAttrs ally) removeFromPlay
      for_ (Ally.allyHeldCards $ toAttrs ally) discardCard
      for_ (Ally.allyHeldTreacheries $ toAttrs ally) discardCard
      frameworkDiscard $ for_ (HashMap.elems $ gameAttachments g) $ \attachment -> when (Attachment.attachmentAlly (toAttrs attachment) == Just aid) $ removeFromPlay attachment
      identityStep (getAllyController ally) (AllyRemoved aid)
      cardSpecific sid "mgHoldCard" card
    pure $ g & (entitiesL . alliesL %~ deleteMap aid)
      & (removedEntitiesL . alliesL %~ maybe id (insertMap aid) (lookup aid $ gameAllies g))
  CardSpecific GameRef "aosHostageAlly" (fromJSON -> Success (aid,sid)) -> do
    for_ (lookup aid $ gameAllies g) $ \ally -> identityStep (getAllyController ally) (AllyRemoved aid)
    pure g {gameUncontrolledAllies = aid : filter (/= aid) g.gameUncontrolledAllies, gameAosHostages = (sid,aid) : filter ((/= sid) . fst) g.gameAosHostages}
  CardSpecific GameRef "aosReleaseHostage" (fromJSON -> Success (sid,you)) -> do
    let held = [aid | (scheme,aid) <- g.gameAosHostages, scheme == sid]
    for_ held $ \aid -> do
      allyStep aid (AllyChangedController you)
      identityStep you (AllyCreated aid)
    pure g {gameUncontrolledAllies = filter (`notElem` held) g.gameUncontrolledAllies,gameAosHostages = filter ((/=sid) . fst) g.gameAosHostages}
  CardSpecific GameRef key (fromJSON -> Success (eid,mid)) | key `elem` ["aosBackupMinion","aosBackupMinionInitial"] -> do
    case lookup mid (gameMinions g) of
      Nothing -> pure g
      Just minion -> do
        let previous = headMay [old | (env,old) <- g.gameAosBackup, env == eid]
            owner = minionEngagedIdentity $ toAttrs minion
        for_ previous $ \old -> when (old /= mid) $ engageMinion old owner
        when (previous /= Just mid) $ minionDisengages owner mid
        when (key == "aosBackupMinion") do
          difficulty <- getDifficulty
          amount <- fromIntegral <$> fromGameValue (PerPlayer $ if difficulty == Expert then 2 else 1)
          heal mid amount
          when (difficulty == Expert) $ giveStatus mid ToughStatus
        pure g {gameAosBackup = (eid,mid) : filter (\(env,attached) -> env /= eid && attached /= mid) g.gameAosBackup}
  MinionMessage mid (MinionEngagedIdentity _) -> pure g {gameAosBackup = filter ((/=mid) . snd) g.gameAosBackup}
  CardSpecific GameRef key (fromJSON -> Success (source,from :: W.ThreatSource,scheme,n :: Natural))
    | key `elem` ["placeThreatBy","placeReportingThreatBy","placeVillainThreatBy"] -> do
      let resolvePlacement = do
            actor <- threatSourceCharacter source
            when (n > 0) $ for_ actor $ \character -> checkWindows [W.Window W.Would $ W.CharacterPlacingThreat character scheme n]
            cardSpecific GameRef continuation (source,from,scheme,n)
          continuation = case key of
            "placeReportingThreatBy" -> "placeReportingThreatAfterCharacterWindow"
            "placeVillainThreatBy" -> "placeVillainThreatAfterCharacterWindow"
            _ -> "placeThreatAfterCharacterWindow"
      if from `elem` [W.ThreatFromVillain, W.ThreatFromMinion] then do
        you <- getYou
        chooseOnePromptedAbout you "$label.resolveEnemyScheme" source
          [labeled (countLabel "placeActivationThreat" n) resolvePlacement]
      else resolvePlacement
      pure g
  CardSpecific GameRef "placeReportingThreatAfterCharacterWindow" (fromJSON -> Success (source,_ :: W.ThreatSource,scheme,n)) ->
    g <$ placeThreatOnReportingAfterCharacterWindow source scheme n
  CardSpecific GameRef "placeVillainThreatAfterCharacterWindow" (fromJSON -> Success (VillainRef vid,_ :: W.ThreatSource,scheme,n)) ->
    g <$ placeVillainSchemeThreatAfterCharacterWindow vid scheme n
  CardSpecific GameRef "placeThreatAfterCharacterWindow" (fromJSON -> Success (_ :: Source,from,scheme :: SchemeId,n :: Natural)) ->
    g <$ placeThreatFrom from scheme n
  CardSpecific GameRef "attachEncounterToCharacter" (fromJSON -> Success (card,character)) -> do
    let aid = AttachmentId $ unCardId $ ecCardId card
        existing = lookup aid $ gameAttachments g
        attachment = fromMaybe (createAttachment card) existing
    present <- characterMatches AnyCharacter character
    modifiers <- getModifiers (characterRef character)
    if not present || any (`elem` modifiers) [CannotHaveAttachedCards,CannotHaveAttachedEncounterCards]
      then do
        when (isNothing existing) $ discardCard card
        pure g
      else do
        cardSpecific GameRef "removeEncounterCardFromPiles" card
        attachToCharacter aid (character :: CharacterId)
        pure $ g {gameRemovedFromGameEncounterCards = filter ((/= ecCardId card) . ecCardId) gameRemovedFromGameEncounterCards}
          & entitiesL . attachmentsL %~ insertMap aid attachment
          & boostCardsL %~ HashMap.filter ((/= ecCardId card) . ecCardId)
  CardSpecific GameRef "aosAttachEncounterToCharacter" (fromJSON -> Success (card,character)) -> g <$ attachEncounterToCharacter card character
  CardSpecific GameRef "aosReflectPendingAttack" (fromJSON -> Success (_you :: IdentityId,vid,attacker :: Maybe CharacterId,source :: Maybe Source)) -> do
    pending <- withQueue $ \q -> (q,headMay [d | message <- concatMap queuedMessageLeaves q, VillainMessage target (VillainDamaged from d) <- [message], target == vid, Just from == source])
    for_ pending $ \d -> for_ attacker $ \character -> damageCharacter character (VillainRef vid) d {damageSource=FromAbility}
    pure g
  CardSpecific GameRef "aosReturnThunderbolt" (fromJSON -> Success (you,target :: Target)) -> do
    let candidates = filter (cardMatch $ CardWithType MinionType <> CardWithTrait AosTrait.Thunderbolt) g.gameVictoryDisplay
    shuffled <- shuffleM candidates
    case headMay shuffled of
      Nothing -> do
        cardSpecific target "aosGainSurge" ()
        pure g
      Just card -> do
        revealEncounterCard you card
        cardSpecific (MinionId $ unCardId $ ecCardId card) "aosPlaceDamageToFive" ()
        removeFromGame target
        pure g {gameVictoryDisplay=filter (/=card) g.gameVictoryDisplay}
  CardSpecific GameRef "aosReluctantFoe" (fromJSON -> Success (you,aid :: AttachmentId)) -> do
    titles <- getUniqueTitlesInPlay
    let candidates = [def | def <- allHeroCards, (title (cdName def)) `notElem` map fst titles]
    chooseOne you [cardLabeled (toCardCode def) $ cardSpecific GameRef "aosMakeReluctantFoe" (you,aid,toCardCode def) | def <- candidates]
    pure g
  CardSpecific GameRef "aosMakeReluctantFoe" (fromJSON -> Success (you,aid,code)) -> do
    mid <- getRandom
    let hero = AosHero.lookupHeroByCardCode code you
        h = toAttrs hero
        def = (getCardDef hero) {cdCardType=MinionType,cdKeywords=HashSet.singleton Villainous,cdTraits=HashSet.singleton AosTrait.Elite,cdEncounterSet=Just EncounterSet.AosBaronZemo,cdEncounterSetQuantity=Just 0}
        base = buildCard AosMinion.aosReluctantFoeMinion (you,mid)
    hp <- fromIntegral <$> fromGameValue (unHp $ Hero.heroStartingHP h)
    let minion = Minion.Minion $ Minion.overMinionAttrs (\a -> a {Minion.minionCardDef=def,Minion.minionScheme=Sch $ unThw $ Hero.heroBaseThwart h,Minion.minionAttack=Hero.heroBaseAttack h,Minion.minionHitPoints=HP hp}) base
    engageMinion mid you
    attachTo aid mid
    pure $ g & entitiesL . minionsL %~ insertMap mid minion
  CardSpecific GameRef "resolveVillainActivation" (fromJSON -> Success (vid, you)) -> do
    hero <- identityMatches HeroIdentity you
    resolveVillainActivationAfterWould vid $
      if hero then VillainAttacks you else VillainSchemesAgainst you
    pure g
  CardSpecific GameRef "resolveCurrentVillainActivation" (fromJSON -> Success you) | competitivePhaseStoppedFor g you -> pure g
  CardSpecific GameRef "resolveCurrentVillainActivation" (fromJSON -> Success you) -> do
    villain <- asYou you $ selectOne Matchers.ActiveVillain
    case villain of
      Just vid -> cardSpecific GameRef "resolveVillainActivation" (vid, you :: IdentityId)
      Nothing -> do
        code <- getScenarioCode
        when (code == "27100") $ ActivationSix.ambushThen GameRef "activationAfterAmbush" (you :: IdentityId)
    pure g
  CardSpecific GameRef "activationAfterAmbush" (fromJSON -> Success (entered, value)) -> do
    for_ (entered :: Maybe VillainId) \vid -> case fromJSON value of
      Success you -> villainActivatesAgainst vid you
      _ -> pure ()
    pure g
  DiscardedCard (EncounterCard card) -> pure $ g
    {gameRemovedFromGameEncounterCards = filter ((/= ecCardId card) . ecCardId) gameRemovedFromGameEncounterCards}
      & collectionL %~ filter ((/= ecCardId card) . toCardId)
  RemoveFromGame target
    | Just (EncounterCard card) <- cardInPlay g target
    , card `notElem` gameRemovedFromGameEncounterCards ->
        do
          modifiers <- getModifiers target
          if CannotLeavePlay `elem` modifiers then pure g
            else runGameMessage msg g {gameRemovedFromGameEncounterCards = gameRemovedFromGameEncounterCards <> [card]}
  CardSpecific _ "holdEncounterCard" (fromJSON -> Success card) | card `elem` gameRemovedFromGameEncounterCards ->
    runGameMessage msg g {gameRemovedFromGameEncounterCards = filter (/= card) gameRemovedFromGameEncounterCards}
  RevealedEncounterCardFrom _ _ card | card `elem` gameRemovedFromGameEncounterCards ->
    runGameMessage msg g {gameRemovedFromGameEncounterCards = filter (/= card) gameRemovedFromGameEncounterCards}
  RevealedEncounterCardWithoutWhenRevealedFrom _ _ card | card `elem` gameRemovedFromGameEncounterCards ->
    runGameMessage msg g {gameRemovedFromGameEncounterCards = filter (/= card) gameRemovedFromGameEncounterCards}
  RevealedEncounterCard _ card | card `elem` gameRemovedFromGameEncounterCards ->
    runGameMessage msg g {gameRemovedFromGameEncounterCards = filter (/= card) gameRemovedFromGameEncounterCards}
  RevealedEncounterCardWithoutWhenRevealed _ card | card `elem` gameRemovedFromGameEncounterCards ->
    runGameMessage msg g {gameRemovedFromGameEncounterCards = filter (/= card) gameRemovedFromGameEncounterCards}
  PutEncounterCardIntoPlay _ card | card `elem` gameRemovedFromGameEncounterCards ->
    runGameMessage msg g {gameRemovedFromGameEncounterCards = filter (/= card) gameRemovedFromGameEncounterCards}
  CardSpecific GameRef key value | key `elem` ["aosPrepareEvidence","aosGainEvidence","aosMakeAccusation","aosCompareAccusation","aosAccusedPenalty","aosRevealMole","aosEvidenceSetup","aosEvidenceNext","aosEvidenceSearch"] -> g <$ ShieldCampaign.runShieldGameStep key value
  CardSpecific GameRef "afterDiscardFromPlay" (fromJSON -> Success (PlayerCard card)) -> do
    for_ (pcOwner card <|> pcController card) $ \you -> do
      discarded <- maybe False (elem card . unDiscard . playerIdentityDiscard . toAttrs) <$> pure (lookup you $ gamePlayers g)
      when discarded $ checkWindows [W.Window W.After $ W.PlayerCardDiscardedFromPlay (fromMaybe you $ pcController card) card]
    pure g
  CardSpecific GameRef "aosInformantScheme" (fromJSON -> Success (you,source,scheme,n)) -> g <$ ShieldPlayer.informantScheme you source scheme n
  CardSpecific GameRef "aosIntelligence" (fromJSON -> Success you) -> do
    cardSpecific GameRef "aosIntelligenceChoose" you
    cards <- intelligenceCards you
    pure g {gamePrivateCards = HashMap.insert you (map EncounterCard cards) gamePrivateCards}
  CardSpecific GameRef "aosIntelligenceChoose" (fromJSON -> Success you) -> do
    cards <- intelligenceCards you
    chooseOne you $ labeled "$label.done" (cardSpecific GameRef "aosIntelligenceDone" you)
      : [targeting card (cardSpecific GameRef "aosIntelligencePartner" (you,card)) | card <- cards]
    pure g
  CardSpecific GameRef "aosIntelligencePartner" (fromJSON -> Success (you,first :: EncounterCard)) -> do
    cards <- intelligenceCards you
    chooseOne you $ labeled "$label.done" (cardSpecific GameRef "aosIntelligenceChoose" you)
      : [targeting card (cardSpecific GameRef "aosIntelligenceSwap" (you,first,card)) | card <- cards,card /= first]
    pure g
  CardSpecific GameRef "aosIntelligenceSwap" (fromJSON -> Success (you,first :: EncounterCard,second :: EncounterCard)) -> do
    present <- asYou you getPlayers
    key <- getDeckKeyFor you
    let swap card | card == first = second | card == second = first | otherwise = card
        players = HashMap.mapWithKey (\ident (PlayerIdentity a) -> PlayerIdentity $ if ident `elem` present then a {playerIdentityEncounterCards = map swap (playerIdentityEncounterCards a)} else a) (gamePlayers g)
        scenario = case gameScenario of Scenario inner -> Scenario $ overScenarioAttrs (\a -> a & pileL key %~ \pile -> pile {pileDeck = map swap (take 1 $ pileDeck pile) <> drop 1 (pileDeck pile)}) inner
    cardSpecific GameRef "aosIntelligence" you
    pure $ g {gameScenario = scenario} & entitiesL . playersL .~ players
  CardSpecific GameRef "aosIntelligenceDone" (fromJSON -> Success you) -> pure g {gamePrivateCards = HashMap.delete you gamePrivateCards}
  CardSpecific GameRef "beginFrameworkDiscard" _ -> pure g {gameResolvingAbilitySubTypes = (GameRef, [], False) : gameResolvingAbilitySubTypes}
  CardSpecific GameRef "endFrameworkDiscard" _ -> do
    let pop [] = []
        pop ((GameRef,_,_):rest) = rest
        pop (entry:rest) = entry : pop rest
    pure g {gameResolvingAbilitySubTypes = pop gameResolvingAbilitySubTypes}
  CardSpecific GameRef "setShieldLog" (fromJSON -> Success shieldLog) -> pure g {gameShieldLog = shieldLog, gameCampaign = fmap (Campaign.recordInLog $ Campaign.RecordShieldLog shieldLog) gameCampaign}
  RecordInCampaignLog r -> pure $ g & campaignL %~ fmap (Campaign.recordInLog r)
  -- a campaign game won is first written in the log by the scenario's Victory steps
  GameOver Won | Just c <- gameCampaign, Campaign.currentScenario c == Just (toId gameScenario) -> do
    clearQueue
    campaignStep CampaignVictory
    gameOver Won
    pure g
  GameOver Lost | Just c <- gameCampaign,c.kind == Campaign.FearNoEvilCampaign,isJust c.campaignLog.fearNoEvil.selectedScenario -> do
    clearQueue
    campaignStep $ CampaignSetupThen "fearNoEvil:defeat"
    gameOver Lost
    pure g
  -- an expert campaign's last scenario lost: Red Skull conquers the world
  GameOver Lost
    | Just c <- gameCampaign
    , Campaign.isExpertCampaign c
    , c.kind `notElem` [Campaign.AgeOfApocalypseCampaign,Campaign.ShadowlandCampaign,Campaign.FearNoEvilCampaign]
    , Campaign.scenarioNumber c == length c.scenarios -> do
        clearQueue
        pure $ g & stateL .~ Finished Lost & campaignL .~ Just c {Campaign.lost = True}
  PutObligationIntoPlay ident card -> do
    let obligation = lookupObligation (toCardCode card) (ObligationId $ unCardId $ pcCardId card)
    -- it is the player's, in their play area: told as an obligation revealed is
    obligationStep (toId obligation) (RevealObligation ident)
    pure $ g & entitiesL . obligationsL %~ insertMap (toId obligation) obligation
  -- setup, as the rules reference's appendix II has it: a first player, the identities'
  -- obligations and nemesis sets set aside, the scenario, then everyone's hand
  StartGame -> do
    when (null gamePlayerOrder) $ throwM NoPlayers
    for_ gamePlayerOrder (`identityStep` SetupIdentity)
    cardSpecific GameRef "setupLinkedCards" ()
    -- the choices the last victory left are cards for the decks, so first
    when (isJust gameCampaign) $ campaignStep CampaignChoices
    for_ gamePlayerOrder (`identityStep` PutSetupCardsIntoPlay)
    let chooseFirstPlayer = case gamePlayerOrder of
          players@(p : _ : _) ->
            chooseOnePrompted p "$label.chooseFirstPlayer" $
              [ targeting first (setPlayerOrder (after <> before))
              | (n, first) <- zip [0 ..] players
              , let (before, after) = splitAt n players
              ]
          _ -> pure ()
    -- a campaign game's first player is chosen before the scenario is set up (RR
    -- Appendix II, step 3): its Campaign Instructions put cards into play for them (The
    -- Mad Titan's Shadow's Cosmo and Black Swan)
    when (isJust gameCampaign) chooseFirstPlayer
    for_ gameCompetition $ \c -> for_ [CW.Registration,CW.Resistance] $ \team -> do
      let members = filter (`elem` CW.teamPlayers c team) gamePlayerOrder
          prompt = if team == CW.Registration then "$label.cwChooseRegistrationFirstPlayer" else "$label.cwChooseResistanceFirstPlayer"
      when (length members > 1) $ chooseOnePrompted (CW.teamFirstPlayer c team) prompt
        [targeting member (cardSpecific GameRef "cwSetFirstPlayer" (team,member)) | member <- members]
    startScenario
    when (isJust gameCampaign) $ campaignStep CampaignSetup
    cardSpecific GameRef "setupPoolAspect" ()
    setupPlayers
    unless (isJust gameCampaign || isJust gameCompetition) chooseFirstPlayer
    -- what the Campaign Instructions do with the starting hands
    when (isJust gameCampaign) $ campaignStep (CampaignSetupThen "startingHands")
    pure $ g {gameState = InProgress}
  CardSpecific GameRef "cwAdvancePhase" _ | Just c <- gameCompetition -> do
    let phase = CW.competitivePhase c
        registration = CW.teamOutcome c CW.Registration
        resistance = CW.teamOutcome c CW.Resistance
        result = case (registration,resistance) of
          (Just CW.EnemyLeaderDefeated,Just CW.EnemyLeaderDefeated) -> Just CompetitiveDraw
          (Just CW.EnemyLeaderDefeated,_) -> Just RegistrationWon
          (_,Just CW.EnemyLeaderDefeated) -> Just ResistanceWon
          (Just _,Just _) -> Just BothTeamsLost
          (Just _,Nothing) -> Just ResistanceWon
          (Nothing,Just _) -> Just RegistrationWon
          _ -> Nothing
        equalPhases = phase `elem` [CW.ResistancePlayerPhase,CW.ResistanceVillainPhase]
    case if equalPhases then result else Nothing of
      Just finished -> gameOver finished >> pure g
      Nothing -> do
        let next = fromMaybe CW.RegistrationPlayerPhase (CW.nextTeamPhase phase)
            first = CW.teamFirstPlayer c (CW.phaseTeam next)
        if isNothing (CW.nextTeamPhase phase) then endRound else beginPhase (CW.ordinaryPhase next)
        pure g {gameCompetition = Just c {CW.competitivePhase = next},gameActivePlayer = first}
  CardSpecific GameRef "flipSideSchemeToObligation" (fromJSON -> Success (sid,you,code)) -> case lookup sid (gameSideSchemes g) of
    Nothing -> pure g
    Just scheme -> do
      frameworkDiscard $ do
        for_ [uid | (uid,u) <- HashMap.toList (gameUpgrades g),upgradeAttachedSideScheme (toAttrs u) == Just sid] removeFromPlay
        for_ [aid | (aid,a) <- HashMap.toList (gameAttachments g),attachmentSideScheme (toAttrs a) == Just sid] removeFromPlay
      for_ (SideScheme.sideSchemeHeldCards $ toAttrs scheme) discardCard
      for_ (SideScheme.sideSchemeHeldEncounterCards $ toAttrs scheme) discardCard
      let oid = ObligationId $ unSideSchemeId sid
          obligation = lookupObligation code oid
      obligationStep oid (ObligationOwnedBy you)
      pure $ g
        & entitiesL . sideSchemesL %~ deleteMap sid
        & entitiesL . obligationsL %~ insertMap oid obligation
        & areasL . each %~ (\area -> area {areaSideSchemes = filter (/= sid) (areaSideSchemes area)})
  CardSpecific GameRef "cwBeginLeaderRewards" (fromJSON -> Success (you :: IdentityId)) -> do
    case gameCompetition >>= \c -> (c,) <$> CW.playerTeam c you of
      Nothing -> pure ()
      Just (c,team) -> do
        let codes = CW.leaderBasicCards $ CW.teamScenarioLeader $ CW.scenarioForTeam (CW.competitiveSetup c) team
        players <- filter (`elem` CW.teamPlayers c team) <$> getAllPlayers
        cardSpecific GameRef "cwChooseRewardPlayer" (CW.teamFirstPlayer c team,codes,players)
    pure g
  CardSpecific GameRef "cwChooseRewardPlayer" (fromJSON -> Success (first :: IdentityId,codes :: [CardCode],players :: [IdentityId])) -> do
    live <- getAllPlayers
    cards <- filter ((`elem` codes) . toCardCode) <$> getSetAsidePlayerCards
    let remaining = filter (`elem` live) players
        begin you = cardSpecific GameRef "cwLeaderRewards" (first,codes,you : filter (/= you) remaining,2 :: Natural)
    unless (null cards) $ case remaining of
      [] -> pure ()
      [you] -> begin you
      _ -> chooseOnePrompted first "$label.cwChooseRewardPlayer" [targeting you (begin you) | you <- remaining]
    pure g
  CardSpecific GameRef "cwLeaderRewards" (fromJSON -> Success (first :: IdentityId,codes :: [CardCode],players :: [IdentityId],remaining :: Natural)) -> do
    case players of
      [] -> pure ()
      you : rest | remaining == 0 -> cardSpecific GameRef "cwChooseRewardPlayer" (first,codes,rest)
                 | otherwise -> do
          cards <- filter ((`elem` codes) . toCardCode) <$> getSetAsidePlayerCards
          if null cards then cardSpecific GameRef "cwChooseRewardPlayer" (first,codes,rest)
          else chooseOnePrompted you "$label.cwChooseLeaderReward"
            [cardLabeled (toCardCode card) $ do
              removeFromGame $ CardIdRef $ pcCardId card
              takeControlOf you card
              cardSpecific GameRef "cwLeaderRewards" (first,codes,players,remaining - 1)
            | card <- cards]
    pure g
  CardSpecific GameRef "cwFinishBoard" (fromJSON -> Success (team,outcome)) | Just c <- gameCompetition ->
    pure g {gameCompetition = Just $ CW.recordTeamOutcome team outcome c}
  CardSpecific GameRef "cwSetFirstPlayer" (fromJSON -> Success (team,you)) | Just c <- gameCompetition -> do
    let own = filter (`elem` CW.teamPlayers c team) gamePlayerOrder
        rotated = case break (== you) own of (before,after@(_:_)) -> after <> before; _ -> own
        orderFor side = if side == team then rotated else filter (`elem` CW.teamPlayers c side) gamePlayerOrder
    if you `notElem` own then pure g else pure g
      {gameCompetition = Just $ CW.setTeamFirstPlayer team you c
      ,gamePlayerOrder = orderFor CW.Registration <> orderFor CW.Resistance
      ,gameActivePlayer = if team == CW.Registration then you else gameActivePlayer}
  CardSpecific GameRef "aoaRevealFoundEncounter" (fromJSON -> Success (you, card)) -> do
    let scenario = toAttrs (g.gameScenario)
        source | card `elem` scenarioDiscard scenario = W.RevealedFromDiscardPile
               | EncounterCard card `elem` scenarioSetAsideCards scenario = W.RevealedFromSetAside
               | otherwise = W.RevealedFromEncounterDeck
    removeFromGame card
    revealEncounterCardFrom source you card
    pure g
  CardSpecific GameRef "aoaRedirectEngagementWindow" (fromJSON -> Success (mid, you)) ->
    pure g {gameWindows = map (map (\case
      W.Window timing (W.EngagedMinion _ other) | other == mid -> W.Window timing (W.EngagedMinion you mid)
      window -> window)) (g.gameWindows)}
  CardSpecific GameRef "chooseFearNoEvilScenario" (fromJSON -> Success code) -> case g.gameCampaign of
    Just campaign | campaign.kind == Campaign.FearNoEvilCampaign,code `elem` (if null (Campaign.fearNoEvilUnresolved campaign) then ["60161a"] else Campaign.fearNoEvilUnresolved campaign) ->
      case recommendedSetup code >>= \setup -> lookupScenario setup {difficulty=scenarioDifficulty $ toAttrs $ g.gameScenario} of
        Nothing -> pure g
        Just selected -> pure g {gameScenario=selected,gameCampaign=Just $ Campaign.recordInLog (Campaign.RecordFearNoEvilScenario code) campaign}
    _ -> pure g
  CardSpecific GameRef "blackCatCheckAlly" (fromJSON -> Success (aid :: AllyId)) -> do
    threat <- getCharacterThreat $ AllyCharacter aid
    when (threat /= 4) $ restoreControlledMinion aid
    pure g
  CardSpecific GameRef "beginFriendlyBoost" (fromJSON -> Success (aid,attacking,original :: Message)) -> do
    let context = FriendlyBoostContext aid attacking [] [] 0 []
    let enemy = EnemyMinionId $ MinionId $ unAllyId aid
        owner = maybe (g.gameActivePlayer) getAllyController $ lookup aid $ gameAllies g
    checkWindows [W.Window W.When $ W.EnemyActivation enemy owner]
    dealBoost $ MinionId $ unAllyId aid
    cardSpecific GameRef "flipFriendlyBoosts" (aid,original)
    cardSpecific GameRef "finishFriendlyBoost" aid
    checkWindows [W.Window W.After $ W.EnemyActivation enemy owner]
    pure g {gameFriendlyBoosts=context:g.gameFriendlyBoosts}
  MinionMessage mid (MinionDealtBoost card) | Just aid <- friendlyProxyAlly g mid ->
    pure $ modifyFriendlyBoost aid (\context -> context {friendlyBoostPending=friendlyBoostPending context <> [card]}) g
  MinionMessage mid (MinionAddBoost n) | Just aid <- friendlyProxyAlly g mid ->
    pure $ modifyFriendlyBoost aid (\context -> context {friendlyBoostIcons=friendlyBoostIcons context+n}) g
  MinionMessage mid (MinionAttackGains keyword) | Just aid <- friendlyProxyAlly g mid ->
    pure $ modifyFriendlyBoost aid (\context -> context {friendlyBoostKeywords=nub $ keyword:friendlyBoostKeywords context}) g
  WithBoostIcons (MinionRef mid) _ counts | isJust (friendlyProxyAlly g mid) -> do
    minionStep mid (MinionAddBoost $ sum $ map snd counts)
    pure g
  CardSpecific GameRef "flipFriendlyBoosts" (fromJSON -> Success (aid,original :: Message)) -> do
    let cards = maybe [] friendlyBoostPending $ find ((== aid) . friendlyBoostAlly) (g.gameFriendlyBoosts)
        mid = MinionId $ unAllyId aid
    for_ cards $ \card -> revealBoostCard card mid >> countActivationBoostIcons mid [card]
    if null cards then cardSpecific GameRef "friendlyPowerReady" (aid,original) else cardSpecific GameRef "flipFriendlyBoosts" (aid,original)
    pure $ modifyFriendlyBoost aid (\context -> context {friendlyBoostPending=[],friendlyBoostRevealed=friendlyBoostRevealed context <> cards}) g
  CardSpecific GameRef "friendlyPowerReady" (fromJSON -> Success (aid,original :: Message)) -> do
    next <- if HashMap.member aid (gameAllies g) then runReaderT (dispatchGameMessage original g) (refreshTraits g) else pure g
    pure next
  CardSpecific GameRef "finishFriendlyBoost" (fromJSON -> Success (aid :: AllyId)) -> do
    for_ (find ((== aid) . friendlyBoostAlly) (g.gameFriendlyBoosts)) $ \context ->
      for_ (friendlyBoostRevealed context <> friendlyBoostPending context) $ \card -> whenM (isCurrentBoostCard card) $ consumeBoostCard card >> discardCard card
    pure g {gameFriendlyBoosts=filter ((/= aid) . friendlyBoostAlly) $ g.gameFriendlyBoosts}
  CardSpecific GameRef "racketChooseBusinesses" (fromJSON -> Success (players :: [IdentityId])) -> do
    case players of
      [] -> pure ()
      you:rest -> do
        let available = [card | EncounterCard card <- scenarioSetAsideCards (toAttrs $ g.gameScenario),toCardCode card `elem` ["60134b","60135b","60136b","60137b","60138b"]]
            install card = cardSpecific GameRef "racketInstallBusiness" (you,card,Nothing :: Maybe MainSchemeId,0 :: Natural) >> cardSpecific GameRef "racketChooseBusinesses" rest
        if scenarioDifficulty (toAttrs $ g.gameScenario) == Expert
          then shuffleM available >>= traverse_ install . headMay
          else chooseOne you [cardLabeled (toCardCode card) (install card) | card <- available]
    pure g
  CardSpecific GameRef "racketSwapBusiness" (fromJSON -> Success (you :: IdentityId)) -> do
    old <- getOwnedMainScheme you
    let available = [card | EncounterCard card <- scenarioSetAsideCards (toAttrs $ g.gameScenario),toCardCode card `elem` ["60134b","60135b","60136b","60137b","60138b"]]
    shuffled <- shuffleM available
    for_ old $ \sid -> for_ (headMay shuffled) $ \card ->
      cardSpecific GameRef "racketInstallBusiness" (you,card,Just sid,maybe 0 getMainSchemeThreat $ lookup sid $ gameMainSchemes g)
    pure g
  CardSpecific GameRef "racketInstallBusiness" (fromJSON -> Success (you,card,old :: Maybe MainSchemeId,threat :: Natural)) -> do
    let sid = MainSchemeId $ unCardId $ ecCardId card
        gone = case old of Just mid -> [mid]; Nothing -> [mid | (mid,scheme) <- HashMap.toList (gameMainSchemes g),isNothing $ mainSchemeOwner $ toAttrs scheme]
    case lookupMainScheme (toCardCode card) sid of
      Nothing -> pure g
      Just scheme -> do
        removeEncounterCardFromPiles card
        for_ gone $ \mid -> do
          for_ (lookup mid $ gameMainSchemes g) $ \previous -> do
            when (isJust $ mainSchemeOwner $ toAttrs previous) $ setAside [toCard previous]
            traverse_ discardCard $ MS.mainSchemeHeldCards $ toAttrs previous
            traverse_ discardCard $ MS.mainSchemeHeldEncounterCards $ toAttrs previous
          frameworkDiscard $ do
            for_ [uid | (uid,u) <- HashMap.toList (gameUpgrades g),upgradeAttachedMainScheme (toAttrs u) == Just mid] removeFromPlay
            for_ [aid | (aid,a) <- HashMap.toList (gameAttachments g),Attachment.attachmentMainScheme (toAttrs a) == Just mid] removeFromPlay
        owned <- runMessage (CardSpecific (MainSchemeRef sid) "ownedMainState" $ toJSON (you,threat)) scheme
        cardSpecific sid "racketDifficulty" (scenarioDifficulty (toAttrs $ g.gameScenario) == Expert)
        pure $ g & entitiesL . mainSchemesL %~ (insertMap sid owned . (\schemes -> foldr deleteMap schemes gone))
          & areasL . each %~ (\area -> area {areaMainSchemes=filter (`notElem` gone) $ areaMainSchemes area})
          & intoAreaOf you (\area -> area {areaMainSchemes=areaMainSchemes area <> [sid]})
  CardSpecific GameRef "replaceDealtEncounterCard" (fromJSON -> Success (you,card)) -> case lookup you (gamePlayers g) of
    Just player | card `elem` playerIdentityEncounterCards (toAttrs player) -> do
      discardCard card
      dealEncounterCard you
      pure $ g & entitiesL . playersL . ix you %~ (\(PlayerIdentity a) -> PlayerIdentity a {playerIdentityEncounterCards=filter ((/= ecCardId card) . ecCardId) $ playerIdentityEncounterCards a})
    _ -> pure g
  CardSpecific GameRef "flipMaryWalker" (fromJSON -> Success (aid,you,reveal :: Bool)) -> case lookup aid (gameAttachments g) of
    Nothing -> pure g
    Just old | toCardCode (getCardDef old) == "60113b" -> do
      let sid = SideSchemeId $ unAttachmentId aid
          def = fromMaybe (error "missing Establish Trust") $ HashMap.lookup "60113a" MGEncounter.allEncounterCards
          card = MkEncounterCard (CardId $ unAttachmentId aid) def
          scheme = createSideScheme card
      frameworkDiscard $ do
        for_ [uid | (uid,u) <- HashMap.toList (gameUpgrades g)
          , Upgrade.upgradeAttachedCard (toAttrs u) == Just (AttachmentRef aid)
            || upgradeAttachedEnemy (toAttrs u) == Just (EnemyAttachmentId aid)] removeFromPlay
        for_ [child | (child,a) <- HashMap.toList (gameAttachments g)
          , Attachment.attachmentEnemy (toAttrs a) == Just (EnemyAttachmentId aid)] removeFromPlay
      -- Turning the physical card over enters its other type without revealing it.
      sideSchemeStep sid SideSchemePlaceInitialThreat
      checkWindows [W.Window W.After $ W.SideSchemeEnteredPlay sid]
      when reveal $ sideSchemeStep sid RevealSideScheme
      attachmentRemoved aid
      pure $ g & entitiesL . attachmentsL %~ deleteMap aid
        & entitiesL . sideSchemesL %~ insertMap sid scheme
        & intoAreaOf you (\area -> area {areaSideSchemes=areaSideSchemes area <> [sid]})
    _ -> pure g
  CardSpecific GameRef "aoaTransformSchemeToAttachment" (fromJSON -> Success (sid, code, you)) -> do
    let def = fromMaybe (error "missing transformed attachment") (HashMap.lookup code MGEncounter.allEncounterCards)
        card = MkEncounterCard (CardId $ unSideSchemeId sid) def
    frameworkDiscard $ do
      for_ (lookup sid $ gameSideSchemes g) $ \old -> do
        traverse_ discardCard $ SideScheme.sideSchemeHeldCards $ toAttrs old
        traverse_ discardCard $ SideScheme.sideSchemeHeldEncounterCards $ toAttrs old
      for_ [uid | (uid,u) <- HashMap.toList (gameUpgrades g)
        , upgradeAttachedSideScheme (toAttrs u) == Just sid
          || Upgrade.upgradeAttachedCard (toAttrs u) == Just (SideSchemeRef sid)] removeFromPlay
      for_ [aid | (aid,a) <- HashMap.toList (gameAttachments g)
        , Attachment.attachmentSideScheme (toAttrs a) == Just sid] removeFromPlay
    putEncounterCardIntoPlay you card
    pure $ g & entitiesL . sideSchemesL %~ deleteMap sid
      & areasL . each %~ (\area -> area {areaSideSchemes = filter (/= sid) (areaSideSchemes area)})
  CardSpecific GameRef "aoaPaySettingSpecial" (fromJSON -> Success (you, acid)) -> do
    possible <- canResolveSettingSpecial you
    if possible then AoaSettings.resolveSettingSpecial you else abortPayment acid
    pure g
  CardSpecific GameRef "aoaOptionalCampaignCard" (fromJSON -> Success (you, code)) -> do
    let cards = take 1 [card | Just player <- [lookup you $ gamePlayers g], card <- unDeck $ playerIdentityDeck $ toAttrs player, sameCard (getCardDef card) (MGPlayer.lookupPlayerCard code)]
    for_ cards $ \card -> chooseOnePromptedAbout you "$label.aoaCampaignCardInclusion" card
      [labeled "$label.aoaIncludeCampaignCard" (pure ()), labeled "$label.aoaExcludeCampaignCard" (removeFromGame card)]
    pure g
  CardSpecific GameRef "aoaStartingAlly" (fromJSON -> Success (you, expert)) -> g <$ AoaMission.startingAlly you expert
  CardSpecific GameRef key value | "aoaMission" `T.isPrefixOf` key -> g <$ AoaMission.runMissionStep key value
  PutSetupEncounterCardsIntoPlay -> do
    catalogue <- getPlayerCardCatalogue
    let linked = [def | def <- HashMap.elems catalogue, any (\case Linked _ -> True; _ -> False) (cdKeywords def)]
    let parents = map (toCardCode . getCardDef) $ scenarioEncounterDeck $ toAttrs $ g.gameScenario
    for_ [def | def <- linked, any (\case Linked parent -> parent `elem` parents; _ -> False) (cdKeywords def)] \def -> do
      cid <- getRandom
      setAside [PlayerCard $ MkPlayerCard cid def Nothing Nothing]
    pure g
  CardSpecific GameRef "setupLinkedCards" _ -> do
    -- A separate physical linked set belongs to each deck containing its parent.
    -- Linked cards have no owner until a player takes control of them.
    catalogue <- getPlayerCardCatalogue
    let linked = nub [def | def <- HashMap.elems catalogue, any (\case Linked _ -> True; _ -> False) (cdKeywords def)]
    for_ (HashMap.elems $ gamePlayers g) \player -> do
      let deck = unDeck $ playerIdentityDeck $ toAttrs player
          parents = nub [canonicalCardCode $ getCardDef card | card <- deck]
      for_ [def | def <- linked, any (\case Linked parent -> parent `elem` parents; _ -> False) (cdKeywords def)] \def -> do
        cid <- getRandom
        setAside [PlayerCard $ MkPlayerCard cid def Nothing Nothing]
    pure g
  CardSpecific GameRef "enemyAttacksWithCompletion" (fromJSON -> Success (enemy, who, source)) -> do
    nonce <- getRandom
    prior <- getEnemyAttack enemy
    enemyAttacks enemy who
    cardSpecific GameRef "finishEnemyAttackCompletion" nonce
    pure g {gameTrackedEnemyAttacks = TrackedEnemyAttack nonce enemy source prior Nothing : gameTrackedEnemyAttacks}
  CardSpecific GameRef "finishEnemyAttackCompletion" (fromJSON -> Success nonce) -> do
    case find ((== nonce) . trackedAttackId) gameTrackedEnemyAttacks of
      Nothing -> pure g
      Just tracked -> do
        cardSpecific (trackedAttackSource tracked) "attackCompleted" (trackedAttackResult tracked)
        let without = g {gameTrackedEnemyAttacks = filter ((/= nonce) . trackedAttackId) gameTrackedEnemyAttacks}
        pure $ restoreEnemyAttack (trackedAttackEnemy tracked) (trackedAttackPrevious tracked) without
  CardSpecific GameRef "enemyAttacksWithCompletionId" (fromJSON -> Success (nonce,enemy,who,source)) -> do
    prior <- getEnemyAttack enemy
    enemyAttacks enemy who
    cardSpecific GameRef "finishEnemyAttackCompletionId" nonce
    pure g {gameTrackedEnemyAttacks = TrackedEnemyAttack nonce enemy source prior Nothing : gameTrackedEnemyAttacks}
  CardSpecific GameRef "finishEnemyAttackCompletionId" (fromJSON -> Success nonce) -> do
    case find ((== nonce) . trackedAttackId) gameTrackedEnemyAttacks of
      Nothing -> pure g
      Just tracked -> do
        cardSpecific (trackedAttackSource tracked) "attackCompletedWithId" (nonce,trackedAttackResult tracked)
        let without = g {gameTrackedEnemyAttacks = filter ((/= nonce) . trackedAttackId) gameTrackedEnemyAttacks}
        pure $ restoreEnemyAttack (trackedAttackEnemy tracked) (trackedAttackPrevious tracked) without
  CardSpecific GameRef "moveThreatToUpgradeSlot" (fromJSON -> Success (uid, scheme, slot :: Int)) -> do
    valid <- upgradeSlotIsEmpty uid slot
    removable <- canMoveSchemeThreatFrom uid scheme
    amount <- gameSelectCountScheme SchemeThreat (SchemeWithId scheme)
    when (valid && removable && amount > 0) do
      redirected <- elem RemovedThreatPlacedOnMinion <$> getModifiers (schemeRef scheme)
      case scheme of
        SchemeMainSchemeId sid -> mainSchemeStep sid (MainSchemeThreatRemoved (UpgradeRef uid) 1)
        SchemeSideSchemeId sid -> sideSchemeStep sid (SideSchemeThreatRemoved (UpgradeRef uid) 1)
      unless redirected $ cardSpecific uid "occupyResourceSlot" slot
    pure g
  CardSpecific GameRef "moveDamageToUpgradeSlot" (fromJSON -> Success (uid, character, slot :: Int)) -> do
    valid <- upgradeSlotIsEmpty uid slot
    available <- characterStoredDamage character
    healable <- characterMatches CharacterCanBeHealed character
    when (valid && available > 0 && healable) do
      heal character 1
      cardSpecific uid "occupyResourceSlot" slot
    pure g
  CardSpecific GameRef "searchCollectionWeapon" (fromJSON -> Success (you, host)) -> do
    catalogue <- getPlayerCardCatalogue
    let defs = nubBy (\a b -> canonicalCardCode a == canonicalCardCode b) [def | def <- HashMap.elems catalogue, cdCardType def == UpgradeType,
                     Weapon `member` cdTraits def, maybe False (/= Neutral) (cdAspect def)]
    chooseOne you [cardLabeled (toCardCode def) $ cardSpecific GameRef "takeCollectionWeapon" (you, host :: UpgradeId, toCardCode def) | def <- defs]
    pure g
  CardSpecific GameRef "takeCollectionWeapon" (fromJSON -> Success (you, host, code)) -> do
    catalogue <- getPlayerCardCatalogue
    for_ (lookup code catalogue) $ \def -> do
      cid <- getRandom
      let card = MkPlayerCard cid def (Just you) (Just you)
      cardSpecific (host :: UpgradeId) "holdFacedownCard" card
    pure g
  CardSpecific GameRef "swapHeldWeapon" (fromJSON -> Success (you, host, old, card)) -> do
    legal <- getLegalHeldWeaponSwaps you host
    for_ [outgoing | (old, card) `elem` legal, Just outgoing <- [lookup old $ gameUpgrades g]] $ \outgoing -> do
      let a = toAttrs outgoing
          held = maybe [] (upgradeHeldCards . toAttrs) $ lookup host (gameUpgrades g)
      modifiers <- getModifiers outgoing
      titles <- getUniqueTitlesInPlay
      let info def = (title $ cdName def, maybe [] pure $ subtitle $ cdName def)
          remainingTitles = if cdUnique (getCardDef outgoing) then Data.List.delete (info $ getCardDef outgoing) titles else titles
          clash = cdUnique (getCardDef card) && any (uniqueMatch $ info $ getCardDef card) remainingTitles
          sameTitle = cdName (getCardDef outgoing) == cdName (getCardDef card)
          canLeave = sameTitle || all (`notElem` modifiers) [CannotLeavePlay, CannotLeavePlayByCardAbilities]
      when (card `elem` held && upgradeController a == you && Weapon `member` cdTraits (getCardDef outgoing) && canLeave && not clash) do
        targets <- if cdName (getCardDef outgoing) == cdName (getCardDef card) then pure Nothing else getPlayTargets you card
        case targets of
          Nothing -> cardSpecific GameRef "commitHeldWeaponSwap" (you, host, old, card, Nothing :: Maybe Target)
          Just choices -> chooseOne you [targeting target $ cardSpecific GameRef "commitHeldWeaponSwap" (you, host, old, card, Just target) | target <- choices]
    pure g
  CardSpecific GameRef "commitHeldWeaponSwap" (fromJSON -> Success (you, host, old, card, target :: Maybe Target)) -> do
    legal <- getLegalHeldWeaponSwaps you host
    case lookup old (gameUpgrades g) of
      Nothing -> pure g
      Just outgoing -> do
        let a = toAttrs outgoing
            held = maybe [] (upgradeHeldCards . toAttrs) $ lookup host (gameUpgrades g)
        if card `notElem` held || (old, card) `notElem` legal then pure g else do
          let original = case toCard outgoing of PlayerCard c -> c; _ -> error "weapon is not a player card"
          cardSpecific (host :: UpgradeId) "releaseHeldCard" card
          if cdName (getCardDef outgoing) == cdName (getCardDef card) then do
            let new = UpgradeId $ unCardId $ pcCardId card
                changed = case toJSON outgoing of
                  Object o -> Object $ KeyMap.insert "upgradeOwner" (toJSON $ fromMaybe you $ pcOwner card) $ KeyMap.insert "upgradeId" (toJSON new) $ KeyMap.insert "upgradeCardDef" (toJSON $ getCardDef card) o
                  value -> value
            case fromJSON changed of
              Error err -> error err
              Success incoming -> do
                upgradeRemoved old
                upgradeCreated you new
                for_ a.upgradeAttachedAlly (\aid -> upgradeAttachedTo aid new)
                for_ a.upgradeAttachedEnemy (\enemy -> upgradeAttachedTo enemy new)
                cardSpecific host "holdFacedownCard" original
                pure $ g & entitiesL . upgradesL %~ (insertMap new incoming . deleteMap old)
          else do
            removeFromGame old
            case target of
              Nothing -> putIntoPlay you card
              Just destination -> putIntoPlayOn you card destination
            when a.upgradeExhausted $ exhaust (UpgradeId $ unCardId $ pcCardId card)
            holdFacedownCard host original
            pure g
  CardSpecific GameRef "setupPoolAspect" _ -> do
    let groups = case gameCompetition of
          Nothing -> [gamePlayerOrder]
          Just competition -> map (CW.teamPlayers competition) [CW.Registration,CW.Resistance]
    for_ groups $ \members -> do
      let poolUsers = [player | you <- members,Just player <- [lookup you $ gamePlayers g],Marvel.Aspect.Pool `elem` playerIdentityChosenAspects (toAttrs player)]
      for_ (take 1 poolUsers) $ \player -> do
        key <- getDeckKeyFor $ toId player
        cards <- traverse (MGEncounter.genEncounterCard . (MGEncounter.allEncounterCards HashMap.!)) ["44037", "44038", "44039", "44040", "44041", "44041", "44042"]
        fromDeck key $ shuffleIntoEncounterDeck [card | card <- cards,toCardCode card == "44037"]
        fromDeck key $ setAside [EncounterCard card | card <- cards,toCardCode card /= "44037"]
    for_ (HashMap.elems $ gamePlayers g) \player -> do
      let a = toAttrs player
      when (isNothing a.playerIdentityPreviousGameWon && any ((== "44028") . toCardCode) (unDeck a.playerIdentityDeck)) $
        chooseOnePrompted (toId player) "$label.previousGameOutcome"
          [ labeled "$label.previousGameWon" $ cardSpecific player "previousGameWon" True
          , labeled "$label.previousGameNotWon" $ cardSpecific player "previousGameWon" False
          ]
    pure g
  SetupPlayers -> do
    for_ gamePlayerOrder \p -> do
      setActivePlayer p
      identityStep p DealStartingHand
    for_ (take 1 gamePlayerOrder) setActivePlayer
    beginPhase PlayerPhase
    pure g
  SetPlayerOrder xs -> pure $ g {gamePlayerOrder = xs, gameActivePlayer = fromMaybe gameActivePlayer (headMay xs)}
  -- a player eliminated after their turn was queued (damage at the end of a phase) is
  -- passed over
  SetActivePlayer ident
    | HashMap.member ident (gamePlayers g)
      || maybe False (\c -> ident `elem` [CW.teamFirstPlayer c CW.Registration,CW.teamFirstPlayer c CW.Resistance]) gameCompetition -> pure $ g & activePlayerL .~ ident
    | otherwise -> pure g
  BeginPhase phase -> do
    checkWindows [W.Window W.When $ W.PhaseBegan phase]
    checkWindows [W.Window W.After $ W.PhaseBegan phase]
    pure $ g & phaseL .~ phase & playedThisPhaseL .~ [] & heroAttackedThisPhaseL .~ [] & heroThwartedThisPhaseL .~ [] & treacheriesRevealedThisPhaseL .~ [] & usedAbilitiesL . each %~ filter (not . perPhase . baseLimit . abilityLimit . fst)
  AtEndOfPhase msgs -> pure $ g & atEndOfPhaseL <>~ msgs
  -- what was to happen at the end of the phase happens first, as the phase ends
  EndPhase ending -> do
    pushAll gameAtEndOfPhase
    checkWindows [W.Window W.When $ W.PhaseEnded ending]
    when (ending == PlayerPhase) $ for_ (g.gameBombCounters) \(enemy,source,n) -> do
      present <- selectAny (EnemyWithId enemy)
      when present $ dealDamage enemy source (toDamage (2*n) FromAbility)
    checkWindows [W.Window W.After $ W.PhaseEnded ending]
    pure $ modifyMissionInGame (\m -> m {missionDiscount = False}) $ (g & atEndOfPhaseL .~ []) {gameBombCounters = if ending == PlayerPhase then [] else g.gameBombCounters}
  -- the first player token goes to the next player, who takes the first turn of the round
  PassFirstPlayer -> do
    let rotate [] = []
        rotate (p : ps) = ps <> [p]
    case gameCompetition of
      Nothing -> do
        let order = rotate gamePlayerOrder
        pure $ g & playerOrderL .~ order & activePlayerL .~ fromMaybe gameActivePlayer (headMay order)
      Just c -> do
        let team = CW.phaseTeam $ CW.competitivePhase c
            own = filter (`elem` CW.teamPlayers c team) gamePlayerOrder
            rotated = rotate own
            orderFor side = if side == team then rotated else filter (`elem` CW.teamPlayers c side) gamePlayerOrder
            first = fromMaybe (CW.teamFirstPlayer c team) (headMay rotated)
        pure g {gamePlayerOrder = orderFor CW.Registration <> orderFor CW.Resistance
               ,gameActivePlayer = first, gameCompetition = Just $ CW.setTeamFirstPlayer team first c}
  -- faceup into The Collection: a card in play leaves play for it, by way of
  -- 'RemoveFromPlay' ('discardFromPlay' sends it on); a card out of play is taken from
  -- where it is by what holds it (a deck, a hand, a discard pile)
  PlaceInCollection card -> case inPlayRef g card of
    Just ref -> do
      removeFromPlay ref
      pure $ g & collectingL %~ (toCardId card :)
    Nothing ->
      pure $
        g
          & (collectingL %~ filter (/= toCardId card))
          & (collectionL %~ (<> [card]) . filter ((/= toCardId card) . toCardId))
  -- a card leaving The Collection for its owner's discard pile
  DiscardedCard card -> pure $ g & collectionL %~ filter ((/= toCardId card) . toCardId)
  -- out of play for the victory display, not discarded: what is attached to it is
  -- discarded as it would be (RR Victory X)
  AddToVictoryDisplay target -> case target of
    ObligationRef oid -> case lookup oid (gameObligations g) of
      Nothing -> pure g
      Just obligation -> do
        for_ (Obligation.obligationHeldEncounterCards $ toAttrs obligation) discardCard
        for_ (Obligation.obligationHeldCards $ toAttrs obligation) discardCard
        pure $ g & (entitiesL . obligationsL %~ deleteMap oid)
          & (removedEntitiesL . obligationsL %~ insertMap oid obligation)
          & (victoryDisplayL <>~ [ec | EncounterCard ec <- [toCard obligation]])
          & (playerVictoryDisplayL <>~ [pc | PlayerCard pc <- [toCard obligation]])
    UpgradeRef uid -> case lookup uid (gameUpgrades g) of
      Nothing -> pure g
      Just upgrade -> do
        upgradeRemoved uid
        pure $ g & (entitiesL . upgradesL %~ deleteMap uid)
          & (removedEntitiesL . upgradesL %~ insertMap uid upgrade)
          & (playerVictoryDisplayL <>~ [pc | PlayerCard pc <- [toCard upgrade]])
    MainSchemeRef mid -> case lookup mid (gameMainSchemes g) of
      Nothing -> pure g
      Just scheme -> whenCanLeavePlay g mid do
        frameworkDiscard $ for_ [uid | (uid, u) <- HashMap.toList (gameUpgrades g), upgradeAttachedMainScheme (toAttrs u) == Just mid] removeFromPlay
        for_ [aid | (aid, attachment) <- HashMap.toList (gameAttachments g), Attachment.attachmentMainScheme (toAttrs attachment) == Just mid] removeFromPlay
        pure $ g
          & (entitiesL . mainSchemesL %~ deleteMap mid)
          & (removedEntitiesL . mainSchemesL %~ insertMap mid scheme)
          & (areasL . each %~ \area -> area {areaMainSchemes = filter (/= mid) (areaMainSchemes area)})
          & (victoryDisplayL <>~ [ec | EncounterCard ec <- [toCard scheme]])
    EnvironmentRef eid -> case lookup eid (gameEnvironments g) of
      Nothing -> pure g
      Just environment -> pure $ g
        & (entitiesL . environmentsL %~ deleteMap eid)
        & (victoryDisplayL <>~ [ec | EncounterCard ec <- [toCard environment]])
        & (areasL . each %~ \a -> a {areaEnvironments = filter (/= eid) (areaEnvironments a)})
    AllyRef aid -> case lookup aid (gameAllies g) of
      Nothing -> pure g
      Just ally -> do
        transferMojoThreat (AllyCharacter aid)
        frameworkDiscard $ for_ (allyUpgrades $ toAttrs ally) defeatedLeavesPlay
        frameworkDiscard $ for_ [toId a | a <- HashMap.elems (gameAttachments g), Attachment.attachmentAlly (toAttrs a) == Just aid] removeFromPlay
        for_ (Ally.allyHeldCards $ toAttrs ally) discardCard
        for_ (Ally.allyHeldTreacheries $ toAttrs ally) discardCard
        identityStep (getAllyController ally) (AllyRemoved aid)
        let card = MkEncounterCard (CardId $ unAllyId aid) (getCardDef ally)
        pure $ g
          & (entitiesL . alliesL %~ deleteMap aid)
          & (removedEntitiesL . alliesL %~ insertMap aid ally)
          & (victoryDisplayL <>~ [card])
    MinionRef mid -> case lookup mid (gameMinions g) of
      Nothing -> pure g
      Just minion -> do
        transferMojoThreat (MinionCharacter mid)
        cancelEnemyActivation mid
        let a = toAttrs minion
        frameworkDiscard $ for_ (minionUpgrades a) removeFromPlay
        frameworkDiscard $ for_ (minionAttachments a) defeatedLeavesPlay
        for_ (Minion.minionHeldCards a) discardCard
        for_ (Minion.minionHeldTreacheries a) discardCard
        minionDisengages (minionEngagedIdentity a) mid
        pure $
          g
            & (entitiesL . minionsL %~ deleteMap mid)
            & (removedEntitiesL . minionsL %~ insertMap mid minion)
            & (victoryDisplayL <>~ [ec | EncounterCard ec <- [toCard minion]])
    SideSchemeRef sid -> case lookup sid (gameSideSchemes g) of
      Nothing -> pure g
      Just scheme -> whenCanLeavePlay g sid do
        for_ (SideScheme.sideSchemeHeldEncounterCards $ toAttrs scheme) discardCard
        frameworkDiscard $ for_ [uid | (uid, u) <- HashMap.toList (gameUpgrades g), upgradeAttachedSideScheme (toAttrs u) == Just sid] removeFromPlay
        for_ [aid | (aid, at) <- HashMap.toList (gameAttachments g), attachmentSideScheme (toAttrs at) == Just sid] removeFromPlay
        pure $
          g
            & (entitiesL . sideSchemesL %~ deleteMap sid)
            & (removedEntitiesL . sideSchemesL %~ insertMap sid scheme)
            & (areasL . each %~ \area -> area {areaSideSchemes = filter (/= sid) (areaSideSchemes area)})
            & (victoryDisplayL <>~ [ec | EncounterCard ec <- [toCard scheme]])
            & (playerVictoryDisplayL <>~ [pc | PlayerCard pc <- [toCard scheme]])
    AttachmentRef aid -> case lookup aid (gameAttachments g) of
      Nothing -> pure g
      Just at -> do
        attachmentRemoved aid
        pure $ g & (entitiesL . attachmentsL %~ deleteMap aid) & (victoryDisplayL <>~ [ec | EncounterCard ec <- [toCard at]])
    _ -> error $ "Cannot add to the victory display: " <> show target
  CardSpecific GameRef "shuffleMinionIntoEncounterDeck" (fromJSON -> Success mid) -> do
    for_ (cardInPlay g $ MinionRef mid) $ \case
      EncounterCard card -> do
        checkWindows [W.Window W.Would $ W.MinionLeavingPlay mid]
        push $ ResolveMinionLeavingPlay mid (CardSpecific GameRef "shuffleMinionAfterLeaveWindow" $ toJSON (mid, card))
      _ -> pure ()
    pure g
  CardSpecific GameRef "shuffleMinionAfterLeaveWindow" (fromJSON -> Success (mid, card)) -> do
    removeMinionFromGameWithoutLeaveWindow mid
    shuffleIntoEncounterDeck [card]
    pure g
  CardSpecific GameRef "minionActivatesAgainst" (fromJSON -> Success (mid, you)) -> do
    present <- selectAny (MinionWithId mid)
    when present do
      hero <- identityMatches HeroIdentity you
      if hero then enemyAttacks mid you else minionSchemesAgainst mid you
    pure g
  CardSpecific GameRef "enemyDefeatedIdentity" (fromJSON -> Success (ident, enemy)) -> do
    let defeated = maybe True (playerIdentityDefeated . toAttrs) $ lookup ident (gamePlayers g)
    when defeated $ checkWindows [W.Window W.After $ W.EnemyAttackDefeatedCharacter enemy (IdentityCharacter ident)]
    pure g
  CardSpecific GameRef "beginSimultaneousMinionAttacks" (fromJSON -> Success (mid, you, others)) -> do
    case lookup mid (gameMinions g) of
      Nothing -> pure g
      Just minion -> do
        let attrs = toAttrs minion
        nonce <- getRandom
        keywords <- Minion.getModifiedKeywords attrs
        base <- Minion.getModifiedAttackSigned attrs
        extras <- for others \other -> do
          attack' <- prepareEnemyAttack (EnemyMinionId mid) $ withAttackBase base $ attackWith keywords (EnemyMinionId mid) (IdentityCharacter other) 0
          pure (other, attack' {attackIsSimultaneousAdditional = True, attackSimultaneousGroupId = Just nonce}, [], 0, (0, False))
        case Minion.minionAttacking attrs of
          Nothing -> pure g
          Just attack' -> do
            cardSpecific GameRef "simultaneousMinionStep" ("announce" :: Text, 1 :: Int)
            let initial = attack' {attackSimultaneousGroupId = Just nonce}
            pure $ (g & entitiesL . minionsL . ix mid %~ overMinion (\a -> a {Minion.minionAttacking = Just initial}))
              {gameSimultaneousMinionAttack = Just $ SimultaneousMinionAttackGroup mid nonce
                ((you, initial, Minion.minionBoostCards attrs, Minion.minionBoost attrs, (0, False)) : extras) 0 [] gameSimultaneousMinionAttack}
  CardSpecific GameRef "simultaneousMinionStep" (fromJSON -> Success (stage, cursor)) ->
    runSimultaneousMinionStep stage cursor g
  CardSpecific GameRef "captureSimultaneousMinionStep" (fromJSON -> Success (stage, cursor)) -> do
    let saved = saveSimultaneousMinionAttack g
        withDefender = case saved.gameSimultaneousMinionAttack of
          Just group | stage == ("defend" :: Text), Just (_, attack', _, _, _) <- headMay $ drop cursor $ simultaneousAttacks group ->
            let defense = case attackCharacter attack' of
                  IdentityCharacter who | attackDefended attack', Just player <- lookup who (gamePlayers saved) ->
                    (playerIdentityDamageReduction $ toAttrs player, playerIdentityDefended $ toAttrs player)
                  _ -> (0, False)
                record n (who, attack0, boosts, bonus, oldDefense) = (who, attack0, boosts, bonus, if n == cursor then defense else oldDefense)
             in saved {gameSimultaneousMinionAttack = Just group
                { simultaneousDefenders = nub $ simultaneousDefenders group <> [attackCharacter attack' | attackDefended attack']
                , simultaneousAttacks = zipWith record [0 ..] (simultaneousAttacks group)}}
          _ -> saved
    cardSpecific GameRef "simultaneousMinionStep" (stage, cursor + (1 :: Int))
    pure withDefender
  CardSpecific GameRef "finishSimultaneousMinionAttacks" _ -> do
    for_ gameSimultaneousMinionAttack \group -> do
      for_ [0 .. length (simultaneousAttacks group) - 1] \cursor ->
        cardSpecific GameRef "afterSimultaneousMinionAttack" cursor
      cardSpecific GameRef "closeSimultaneousMinionAttacks" ()
    pure g
  CardSpecific GameRef "afterSimultaneousMinionAttack" (fromJSON -> Success cursor) -> do
    let selected = selectSimultaneousMinionAttack cursor g
    for_ (selected.gameSimultaneousMinionAttack) \group ->
      for_ (headMay $ drop cursor $ simultaneousAttacks group) \(you, attack', _, _, _) -> do
        controller <- getSourceController (refocus $ characterRef $ attackCharacter attack')
        checkWindows [W.Window W.After $ W.EnemyAttack (EnemyMinionId $ simultaneousMinion group) (fromMaybe you controller)]
    pure selected
  CardSpecific GameRef "closeSimultaneousMinionAttacks" _ -> do
    let selected = selectSimultaneousMinionAttack 0 g
    for_ gameSimultaneousMinionAttack \group ->
      afterMatching (== MinionMessage (simultaneousMinion group) MinionEndAttack) $
        cardSpecific GameRef "restorePreviousSimultaneousMinionAttacks" (simultaneousPrevious group)
    pure selected
  CardSpecific GameRef "restorePreviousSimultaneousMinionAttacks" (fromJSON -> Success previous) ->
    pure $ restoreSimultaneousMinionAttack previous g
  PrepareDividedThreat ident source upTo n matcher -> do
    sourceModifiers <- getModifiers source
    let bonus = if ThreatRemovalBonusInTotal `elem` sourceModifiers
          then sum [k | ThreatRemovedModifier k <- sourceModifiers]
          else 0
    push $ AssignDividedThreat ident source False upTo (n + bonus) matcher []
    pure g
  ResolveThreatRemoval basic thwarting ident target source n -> do
    msgs <- asYou ident $ resolveThreatRemoval basic thwarting target source n
    traverse_ push msgs
    pure g
  DefeatedLeavesPlay target -> do
    let def = case target of
          MinionRef k -> getCardDef <$> lookup k (gameMinions g)
          SideSchemeRef k -> getCardDef <$> lookup k (gameSideSchemes g)
          AttachmentRef k -> getCardDef <$> lookup k (gameAttachments g)
          UpgradeRef k -> getCardDef <$> lookup k (gameUpgrades g)
          _ -> Nothing
        victory = maybe False (any isVictory . cdKeywords) def
        isVictory = \case
          Victory _ -> True
          _ -> False
    let departure = case target of
          MinionRef mid | Just enemy <- Data.List.lookup mid gameDefeatedMinionBoosts ->
            CardSpecific GameRef "moveDefeatedMinionToBoost" (toJSON (mid,enemy))
          _ | victory -> AddToVictoryDisplay target
          MinionRef mid -> CardSpecific GameRef "removeDefeatedMinion" (toJSON mid)
          _ -> RemoveFromPlay target
    -- The defeated minion already answered its would-leave window before cleanup.
    push $ case target of
      MinionRef mid -> ResolveMinionLeavingPlay mid departure
      _ -> departure
    pure g
  SetPeril who -> pure $ g & perilL .~ who
  -- each card's icons and one more for each amplify icon in play, counted as
  -- 'CountBoostIcons' counts them
  CountActivationBoostIcons target cards -> do
    amplify <- getAmplifyCount
    global <- getModifiers GameRef
    own <- if target == GameRef then pure [] else getModifiers target
    let extra = sum [n | AdditionalActivationBoostIcons n <- global <> own]
    counts <- for cards \card -> (card,) . (+ (amplify + extra)) <$> getBoostIcons card
    for_ cards \card -> checkWindows [W.Window When $ W.CountingBoostIcons card]
    push $ WithBoostIcons target (toJSON ()) counts
    pure g
  CardSpecific GameRef "enemyCharacterWasAttacked" (fromJSON -> Success attack) -> do
    let character = attackCharacter attack
    damageCharacter character (attackSource attack) (attackDamageTo attack $ attackDamageDealt attack)
    cardSpecific GameRef "enemyCharacterAttackRetaliation" attack
    pure g
  CardSpecific GameRef "enemyCharacterAttackRetaliation" (fromJSON -> Success attack) -> do
    case attackCharacter attack of
      MinionCharacter mid -> for_ (lookup mid $ gameMinions g) $ \_ -> do
        attackerModifiers <- getModifiers (attackSource attack)
        unless (attackRanged attack || AttacksIgnoreRetaliate `elem` attackerModifiers) $
          minionStep mid $ MinionRetaliates (toCharacter $ attackEnemy attack)
      VillainCharacter vid -> for_ (lookup vid $ gameVillains g) $ \_ -> do
        attackerModifiers <- getModifiers (attackSource attack)
        unless (attackRanged attack || AttacksIgnoreRetaliate `elem` attackerModifiers) $
          villainStep vid $ VillainRetaliates (toCharacter $ attackEnemy attack)
      _ -> pure ()
    pure g
  CardSpecific GameRef "redirectVillainAttackToCharacter" (fromJSON -> Success (vid, character)) -> do
    let redirect attrs = attrs
          { Villain.villainAttackTarget = Just character
          , Villain.villainAttacking = (\attack -> attack {attackCharacter = character, attackDefended = False, attackTarget = Nothing}) <$> Villain.villainAttacking attrs
          }
    pure $ g & entitiesL . villainsL . ix vid %~ (\(Villain.Villain v) -> Villain.Villain $ Villain.overVillainAttrs redirect v)
  CardSpecific GameRef "redirectVillainAttackToAlly" (fromJSON -> Success (vid, aid)) -> do
    let redirect attrs = attrs
          { Villain.villainAttackTarget = Just (AllyCharacter aid)
          , Villain.villainAttacking = (\attack -> attack {attackCharacter = AllyCharacter aid, attackDefended = False, attackTarget = Nothing}) <$> Villain.villainAttacking attrs
          }
    pure $ g & entitiesL . villainsL . ix vid %~ (\(Villain.Villain v) -> Villain.Villain $ Villain.overVillainAttrs redirect v)
  RemovePlayerCardFromHeldZones cid -> pure g
    {gameCollection = filter ((/= cid) . toCardId) (g.gameCollection)}
  RemoveFromGame target -> case target of
    TreacheryRef tid -> pure $ g & (entitiesL . treacheriesL %~ deleteMap tid)
    ObligationRef oid -> do
      for_ (lookup oid $ gameObligations g) \obligation -> do
        traverse_ discardCard (Obligation.obligationHeldEncounterCards $ toAttrs obligation)
        traverse_ discardCard (Obligation.obligationHeldCards $ toAttrs obligation)
      pure $ g & (entitiesL . obligationsL %~ deleteMap oid)
        & (removedEntitiesL . obligationsL %~ maybe id (insertMap oid) (lookup oid $ gameObligations g))
    -- a villain out of the game leaves play as he would, what is on him discarded
    VillainRef _ -> g <$ push (RemoveFromPlay target)
    -- a main scheme out of the game, what is attached to it discarded, leaves its area
    -- without one until a stage takes its place
    MainSchemeRef mid -> do
      frameworkDiscard $ for_ [uid | (uid, u) <- HashMap.toList (gameUpgrades g), upgradeAttachedMainScheme (toAttrs u) == Just mid] removeFromPlay
      pure $ g & (entitiesL . mainSchemesL %~ deleteMap mid) & (areasL . each %~ \a -> a {areaMainSchemes = filter (/= mid) (areaMainSchemes a)})
    -- out of the game, not discarded: what is attached to it is discarded
    SideSchemeRef sid -> whenCanLeavePlay g sid do
      for_ (lookup sid $ gameSideSchemes g) $ traverse_ discardCard . SideScheme.sideSchemeHeldEncounterCards . toAttrs
      frameworkDiscard $ for_
        [ uid
        | (uid, u) <- HashMap.toList (gameUpgrades g)
        , upgradeAttachedSideScheme (toAttrs u) == Just sid
        ]
        removeFromPlay
      for_ [aid | (aid, a) <- HashMap.toList (gameAttachments g), attachmentSideScheme (toAttrs a) == Just sid] removeFromPlay
      pure $ g & (entitiesL . sideSchemesL %~ deleteMap sid) & (areasL . each %~ \a -> a {areaSideSchemes = filter (/= sid) (areaSideSchemes a)})
    MinionRef mid -> case lookup mid (gameMinions g) of
      Nothing -> pure g
      Just minion -> do
        let a = toAttrs minion
        frameworkDiscard $ for_ (minionUpgrades a) removeFromPlay
        frameworkDiscard $ for_ (minionAttachments a) removeFromPlay
        for_ (Minion.minionHeldCards a) discardCard
        for_ (Minion.minionHeldTreacheries a) discardCard
        minionDisengages (minionEngagedIdentity a) mid
        pure $ g & (entitiesL . minionsL %~ deleteMap mid) & (removedEntitiesL . minionsL %~ insertMap mid minion)
    AttachmentRef aid -> do
      when (HashMap.member aid (gameAttachments g)) $ attachmentRemoved aid
      pure $ g & (entitiesL . attachmentsL %~ deleteMap aid)
    EnvironmentRef eid -> pure $ g & (entitiesL . environmentsL %~ deleteMap eid)
      & (areasL . each %~ \area -> area {areaEnvironments = filter (/= eid) (areaEnvironments area)})
    CardIdRef cid -> g <$ push (RemovePlayerCardFromHeldZones cid) -- Identity also removes ordinary player zones
    -- an ally in play taken out of the game (tucked): what is attached to it discarded
    AllyRef aid -> do
      for_ (lookup aid $ gameAllies g) \ally -> do
        frameworkDiscard $ for_ (allyUpgrades $ toAttrs ally) removeFromPlay
        frameworkDiscard $ for_ [Attachment.attachmentId a | at <- HashMap.elems (gameAttachments g),let a = toAttrs at,Attachment.attachmentAlly a == Just aid] removeFromPlay
        for_ (Ally.allyHeldCards $ toAttrs ally) discardCard
        for_ (Ally.allyHeldTreacheries $ toAttrs ally) discardCard
        identityStep (getAllyController ally) (AllyRemoved aid)
      pure $ g & (entitiesL . alliesL %~ deleteMap aid)
        & (removedEntitiesL . alliesL %~ maybe id (insertMap aid) (lookup aid $ gameAllies g))
    SupportRef sid | maybe False (elem Permanent . cdKeywords . supportCardDef . toAttrs) (lookup sid $ gameSupports g) -> pure g
    SupportRef sid -> do
      for_ (lookup sid $ gameSupports g) \support -> do
        for_ (supportHeldCards $ toAttrs support) discardCard
        identityStep (getSupportController support) (SupportRemoved sid)
      pure $ g & entitiesL . supportsL %~ deleteMap sid
    -- an upgrade out of the game, not discarded: the cards it holds are
    UpgradeRef uid -> do
      for_ (lookup uid $ gameUpgrades g) \upgrade -> do
        upgradeRemoved uid
        for_ (upgradeHeldCards $ toAttrs upgrade) discardCard
      pure $ g & (entitiesL . upgradesL %~ deleteMap uid)
        & (removedEntitiesL . upgradesL %~ maybe id (insertMap uid) (lookup uid $ gameUpgrades g))
    -- an event resolved and gone, out of the game rather than discarded (a Cosmic Entity
    -- revealed from the encounter deck)
    EventRef eid -> pure $ g & (entitiesL . eventsL %~ deleteMap eid)
    _ -> error "Unhandled"
  RemoveFromPlay target
    | Just (PlayerCard card) <- cardInPlay g target -> do
        modifiers <- getModifiers target
        let permanent = Permanent `elem` cdKeywords (getCardDef card)
            barred = CannotLeavePlay `elem` modifiers || CannotBeDiscarded `elem` modifiers
            removed = RemovedFromGameInsteadOfLeavingPlay `elem` modifiers
            collecting = toCardId card `elem` gameCollecting
            finish = case target of
              AllyRef aid | Just ally <- lookup aid (gameAllies g) ->
                ResolveAllyLeavingPlay (getAllyController ally) aid (getCardDef ally) (RemoveFromPlayAfterWindow target)
              _ -> RemoveFromPlayAfterWindow target
        unless (permanent || barred) $
          if removed then push (RemoveFromGame target) else do
            unless collecting $ checkWindows [W.Window W.Would $ W.WouldBeDiscardedFromPlay (Left card)]
            push finish
            if collecting then placeInCollection card else do
              discardCard card
              cardSpecific GameRef "afterDiscardFromPlay" (PlayerCard card)
        pure g
  msg | Just (target, windowOpened, defeated) <- (case msg of
          RemoveFromPlay target -> Just (target, False, False)
          RemoveFromPlayAfterWindow target -> Just (target, True, False)
          CardSpecific GameRef "removeDefeatedMinion" (fromJSON -> Success mid) -> Just (MinionRef mid, False, True)
          _ -> Nothing) ->
    let discardLeaving :: (ReverseQueue n, IsCard card) => card -> n ()
        discardLeaving c = unless windowOpened $ discardFromPlay g c
     in case target of
      IdentityRef iid -> do
        transferMojoThreat (IdentityCharacter iid)
        when (isNothing gameCompetition && length (g ^. entitiesL . playersL) <= 1) (gameOver Lost)
        -- the last player of an area defeated, with players left in others
        area <- getAreaOf iid
        when (isNothing gameCompetition && length (g ^. entitiesL . playersL) > 1 && all (== iid) (areaPlayers area)) $
          push (AreaDefeated $ areaId area)
        let competition' = flip fmap gameCompetition $ \c -> case CW.playerTeam c iid of
              Nothing -> c
              Just team ->
                let survivors = filter (\p -> p /= iid && p `elem` CW.teamPlayers c team) gamePlayerOrder
                 in case survivors of
                      [] -> CW.recordTeamOutcome team CW.AllHeroesDefeated c
                      first : _ | CW.teamFirstPlayer c team == iid -> CW.setTeamFirstPlayer team first c
                      _ -> c
            order = filter (/= iid) gamePlayerOrder
            -- whoever is left takes over from an eliminated active player
            next = case break (== iid) gamePlayerOrder of
              (before, _ : after) -> headMay (after <> before)
              _ -> headMay order
        pure $
          g
            & (\table -> table {gameCompetition = competition'})
            & (entitiesL . playersL %~ deleteMap iid)
            & (removedEntitiesL . playersL %~ maybe id (insertMap iid) (lookup iid $ gamePlayers g))
            & (activeCostL %~ filter ((/= iid) . activeCostIdentityId))
            & (playerOrderL .~ order)
            & (activePlayerL %~ \active -> if active == iid then fromMaybe active next else active)
            & (areasL . each %~ \a -> a {areaPlayers = filter (/= iid) (areaPlayers a)})
      -- a villain leaving play takes his own deck and side scheme with him, and what is
      -- attached to him is discarded; the next villain of his area becomes active
      VillainRef vid -> case lookup vid (gameVillains g) of
        Nothing -> pure g
        Just v -> do
          let a = toAttrs v
          for_ (Villain.villainAttachments a) (removeFromPlay . AttachmentRef)
          for_ (villainUpgrades a) (removeFromPlay . UpgradeRef)
          push $ RemoveDeck (VillainDeckKey vid)
          let signature = lookup vid gameSignatureSchemes
          -- what is attached to his side scheme leaves play with it
          for_
            [ aid
            | (aid, att) <- HashMap.toList (gameAttachments g)
            , isJust signature
            , attachmentSideScheme (toAttrs att) == signature
            ]
            (removeFromPlay . AttachmentRef)
          let
            leave area =
              -- he stays listed in his area, out of play there: his kept deck is shown in it
              let rest = filter (/= vid) (areaVillains area)
               in area
                    { areaActiveVillain = if areaActiveVillain area == Just vid then headMay rest else areaActiveVillain area
                    , areaSideSchemes = filter ((/= signature) . Just) (areaSideSchemes area)
                    }
          pure $
            g
              & (entitiesL . villainsL %~ deleteMap vid)
              & (removedEntitiesL . villainsL %~ insertMap vid v)
              & (entitiesL . sideSchemesL %~ maybe id deleteMap signature)
              & (signatureSchemesL %~ deleteMap vid)
              & (areasL . each %~ leave)
      AllyRef aid -> do
        transferMojoThreat (AllyCharacter aid)
        removed <- elem RemovedFromGameInsteadOfLeavingPlay <$> getModifiers target
        if removed then g <$ push (RemoveFromGame target) else do
          for_ (lookup aid $ gameAllies g) $ \ally -> do
            let ident = getAllyController ally
            -- what is attached to it is discarded with it
            frameworkDiscard $ for_ (allyUpgrades $ toAttrs ally) (if defeated then defeatedLeavesPlay else removeFromPlay)
            frameworkDiscard $ for_ [toId at | at <- HashMap.elems (gameAttachments g), Attachment.attachmentAlly (toAttrs at) == Just aid] (if defeated then defeatedLeavesPlay else removeFromPlay)
            for_ (Ally.allyHeldCards $ toAttrs ally) discardCard
            for_ (Ally.allyHeldTreacheries $ toAttrs ally) discardCard
            identityStep ident $ AllyRemoved aid
            discardLeaving ally
          -- kept as removed, so an ability it was discarded to pay for still resolves
          pure $
            g
              & (entitiesL . alliesL %~ deleteMap aid)
              & (removedEntitiesL . alliesL %~ maybe id (insertMap aid) (lookup aid $ gameAllies g))
      -- a permanent support does not leave play: the Milano stays for the next first player
      SupportRef sid | maybe False (elem Permanent . cdKeywords . supportCardDef . toAttrs) (lookup sid $ gameSupports g) -> pure g
      SupportRef sid -> do
        -- it is discarded, and the cards attached to it facedown with it
        for_ (lookup sid $ gameSupports g) $ \support -> do
          identityStep (getSupportController support) (SupportRemoved sid)
          discardLeaving (toAttrs support)
          for_ (supportHeldCards $ toAttrs support) discardCard
        -- kept as removed, so an ability it was discarded to pay for still resolves
        pure $
          g
            & (entitiesL . supportsL %~ deleteMap sid)
            & (removedEntitiesL . supportsL %~ maybe id (insertMap sid) (lookup sid $ gameSupports g))
      UpgradeRef uid -> do
        case (lookup uid $ gameUpgrades g) of
          Nothing -> pure g
          -- the permanent keyword: it cannot leave play ("Permanent")
          Just upgrade | isPermanent upgrade -> pure g
          Just upgrade -> do
            upgradeRemoved uid
            discardLeaving upgrade
            for_ (upgradeHeldCards $ toAttrs upgrade) discardCard
            pure $
              g
                & (entitiesL . upgradesL %~ deleteMap uid)
                & (removedEntitiesL . upgradesL %~ insertMap uid upgrade)
      -- a permanent attachment does not leave play: it attaches anew ('Marvel.Attachment.Types')
      AttachmentRef aid | maybe False isPermanent (lookup aid $ gameAttachments g) -> pure g
      AttachmentRef aid -> do
        protected <- elem CannotBeDiscarded <$> getModifiers (AttachmentRef aid)
        if protected then pure g else do
          for_ (lookup aid $ gameAttachments g) $ \attachment -> do
            attachmentRemoved aid
            discardLeaving attachment
          pure $ g & (entitiesL . attachmentsL %~ deleteMap aid)
      -- discarded, its own cards with it, and no longer engaged
      MinionRef mid -> case lookup mid (gameMinions g) of
        Nothing -> pure g
        Just minion -> do
          transferMojoThreat (MinionCharacter mid)
          cancelEnemyActivation mid
          let a = toAttrs minion
          -- Host discard replacements resolve while its attachments still exist.
          discardLeaving minion
          frameworkDiscard $ for_ (minionUpgrades a) removeFromPlay
          frameworkDiscard $ for_ (minionAttachments a) (if defeated then defeatedLeavesPlay else removeFromPlay)
          for_ (Minion.minionHeldCards a) discardCard
          for_ (Minion.minionHeldTreacheries a) discardCard
          minionDisengages (minionEngagedIdentity a) mid
          pure $
            g
              & (entitiesL . minionsL %~ deleteMap mid)
              & (removedEntitiesL . minionsL %~ insertMap mid minion)
      TreacheryRef tid -> do
        for_ (lookup tid $ gameTreacheries g) discardCard
        pure $ g & (entitiesL . treacheriesL %~ deleteMap tid)
      ObligationRef oid -> do
        for_ (lookup oid $ gameObligations g) $ \obligation -> do
          discardLeaving obligation
          for_ (obligationOwner $ toAttrs obligation) $ cardSpecific GameRef "mojoObligationDiscarded"
        pure $ g & (entitiesL . obligationsL %~ deleteMap oid)
      -- a villain's own side scheme cannot leave play while he is in play
      SideSchemeRef sid | sid `elem` HashMap.elems gameSignatureSchemes -> pure g
      SideSchemeRef sid -> whenCanLeavePlay g sid do
        for_ (lookup sid $ gameSideSchemes g) $ traverse_ discardCard . SideScheme.sideSchemeHeldEncounterCards . toAttrs
        -- what is attached to it is discarded with it, first
        frameworkDiscard $ for_
          [ uid
          | (uid, u) <- HashMap.toList (gameUpgrades g)
          , upgradeAttachedSideScheme (toAttrs u) == Just sid
          ]
          removeFromPlay
        for_ (lookup sid $ gameSideSchemes g) discardLeaving
        pure $
          g
            & (entitiesL . sideSchemesL %~ deleteMap sid)
            & (removedEntitiesL . sideSchemesL %~ maybe id (insertMap sid) (lookup sid $ gameSideSchemes g))
      MainSchemeRef mid -> do
        frameworkDiscard $ for_ [uid | (uid, u) <- HashMap.toList (gameUpgrades g), upgradeAttachedMainScheme (toAttrs u) == Just mid] removeFromPlay
        pure $ g & (entitiesL . mainSchemesL %~ deleteMap mid)
          & (areasL . each %~ \area -> area {areaMainSchemes = filter (/= mid) (areaMainSchemes area)})
      EnvironmentRef eid -> do
        for_ (lookup eid $ gameEnvironments g) discardLeaving
        -- it leaves its area too: one put into play again is listed afresh, last
        pure $ g & (entitiesL . environmentsL %~ deleteMap eid) & (areasL . each %~ \a -> a {areaEnvironments = filter (/= eid) (areaEnvironments a)})
      _ -> error "Unhandled target"
  AddMainScheme cardCode -> do
    you <- getYou
    addMainSchemeIn (maybe defaultAreaId areaId $ areaOfTarget g (IdentityRef you)) cardCode g
  AddSetupMainScheme cardCode -> do
    mainSchemeId <- getRandom
    case lookupMainScheme cardCode mainSchemeId of
      Just x -> do
        first <- selectJust FirstPlayer
        chooseOnePromptedAbout first "$label.setupMainScheme" mainSchemeId
          [ targeting mainSchemeId do
              setupMainScheme
              mainSchemeStep mainSchemeId MainSchemePlaceInitialThreat
              mainSchemeStep mainSchemeId RevealMainScheme
              you <- getYou
              let here = maybe defaultAreaId areaId $ areaOfTarget g (IdentityRef you)
                  starting = null [sid | sid <- HashMap.keys (gameMainSchemes g), (areaId <$> areaOfTarget g (MainSchemeRef sid)) == Just here]
              when starting $ for_ (villainsIn g here) $ \vid ->
                checkWindows [W.Window When $ W.RevealVillain vid W.RevealedFromVillain]
          ]
        you <- getYou
        pure $ g & (entitiesL . mainSchemesL . at mainSchemeId ?~ x) & intoAreaOf you (\a -> a {areaMainSchemes = areaMainSchemes a <> [mainSchemeId]})
      Nothing -> throwM $ MissingCardCode "AddSetupMainScheme" cardCode
  -- the main scheme leaves the game, each card attached to it discarded, and the next
  -- takes its place in its area
  ReplaceMainScheme old cardCode -> do
    for_
      [ uid
      | (uid, u) <- HashMap.toList (gameUpgrades g)
      , upgradeAttachedMainScheme (toAttrs u) == Just old
      ]
      removeFromPlay
    let aid = maybe defaultAreaId areaId $ areaOfTarget g (MainSchemeRef old)
    addMainSchemeIn aid cardCode $
      g
        & (entitiesL . mainSchemesL %~ deleteMap old)
        & (areasL . each %~ \a -> a {areaMainSchemes = filter (/= old) (areaMainSchemes a)})
  -- the villain card replaced by another of his title, the same villain: swapped, his
  -- hit point dial stays (RR "Swap"), and the after-swap window opens; advanced to, it
  -- enters play at its own hit points, as a next stage
  ReplaceVillain vid cardCode kind -> case (lookup vid (gameVillains g), lookupVillain cardCode vid) of
    (Just old, Just (Villain.Villain new)) -> do
      let o = toAttrs old
          keep = case kind of
            Swapped -> Villain.keepDial o
            Advanced -> id
      case kind of
        Swapped -> checkWindows [W.Window After $ W.VillainSwapped vid]
        Advanced -> advanceVillain vid
      pure $ g & entitiesL . villainsL . at vid ?~ Villain.Villain (Villain.overVillainAttrs (keep . Villain.carryOver o) new)
    _ -> pure g
  AddCardToVictoryDisplay card -> pure $ g & victoryDisplayL <>~ [card]
  AddVillain cardCode -> do
    villainId <- getRandom
    case lookupVillain cardCode villainId of
      Just x -> do
        villainStep villainId SetVillainHp
        you <- getYou
        let join a = a {areaVillains = areaVillains a <> [villainId], areaActiveVillain = areaActiveVillain a <|> Just villainId}
        pure $ g & (entitiesL . villainsL . at villainId ?~ x) & intoAreaOf you join
      Nothing -> throwM $ MissingCardCode "AddVillain" cardCode
  UsedAbility ident a ->
    pure $ g & usedAbilitiesL %~ HashMap.insertWith (<>) ident [(a, gameWindowDepth)]
  DisableActiveCost ident -> pure $ g & activeCostL %~ filter ((/= ident) . activeCostId)
  CardSpecific GameRef "playSearchedCardWithReduction" (fromJSON -> Success (you :: IdentityId, amount :: Natural, card :: PlayerCard)) -> do
    playable <- canPlaySearchedCardWithReduction you amount card
    if not playable then pure g else do
      eid <- getRandom
      identityStep you (PlayingCard card Nothing)
      disableEffect eid
      pure $ g & entitiesL . effectsL %~ insertMap eid (cardPlayReduction eid you card amount)
  PlayCardWithReduction ident card reduction -> do
    eid <- getRandom
    cardSpecific ident "playFromHand" (card, Nothing :: Maybe W.Window)
    -- The nested play, including its payment or abort, completes before cleanup.
    disableEffect eid
    pure $ g & entitiesL . effectsL %~ insertMap eid (cardPlayReduction eid ident card reduction)
  PlayCardIgnoringResourceCost ident card -> do
    eid <- getRandom
    cardSpecific ident "playFromHand" (card, Nothing :: Maybe W.Window)
    disableEffect eid
    pure $ g & entitiesL . effectsL %~ insertMap eid (cardPlayIgnoringCost eid ident card)
  SetActiveCost activeCost -> do
    createdActiveCost (activeCostId activeCost)
    pure $ g & activeCostL %~ (activeCost :)
  CardSpecific GameRef "enemyActivatesWithoutBoost" (fromJSON -> Success (source :: Source, enemy, you)) -> do
    case enemy of
      EnemyVillainId vid -> do
        hero <- identityMatches HeroIdentity you
        if hero then villainAttacksWithoutBoost vid you else villainSchemesWithoutBoostAgainst vid you
        pure g
      EnemyMinionId mid -> do
        eid <- getRandom
        let attrs = Effect.EffectAttrs eid "engine-lasting" source (EnemyEntity $ EnemyWithId enemy) [NoActivationBoostCard] Nothing
        minionActivatesAgainst mid you
        disableEffect eid
        pure $ g & entitiesL . effectsL %~ insertMap eid (Effect.Effect $ Lasting attrs)
      EnemyAttachmentId _ -> pure g
  CardSpecific GameRef "untilNextVillainPhase" (fromJSON -> Success (source, target, modifiers)) -> do
    eid <- getRandom
    let attrs = Effect.EffectAttrs eid "engine-lasting" source (SourceEntity target) modifiers (Just Effect.DisableAtStartOfVillainPhase)
    pure $ g & entitiesL . effectsL %~ insertMap eid (Effect.Effect $ Lasting attrs)
  CardSpecific GameRef "preventAllDamageFromAttack" (fromJSON -> Success source) -> do
    eid <- getRandom
    let attrs = Effect.EffectAttrs eid "engine-lasting" source (SourceEntity source) [PreventAllAttackDamage] (Just Effect.DisableAtEndOfAttack)
    pure $ g & entitiesL . effectsL %~ insertMap eid (Effect.Effect $ Lasting attrs)
  CardSpecific GameRef "payEnemyAttackNoDamageCost" (fromJSON -> Success (you :: IdentityId,enemy :: EnemyId,source :: Source,cost :: Cost)) -> do
    payCostThenWithPayment you source cost GameRef "enemyAttackNoDamageCostPaid" (you,enemy,source)
    pure g
  CardSpecific GameRef "enemyAttackNoDamageCostPaid" (fromJSON -> Success ((you :: IdentityId,enemy :: EnemyId,source :: Source),_payment :: Payment)) -> do
    eid <- getRandom
    let attrs = Effect.EffectAttrs eid "engine-lasting" source (EnemyEntity $ EnemyWithId enemy) [AttackAdditionalCostPaid source] (Just Effect.DisableAtEndOfAttack)
    cardSpecific GameRef "resumePaidEnemyAttackWithoutDamage" (you,enemy,source)
    pure $ g & entitiesL . effectsL %~ insertMap eid (Effect.Effect $ Lasting attrs)
  CardSpecific GameRef "resumePaidEnemyAttackWithoutDamage" (fromJSON -> Success (you,enemy :: EnemyId,source :: Source)) -> do
    attackEnemyWithoutDamage you enemy source
    pure g
  CardSpecific GameRef "payEnemyAttackCost" (fromJSON -> Success (you :: IdentityId,target :: Target,source :: Source,damage :: Damage,cost :: Cost)) -> do
    payCostThenWithPayment you source cost GameRef "enemyAttackCostPaid" (you,target,source,damage)
    pure g
  CardSpecific GameRef "enemyAttackCostPaid" (fromJSON -> Success ((you :: IdentityId,target :: Target,source :: Source,damage :: Damage),_payment :: Payment)) -> do
    eid <- getRandom
    let attrs = Effect.EffectAttrs eid "engine-lasting" source (SourceEntity $ toSource target) [AttackAdditionalCostPaid source] (Just Effect.DisableAtEndOfAttack)
    -- Choice construction reads the game. Resume only once the paid marker is
    -- stored, so the same attack cannot ask for its additional cost twice.
    cardSpecific GameRef "resumePaidEnemyAttack" (you,target,source,damage)
    pure $ g & entitiesL . effectsL %~ insertMap eid (Effect.Effect $ Lasting attrs)
  CardSpecific GameRef "resumePaidEnemyAttack" (fromJSON -> Success (you,target,source,damage)) -> do
    resolveAs you $ DamageEnemy target source damage
    pure g
  CardSpecific GameRef "basicThwartIgnoringPatrolAndCrisis" (fromJSON -> Success (you,bonus)) -> do
    confused <- identityMatches ConfusedIdentity you
    if confused then removeConfused you >> pure g else do
      replaced <- replacePowerWithDiscard you
      if replaced then pure g else do
        eid <- getRandom
        let attrs = Effect.EffectAttrs eid "engine-lasting" (IdentityRef you) (SourceEntity $ IdentityRef you) [BasicThwartIgnoresPatrol, IgnoresCrisis] Nothing
        checkWindows [W.Window W.When $ W.MadeBasicThwart you]
        basicThwart you bonus
        disableEffect eid
        pure $ g & entitiesL . effectsL %~ insertMap eid (Effect.Effect $ Lasting attrs)
  CardSpecific GameRef "revealEncounterCardWithPlayerCount" (fromJSON -> Success (you,count,card)) -> do
    let target = SideSchemeRef $ SideSchemeId $ unCardId $ ecCardId card
    revealEncounterCard you card
    cardSpecific GameRef "clearPrintedPlayerCount" target
    pure g {gamePrintedPlayerCounts = (target,count) : filter ((/= target) . fst) gamePrintedPlayerCounts}
  CardSpecific GameRef "clearPrintedPlayerCount" (fromJSON -> Success target) ->
    pure g {gamePrintedPlayerCounts = filter ((/= target) . fst) gamePrintedPlayerCounts}
  CardSpecific GameRef "resolveEncounterWhenRevealed" (fromJSON -> Success (you, card)) -> do
    let treachery = createTreachery card
    treacheryStep (toId treachery) (RevealTreachery you)
    cardSpecific GameRef "clearWhenRevealedTreachery" (you,toId treachery)
    pure $ g & entitiesL . treacheriesL %~ insertMap (toId treachery) treachery
  CardSpecific GameRef "dealMinionAsEncounterCard" (fromJSON -> Success (mid :: MinionId,you :: IdentityId)) -> do
    for_ (lookup mid $ gameMinions g) $ \minion -> do
      checkWindows [W.Window W.Would $ W.MinionLeavingPlay mid]
      push $ ResolveMinionLeavingPlay mid (RemoveFromPlayAfterWindow $ MinionRef mid)
      cardSpecific GameRef "dealDepartedMinion" (you,toCard minion)
    pure g
  CardSpecific GameRef "dealDepartedMinion" (fromJSON -> Success (you,EncounterCard card)) -> do
    unless (HashMap.member (MinionId $ unCardId $ ecCardId card) (gameMinions g)) $ dealFoundEncounterCard you card
    pure g
  CardSpecific GameRef "clearWhenRevealedTreachery" (fromJSON -> Success (you,tid)) -> do
    for_ (lookup tid $ gameTreacheries g) $ \treachery ->
      when (Treachery.treacherySurge (toAttrs treachery) || Marvel.Keyword.Surge `member` cdKeywords (getCardDef treachery)) $ surge you
    pure $ g & entitiesL . treacheriesL %~ deleteMap tid
  CardSpecific GameRef "playerCardSetup" (fromJSON -> Success (you :: IdentityId)) -> do
    let supports = [support | support <- HashMap.elems (gameSupports g), getSupportController support == you]
    for_ supports $ \support -> for_ (filter ((== Ability.Setup) . abilityType) $ getAbilities support) $ \ability ->
      ranAbility you (toTarget support) (abilityIndex ability) [] NoPayment
    pure g
  CardSpecific GameRef "campaignRemoveAccelerationToken" _ -> do
    you <- getYou
    let mains = [sid | (sid, scheme) <- HashMap.toList (gameMainSchemes g), mainSchemeAccelerationTokens (toAttrs scheme) > 0
          || any (\area -> areaAccelerationTokens area > 0 && Just (areaId area) == (areaId <$> areaOfTarget g (MainSchemeRef sid))) gameAreas]
        sides = [sid | (sid, scheme) <- HashMap.toList (gameSideSchemes g), sideSchemeAccelerationTokens (toAttrs scheme) > 0]
    chooseOne you $ [targeting sid (cardSpecific GameRef "campaignRemoveMainAcceleration" sid) | sid <- mains]
      <> [targeting sid (cardSpecific GameRef "campaignRemoveSideAcceleration" sid) | sid <- sides]
    pure g
  CardSpecific GameRef "campaignRemoveMainAcceleration" (fromJSON -> Success sid) -> do
    let aid = maybe defaultAreaId areaId $ areaOfTarget g (MainSchemeRef sid)
        adjacent = any (\area -> areaId area == aid && areaAccelerationTokens area > 0) gameAreas
    if adjacent then pure $ overArea aid (\area -> area {areaAccelerationTokens = subtractNatural 1 $ areaAccelerationTokens area}) g
      else do
        for_ (lookup sid $ gameMainSchemes g) $ \scheme -> mainSchemeStep sid (SetMainSchemeAcceleration $ subtractNatural 1 $ mainSchemeAccelerationTokens $ toAttrs scheme)
        pure g
  CardSpecific GameRef "campaignRemoveSideAcceleration" (fromJSON -> Success (sid :: SideSchemeId)) -> do
    cardSpecific sid "removeAccelerationToken" ()
    pure g
  CardSpecific GameRef "skipNextVillainPhase" _ -> do
    eid <- getRandom
    let attrs = Effect.EffectAttrs eid "engine-lasting" GameRef (SourceEntity GameRef) [SkipNextVillainPhase] Nothing
    pure $ g & entitiesL . effectsL %~ insertMap eid (Effect.Effect $ Lasting attrs)
  CardSpecific GameRef "untilEnemyActivationEnds" (fromJSON -> Success (source,enemy,modifiers)) -> do
    eid <- getRandom
    let attrs = Effect.EffectAttrs eid "engine-lasting" source (EnemyEntity $ EnemyWithId enemy) modifiers (Just $ Effect.DisableAfterEnemyActivation enemy)
    pure $ g & entitiesL . effectsL %~ insertMap eid (Effect.Effect $ Lasting attrs)
  CardSpecific GameRef "lastingModifiers" (fromJSON -> Success (source, matcher, modifiers, roundEnd :: Bool)) -> do
    eid <- getRandom
    let attrs = Effect.EffectAttrs eid "engine-lasting" source matcher modifiers (Just $ if roundEnd then Effect.DisableAtEndOfRound else Effect.DisableAtEndOfPhase)
    pure $ g & entitiesL . effectsL %~ insertMap eid (Effect.Effect $ Lasting attrs)
  CardSpecific GameRef "beginInPlayMinionBoost" (fromJSON -> Success (mid,you,actor)) -> do
    blanked <- isBlanked (MinionRef mid)
    case lookup mid (gameMinions g) of
      Just minion | not blanked, Star `elem` cdBoostIcons (getCardDef minion) -> do
        current <- withQueue $ \queue -> (queue,headMay [enemy | CheckWindows windows <- queue, W.Window W.After (W.EnemyActivation enemy _) <- windows])
        let enemy = fromMaybe (EnemyAttachmentId $ AttachmentId $ unMinionId mid) (actor <|> current)
        revealedAsBoost (MinionRef mid) enemy
        cardSpecific GameRef "endInPlayMinionBoost" mid
        pure g {gameInPlayBoosts = (mid,you,minion) : gameInPlayBoosts}
      _ -> pure g
  CardSpecific GameRef "endInPlayMinionBoost" (fromJSON -> Success mid) -> do
    active <- isQueued (== ClearBoosts)
    if active then do
      afterMatching (== ClearBoosts) $ cardSpecific GameRef "finishInPlayMinionBoost" (mid :: MinionId)
      pure g
    else pure g {gameInPlayBoosts = dropFirstInPlayBoost mid gameInPlayBoosts}
  CardSpecific GameRef "finishInPlayMinionBoost" (fromJSON -> Success mid) ->
    pure g {gameInPlayBoosts = dropFirstInPlayBoost mid gameInPlayBoosts}
  CardSpecific GameRef "giveDefeatedMinionAsBoost" (fromJSON -> Success (mid :: MinionId, enemy :: EnemyId)) -> do
    pending <- isQueued $ \case
      MinionMessage other MinionDefeated -> other == mid
      ResolveMinionLeavingPlay other (MinionMessage who MinionLeavesPlay) -> other == mid && who == mid
      _ -> False
    present <- inAnyArea $ selectAny $ EnemyWithId enemy
    let departed = find ((== CardId (unMinionId mid)) . toCardId) $
          map EncounterCard (scenarioDiscard $ toAttrs $ g.gameScenario)
          <> [PlayerCard card | player <- HashMap.elems $ gamePlayers g,card <- unDiscard $ playerIdentityDiscard $ toAttrs player]
          <> map EncounterCard (g.gameVictoryDisplay) <> map PlayerCard (g.gamePlayerVictoryDisplay)
    if not (pending && present && HashMap.member mid (gameMinions g)) then case departed of
      Just physical | present -> do
        let (card,original) = case physical of
              EncounterCard card -> (card,[])
              PlayerCard card -> (MkEncounterCard (pcCardId card) (pcCardDef card),[(pcCardId card,card)])
        giveCardAsBoost card enemy
        pure g {gamePlayerBoostCards = original <> g.gamePlayerBoostCards}
      _ -> pure g
    else do
      -- The printed When Defeated resolves while its card is still in play (RR p48).
      -- Cleanup also survives an interrupt cancelling the defeat and its after window.
      replaceMatchingMessage
        (\message -> [message,CardSpecific GameRef "clearDefeatedMinionBoost" (toJSON mid)])
        (\case CheckWindows ws -> any (\case W.Window W.After (W.DefeatedMinion other _ _ _) -> other == mid; _ -> False) ws; _ -> False)
      pure g {gameDefeatedMinionBoosts = (mid,enemy) : filter ((/= mid) . fst) gameDefeatedMinionBoosts}
  CardSpecific GameRef "clearDefeatedMinionBoost" (fromJSON -> Success (mid :: MinionId)) ->
    pure g {gameDefeatedMinionBoosts = filter ((/= mid) . fst) gameDefeatedMinionBoosts}
  CardSpecific GameRef "moveDefeatedMinionToBoost" (fromJSON -> Success (mid :: MinionId, enemy :: EnemyId)) -> do
    case lookup mid (gameMinions g) of
      Nothing -> pure g
      Just minion -> do
        present <- inAnyArea $ selectAny $ EnemyWithId enemy
        modifiers <- getModifiers (MinionRef mid)
        if not present || any (`elem` modifiers) [CannotLeavePlay,CannotLeavePlayByCardAbilities] then do
          cardSpecific GameRef "clearDefeatedMinionBoost" mid
          defeatedLeavesPlay mid
          pure g
        else do
          transferMojoThreat (MinionCharacter mid)
          cancelEnemyActivation mid
          let a = toAttrs minion
              (card, original) = case toCard minion of
                EncounterCard c -> (c,[])
                PlayerCard c -> (MkEncounterCard (pcCardId c) (pcCardDef c),[(pcCardId c,c)])
          frameworkDiscard $ for_ (minionUpgrades a) removeFromPlay
          frameworkDiscard $ for_ (minionAttachments a) defeatedLeavesPlay
          for_ (Minion.minionHeldCards a) discardCard
          for_ (Minion.minionHeldTreacheries a) discardCard
          for_ (Minion.minionBoostCards a) discardCard
          minionDisengages (minionEngagedIdentity a) mid
          case enemy of
            EnemyVillainId vid -> villainStep vid $ VillainDealtBoost card
            EnemyMinionId host -> minionStep host $ MinionDealtBoost card
            EnemyAttachmentId _ -> pure ()
          pure $ (g {gamePlayerBoostCards = original <> g.gamePlayerBoostCards,gameDefeatedMinionBoosts = filter ((/= mid) . fst) gameDefeatedMinionBoosts})
            & entitiesL . minionsL %~ deleteMap mid
            & removedEntitiesL . minionsL %~ insertMap mid minion
  CardSpecific GameRef "giveCardAsBoost" (fromJSON -> Success (EncounterCard card, enemy)) -> do
    let target = CardIdRef $ ecCardId card
    removeFromGame target
    case enemy of
      EnemyVillainId vid -> villainStep vid $ VillainDealtBoost card
      EnemyMinionId mid -> minionStep mid $ MinionDealtBoost card
      EnemyAttachmentId _ -> pure ()
    pure g
  CardSpecific GameRef "enemyAttackLosesKeyword" (fromJSON -> Success (enemy,keyword)) -> do
    current <- getEnemyAttack enemy
    pure $ restoreEnemyAttack enemy (loseKeyword keyword <$> current) g
  CardSpecific GameRef "repeatEnemyAttackAfterPayment" (fromJSON -> Success ((enemy :: EnemyId,character),_ :: Payment)) -> do
    present <- characterMatches AnyCharacter character
    owner <- getCharacterOwner character
    when present $ for_ owner $ \you -> enemyActivationAfterCurrent enemy $ case character of
      IdentityCharacter _ -> enemyAttacks enemy you
      AllyCharacter aid -> enemyAttacksAlly enemy you aid
      _ -> pure ()
    pure g
  CardSpecific GameRef "finishRaiseTheStakes" (fromJSON -> Success (card,defeated :: Bool)) -> do
    let aid = AttachmentId $ unCardId $ ecCardId card
        alreadyRemoved = card `elem` gameRemovedFromGameEncounterCards
    if defeated then do
      removeFromGame aid
      cardSpecific GameRef "removeEncounterCardFromPiles" card
      pure $ g {gameRemovedFromGameEncounterCards = nub $ card : gameRemovedFromGameEncounterCards}
    else if alreadyRemoved then pure g else do
      when (HashMap.member aid (gameAttachments g)) $ attachmentRemoved aid
      cardSpecific GameRef "removeEncounterCardFromPiles" card
      shuffleIntoEncounterDeck [card]
      pure $ g & entitiesL . attachmentsL %~ deleteMap aid
  CardSpecific GameRef "resolveTreacheryAsBoost" (fromJSON -> Success (tid,you,card)) -> do
    checkWindows [W.Window W.After $ W.ResolvedTreacheryCard you card]
    pure $ g {gameRemovedFromGameEncounterCards = filter ((/= ecCardId card) . ecCardId) gameRemovedFromGameEncounterCards}
      & entitiesL . treacheriesL %~ deleteMap tid
  CreatedEffect def source matcher -> do
    effectId <- getRandom
    ident <- toId <$> getActivePlayer
    let effect = createEffect effectId (toCardCode def) source matcher
    effectStep effectId (UsedEffect ident)
    pure $ g & (entitiesL . effectsL %~ insertMap effectId effect)
  DisabledEffect effectId ->
    pure $ g & (entitiesL . effectsL %~ deleteMap effectId)
  CardSpecific GameRef "cancelledPlayedEvent" (fromJSON -> Success (you,card)) -> do
    let event = createEvent you card
        played timing = checkWindows [W.Window timing $ W.EventPlayed you (toId event) (getCardDef card)]
    removeFromGame (CardIdRef $ pcCardId card)
    eventOwnedBy (toId event) (fromMaybe you $ pcOwner card)
    played W.When
    eventStep (toId event) ResolvedEvent
    played W.After
    reserved <- echoReservedReplay you
    when (maybe False ((== card) . fst) reserved) $ cardSpecific you "fneReplayFinished" ()
    pure $ g {gameResolvingEvents = Just $ toId event : filter (/= toId event) (fromMaybe [] gameResolvingEvents)}
      & (entitiesL . eventsL %~ insertMap (toId event) event)
  PutCardIntoPlay ident card payment mWindow wasPlayed -> do
    let mission = scenarioMission (toAttrs $ g.gameScenario)
        entersMission = wasPlayed && maybe False (\m -> not (missionFinished m) && missionPendingAlly m == Just (pcCardId card)) mission
    entryController <- case (cdCardType $ getCardDef card, paymentTarget payment) of
      (SupportType, Just (IdentityRef who)) -> pure who
      (UpgradeType, Just (AllyRef aid)) -> fromMaybe ident <$> getSourceController (AllyRef aid)
      _ -> pure ident
    let entersExhausted controller = any (\case CardsEnterPlayExhausted matcher -> cardMatch matcher card; _ -> False) <$> getModifiers (IdentityRef controller)
    result <- case cdCardType (getCardDef card) of
      PlayerSideSchemeType -> do
        let initial = lookupSideScheme (toCardCode card) (SideSchemeId $ unCardId $ pcCardId card)
            original = card {pcOwner = pcOwner card <|> Just ident, pcController = Just ident}
        scheme <- runMessage (CardSpecific (toRef initial) "playerSideSchemeCard" (toJSON original)) initial
        sideSchemeStep (toId scheme) SideSchemePlaceInitialThreat
        checkWindows [W.Window After $ W.SideSchemeEnteredPlay (toId scheme)]
        cardSpecific GameRef "checkPlayerSideSchemeLimit" ()
        pure $ g & (entitiesL . sideSchemesL %~ insertMap (toId scheme) scheme)
          & (removedEntitiesL . sideSchemesL %~ deleteMap (toId scheme))
          & intoAreaOf ident (\a -> a {areaSideSchemes = areaSideSchemes a <> [toId scheme]})
      AllyType -> do
        let fromMedLab = any (\(sid, _) -> maybe False ((== "38028") . toCardCode . supportCardDef . toAttrs) (lookup sid $ gameSupports g)) (paymentReservedSupportCards payment)
            initialAlly = createAlly ident card
        exhaustedEntry <- entersExhausted ident
        ally <- if fromMedLab || exhaustedEntry then runMessage (AllyMessage (toId initialAlly) AllyMsg.ExhaustedAlly) initialAlly else pure initialAlly
        allyStep (toId ally) (AllyOwnedBy $ fromMaybe ident $ pcOwner card)
        allyStep (toId ally) (AllyPaidWith payment)
        unless entersMission do
          cardSpecific (AllyRef $ toId ally) "applyToughness" ()
          identityStep ident (AllyCreated $ toId ally)
        -- Colossus is played and paid for during the attack's interrupt window;
        -- his printed instruction declares him without exhausting him.
        when (not entersMission && wasPlayed && toCardCode card == "45031") $ for_ mWindow \case
          W.Window _ (W.EnemyAttack enemy attacked) | attacked == ident ->
            enemyDefendedBy enemy (AllyCharacter $ toId ally)
          _ -> pure ()
        checkWindows $
          W.Window After (W.AllyEnteredPlay $ toId ally)
            : [W.Window After $ W.PlayedAlly (toId ally) | wasPlayed]
        -- back in play, no longer the copy kept as it was discarded
        let added = g & (entitiesL . alliesL %~ insertMap (toId ally) ally) & (removedEntitiesL . alliesL %~ deleteMap (toId ally))
        pure $ if entersMission then
          modifyMissionInGame (\m -> m {missionAllies = missionAllies m <> [toId ally], missionPendingAlly = Nothing, missionDiscount = False})
            added {gameUncontrolledAllies = added.gameUncontrolledAllies <> [toId ally]}
          else added
      SupportType -> do
        -- played under the control of the player it was played on, if any; it stays its
        -- owner's, whose discard pile it goes to (RR Owner)
        let controller = case paymentTarget payment of
              Just (IdentityRef p) -> p
              _ -> ident
        exhaustedEntry <- entersExhausted controller
        let initialSupport = createSupport controller card
        support <- if exhaustedEntry then runMessage (SupportMessage (toId initialSupport) ExhaustedSupport) initialSupport else pure initialSupport
        supportStep (toId support) (SupportOwnedBy $ fromMaybe ident $ pcOwner card)
        identityStep controller (SupportCreated $ toId support)
        when wasPlayed $ checkWindows [W.Window After $ W.PlayedSupport (toId support)]
        pure $ g
          & (entitiesL . supportsL %~ insertMap (toId support) support)
          & (removedEntitiesL . supportsL %~ deleteMap (toId support))
      UpgradeType -> do
        exhaustedEntry <- entersExhausted entryController
        let initialUpgrade = createUpgrade entryController card
        upgrade <- if exhaustedEntry then runMessage (UpgradeMessage (toId initialUpgrade) ExhaustedUpgrade) initialUpgrade else pure initialUpgrade
        upgradeStep (toId upgrade) (UpgradeOwnedBy $ fromMaybe ident $ pcOwner card)
        upgradeStep (toId upgrade) (PlayedUpgrade $ paymentTarget payment)
        pure $ g & (entitiesL . upgradesL %~ insertMap (toId upgrade) upgrade)
      EventType -> do
        let
          event = createEvent ident card
          -- "when you play" answers once the cost is paid, before the event resolves;
          -- "after you play", once it has
          played timing = checkWindows [W.Window timing $ W.EventPlayed ident (toId event) (getCardDef card)]
        eventOwnedBy (toId event) (fromMaybe ident $ pcOwner card)
        eventStep (toId event) (EventPaidWith payment)
        played When
        resolveEventEffect ident (toId event) (PlayedEvent ident payment (W.windowType <$> mWindow))
        eventStep (toId event) ResolvedEvent
        played After
        reserved <- echoReservedReplay ident
        when (maybe False ((== card) . fst) reserved) $ cardSpecific ident "fneReplayFinished" ()
        pure $ g {gameResolvingEvents = Just $ toId event : filter (/= toId event) (fromMaybe [] gameResolvingEvents)}
          & (entitiesL . eventsL %~ insertMap (toId event) event)
      _ -> error "Unhandled"
    when (cdCardType (getCardDef card) `elem` [AllyType, SupportType, UpgradeType, PlayerSideSchemeType]) $ do
      checkWindows [W.Window After $ W.PlayerCardEnteredPlay entryController card]
    pure result
  CardSpecific GameRef "checkPlayerSideSchemeLimit" _ -> do
    schemes <- filterM (fmap (notElem NotCountedAgainstPlayerSideSchemeLimit) . getModifiers) [s | s <- HashMap.elems (gameSideSchemes g), cdCardType (getCardDef s) == PlayerSideSchemeType,
                  toCardCode (getCardDef s) `notElem` ["40190a", "40191a", "40192a", "40193a", "40194a", "40195a"]]
    let limit = if length gameSeats <= 2 then 1 else 2
    when (length schemes > limit) $ do
      first <- selectJust FirstPlayer
      chooseOne first [targeting s (removeFromPlay s >> cardSpecific GameRef "checkPlayerSideSchemeLimit" ()) | s <- schemes]
    pure g
  CardSpecific GameRef "removePlayerVictoryCard" (fromJSON -> Success cid) ->
    pure $ g & playerVictoryDisplayL %~ filter ((/= cid) . pcCardId)
  -- the effect of an attack or thwart event is its player's attack or thwart: a stun or
  -- confusion replaces it as it would resolve, its cost paid ("Stunned", "Confused")
  ResolveEventEffect ident (EventMessage eid playEvent) -> do
    labels <- case lookup eid (gameEvents g) of
      Just (Event event) -> eventAbilitySubTypes event
      Nothing -> pure []
    canceled <- cancelLabeledAbility ident labels
    permitted <- canResolvePlayerCardAbilities ident
    unless (canceled || not permitted) do
      replaced <- if Ability.Attack `elem` labels then replaceHeroAttackWithDiscardAndStun ident else pure False
      unless replaced do
        when (Ability.Defense `elem` labels) $ beginLabeledDefense ident
        eventStep eid playEvent
    pure g
  FriendlyCharacterAttacks attacker target -> do
    let ref = characterRef attacker
    modifiers <- getModifiers ref
    stunned <- case attacker of
      IdentityCharacter ident -> identityMatches StunnedIdentity ident
      AllyCharacter aid -> characterHasCondition StunnedStatus (AllyCharacter aid)
      _ -> pure False
    -- A prohibition is stronger than a request to attack; a stun replaces the attack.
    unless (CannotAttack `elem` modifiers) $
      if stunned
        then removeStatus attacker StunnedStatus
        else do
          replaced <- case attacker of IdentityCharacter who -> replaceHeroAttackWithDiscardAndStun who; _ -> pure False
          unless replaced do
            checkWindows [W.Window When $ W.FriendlyCharacterAttack attacker target]
            push $ FriendlyAttackLands attacker target
    pure g
  FriendlyAttackLands attacker target -> do
    let source = characterRef attacker
    info <- case attacker of
      IdentityCharacter ident -> getsGame (lookup ident . gamePlayers) >>= \case
        Just p | isHero p -> Just <$> ((,) <$> getIdentityHeroAttackDamage p <*> getModifiedKeywords p)
        _ -> pure Nothing
      AllyCharacter aid -> getsGame (lookup aid . gameAllies) >>= \case
        Just a -> do
          modifiers <- getModifiers a
          n <- Ally.getModifiedAttack (toAttrs a)
          pure $ Just (n, toList (cdKeywords $ getCardDef a) <> [k | KeywordModifier k <- modifiers])
        Nothing -> pure Nothing
      _ -> pure Nothing
    for_ info \(n, keywords) -> do
      let damage = (toDamage n FromAttack) {damagePiercing = Piercing `elem` keywords, damageRanged = Ranged `elem` keywords}
      damageCharacter target source damage
      push $ FriendlyAttackResolved attacker target (Ranged `elem` keywords)
    pure g
  FriendlyAttackResolved attacker target ranged -> do
    -- Retaliate is forced and precedes optional after-attack responses. Its source
    -- remains the attacked identity; its target remains the friendly attacker.
    p <- getsGame (lookup target . gamePlayers)
    unless ranged $ for_ p \identity -> do
      n <- sum . map (\case Retaliate k -> k; _ -> 0) <$> getModifiedKeywords identity
      when (n > 0) $ retaliate target attacker n
    checkWindows [W.Window After $ W.FriendlyCharacterAttack attacker target]
    case attacker of
      AllyCharacter aid -> push $ FriendlyAttackConsequential aid
      _ -> pure ()
    pure g
  FriendlyAttackConsequential aid -> do
    ally <- getsGame (lookup aid . gameAllies)
    for_ ally \a -> do
      n <- Ally.getModifiedAttackConsequentialDamage (toAttrs a)
      when (n > 0) $ damageCharacter aid a (toDamage n FromConsequential)
    pure g
  ResolveEventWithoutPlaying ident card -> do
    let event = createEvent ident card
    eventStep (toId event) (PlayedEvent ident NoPayment Nothing)
    eventStep (toId event) ResolvedEvent
    pure $ g {gameResolvingEvents = Just $ toId event : filter (/= toId event) (fromMaybe [] gameResolvingEvents)}
      & (entitiesL . eventsL %~ insertMap (toId event) event)
  PutBoostIntoPlay (MinionRef mid) _ | HashMap.member mid (gameMinions g) -> pure g
  PutBoostIntoPlay target ident -> case target of
    MinionRef minionId ->
      case g ^. boostEntitiesL . minionsL . at minionId of
        Just minion -> do
          checkWindows [W.Window When $ W.MinionEnteredPlay minionId]
          cardSpecific (MinionRef minionId) "applyToughness" ()
          engageMinion minionId ident
          checkWindows [W.Window After $ W.MinionEnteredPlay minionId]
          pure $
            g
              & (boostEntitiesL . minionsL %~ deleteMap (toId minion))
              & (entitiesL . minionsL %~ insertMap (toId minion) minion)
              & (removedEntitiesL . minionsL %~ deleteMap (toId minion))
              & (boostCardsL %~ deleteMap target)
        _ -> error "messed up somewhere"
    -- any other card ("put Goblin Nation into play") enters play as it would revealed,
    -- without its windows, and is no longer a boost card to discard
    _ -> case lookup target gameBoostCards of
      Just card -> do
        putEncounterCardIntoPlay ident card
        pure $ g & boostCardsL %~ deleteMap target
      Nothing -> pure g
  -- "Boost: reveal this card": it is no longer a boost card, and is revealed for the
  -- player
  RevealBoost target ident | Just card <- inPlayBoostCard g target -> g <$ revealEncounterCard ident card
  RevealBoost target ident -> case lookup target gameBoostCards of
    Just card -> do
      revealEncounterCard ident card
      pure $
        g
          & (boostCardsL %~ deleteMap target)
          & (boostEntitiesL . sideSchemesL %~ deleteMap (SideSchemeId $ unCardId $ ecCardId card))
          & (boostEntitiesL . attachmentsL %~ deleteMap (AttachmentId $ unCardId $ ecCardId card))
          & (boostEntitiesL . treacheriesL %~ deleteMap (TreacheryId $ unCardId $ ecCardId card))
          & (boostEntitiesL . minionsL %~ deleteMap (MinionId $ unCardId $ ecCardId card))
    Nothing -> pure g
  RevealBoostCard card enemyId -> do
    checkWindows [W.Window W.When $ W.FlippedBoostCard card enemyId]
    cardSpecific GameRef "revealBoostAfterWindow" (True, card, enemyId)
    pure g
  RevealBoostCardWithoutAbility card enemyId -> do
    checkWindows [W.Window W.When $ W.FlippedBoostCard card enemyId]
    cardSpecific GameRef "revealBoostAfterWindow" (False, card, enemyId)
    pure g
  CardSpecific GameRef "revealBoostAfterWindow" (fromJSON -> Success (allowAbility, card, enemyId)) ->
    revealBoostCardFor allowAbility card enemyId g
  ConsumeBoostCard card -> do
    let cid = unCardId (ecCardId card)
    pure $ g & boostCardsL %~ HashMap.filter ((/= ecCardId card) . ecCardId)
      & boostEntitiesL . treacheriesL %~ deleteMap (TreacheryId cid)
      & boostEntitiesL . minionsL %~ deleteMap (MinionId cid)
      & boostEntitiesL . attachmentsL %~ deleteMap (AttachmentId cid)
      & boostEntitiesL . obligationsL %~ deleteMap (ObligationId cid)
      & boostEntitiesL . sideSchemesL %~ deleteMap (SideSchemeId cid)
      & boostEntitiesL . environmentsL %~ deleteMap (EnvironmentId cid)
  BoostCardResolved _ _ -> pure g
  CountBoostIcons target value cards -> do
    counts <- for cards \card -> (card,) <$> getBoostIcons card
    for_ cards \card -> checkWindows [W.Window When $ W.CountingBoostIcons card]
    push $ WithBoostIcons target value counts
    pure g
  ClearBoosts -> do
    for_ (HashMap.elems gameBoostCards) discardCard
    pure $ g & boostEntitiesL .~ defaultEntities & boostCardsL .~ mempty
  -- Keep eliminated identities for game summaries after their leave-play windows close.
  ClearRemoved -> pure $ g & removedEntitiesL .~ defaultEntities {entitiesPlayers = entitiesPlayers gameRemovedEntities}
  -- indirect damage (RR): assigned a point at a time among the characters the player
  -- controls that can take damage, none past what would defeat it (a tough one too, its
  -- card preventing what it is given), then dealt at once; what cannot be assigned is
  -- ignored
  AttachPlayerCardToMinion mid card -> do
    removePlayerCardFromHeldZones card
    removeFromGame card
    minionStep mid (MinionHoldsCard card)
    pure g
  AttachTreacheryToAlly aid card -> do
    allyStep aid (AllyHoldsTreachery card)
    pure $ g & entitiesL . treacheriesL %~ deleteMap (TreacheryId $ unCardId $ ecCardId card)
  DivideAttackDamage you source damage -> do
    allies <- selectList (AllyControlledBy $ IdentityWithId you)
    let characters = IdentityCharacter you : map AllyCharacter allies
        (base, extra) = damageAmount damage `divMod` fromIntegral (length characters)
    push $ AssignEvenDamage you source damage extra [(c, base) | c <- characters] characters
    pure g
  AssignEvenDamage you source damage extra assigned eligible -> do
    if extra == 0 || null eligible then
      pushAll . simultaneousDamage =<< traverse (\(c,n) -> capture $ when (n > 0) $ damageCharacter c source damage {damageAmount = n}) (HashMap.toList $ HashMap.fromListWith (+) assigned)
    else chooseOnePrompted you "$label.spdrDivideAttack" [targeting (characterRef c) $ push $ AssignEvenDamage you source damage (extra - 1) ((c,1) : assigned) (filter (/= c) eligible) | c <- eligible]
    pure g
  IndirectDamageAndExhaust you source damage assigned -> do
    assignIndirectDamage you [you] source damage assigned (\d as -> push $ IndirectDamageAndExhaust you source d as) True
    pure g
  ExhaustIfDamaged character before -> do
    after <- characterDamageNow character
    when (after > before) $ case character of
      IdentityCharacter p -> exhaust p
      AllyCharacter a -> exhaust a
      _ -> pure ()
    pure g
  AssignIndirectDamage ident source damage assigned ->
    g <$ assignIndirectDamage ident [ident] source damage assigned (indirectDamageAssigned ident source) False
  -- among the players: the group's, the first player assigning it
  AssignIndirectDamageAmongPlayers source n assigned -> do
    first <- selectJust FirstPlayer
    players <- getPlayers
    g <$ assignIndirectDamage first players source (toDamage n FromAbility) assigned (\d as -> push (AssignIndirectDamageAmongPlayers source (damageAmount d) as)) False
  SwapWeatherSupport you old card special -> do
    for_ old $ \sid -> for_ (lookup sid $ gameSupports g) $ \support -> do
      identityStep (getSupportController support) (SupportRemoved sid)
      -- Different titles leave and enter play (RR Swap). The Weather card
      -- returns to its own deck; cards held under it are discarded normally.
      for_ (supportHeldCards $ toAttrs support) discardCard
    putIntoPlay you card
    when special $ resolveCurrentWeatherSpecial you
    let leaving = old >>= \sid -> lookup sid (gameSupports g)
    pure $ g
      & entitiesL . supportsL %~ (\supports -> maybe supports (`deleteMap` supports) old)
      & removedEntitiesL . supportsL %~ maybe id (\support -> insertMap (toId support) support) leaving
  ResolveCurrentWeatherSpecial you -> do
    current <- selectOne (SupportMatches [SupportWithTrait Weather, SupportControlledBy $ IdentityWithId you])
    for_ current (resolveWeatherSpecial you)
    pure g
  WeatherSpecialFinished you sid -> do
    checkWindows [W.Window After $ W.WeatherSpecialResolved you sid]
    pure g
  -- a card set aside from play (Escape the Museum's Milano) leaves it, out of play and
  -- not discarded: permanent or not; the scenario keeps it set aside
  SetAside cards -> do
    let selected cid def = any (\card -> toCardId card == cid && sameCard (getCardDef card) def) cards
        gone = [s | s <- HashMap.elems (gameSupports g), selected (CardId $ unSupportId $ toId s) (supportCardDef $ toAttrs s)]
        upgrades = [u | u <- HashMap.elems (gameUpgrades g), selected (CardId $ unUpgradeId $ toId u) (getCardDef u)]
        minions = [m | m <- HashMap.elems (gameMinions g), selected (toCardId $ toCard m) (getCardDef m)]
        villains = [v | v <- HashMap.elems (gameVillains g), let VillainId uuid = toId v, selected (CardId uuid) (getCardDef v)]
        attachments = [at | at <- HashMap.elems (gameAttachments g), selected (toCardId $ toCard at) (getCardDef at)]
        schemes = [scheme | scheme <- HashMap.elems (gameSideSchemes g), selected (toCardId $ toCard scheme) (getCardDef scheme)]
    for_ schemes $ \scheme -> do
      for_ (SideScheme.sideSchemeHeldEncounterCards $ toAttrs scheme) discardCard
      for_ [uid | (uid,u) <- HashMap.toList (gameUpgrades g), upgradeAttachedSideScheme (toAttrs u) == Just (toId scheme)] removeFromPlay
      for_ [aid | (aid,a) <- HashMap.toList (gameAttachments g), Attachment.attachmentSideScheme (toAttrs a) == Just (toId scheme)] removeFromPlay
    for_ minions $ \m -> do
      minionDisengages (getMinionEngagedIdentity m) (toId m)
      traverse_ removeFromPlay (Minion.minionAttachments $ toAttrs m)
      traverse_ removeFromPlay (Minion.minionUpgrades $ toAttrs m)
    for_ villains $ \v -> do
      traverse_ removeFromPlay (Villain.villainAttachments $ toAttrs v)
      traverse_ removeFromPlay (Villain.villainUpgrades $ toAttrs v)
    traverse_ (attachmentRemoved . toId) attachments
    for_ gone \s -> identityStep (getSupportController s) (SupportRemoved (toId s))
    for_ upgrades \u -> do
      upgradeRemoved (toId u)
      for_ (upgradeHeldCards $ toAttrs u) discardCard
    pure $ g
      & entitiesL . supportsL %~ (\m -> foldr (deleteMap . toId) m gone)
      & entitiesL . upgradesL %~ (\m -> foldr (deleteMap . toId) m upgrades)
      & entitiesL . sideSchemesL %~ (\m -> foldr (deleteMap . toId) m schemes)
      & entitiesL . minionsL %~ (\m -> foldr (deleteMap . toId) m minions)
      & removedEntitiesL . minionsL %~ (\m -> foldr (\e -> insertMap (toId e) e) m minions)
      & entitiesL . villainsL %~ (\m -> foldr (deleteMap . toId) m villains)
      & removedEntitiesL . villainsL %~ (\m -> foldr (\e -> insertMap (toId e) e) m villains)
      & entitiesL . attachmentsL %~ (\m -> foldr (deleteMap . toId) m attachments)
      & removedEntitiesL . attachmentsL %~ (\m -> foldr (\e -> insertMap (toId e) e) m attachments)
      & areasL . each %~ (\area -> let remaining = filter (`notElem` map toId villains) (areaVillains area)
          in area {areaVillains = remaining,areaActiveVillain = if areaActiveVillain area `elem` map (Just . toId) villains then headMay remaining else areaActiveVillain area})
      & areasL . each %~ (\area -> area {areaSideSchemes = filter (`notElem` map toId schemes) (areaSideSchemes area)})
  -- divided damage (Giant Wasp, Wasp Sting): a point at a time among the enemies
  -- matching, then each share dealt at once; an attack's attacks each enemy dealt one
  DivideDamageAndThreat ident source damage schemes enemies -> do
    modifiers <- getModifiers source
    let bonus = if ThreatRemovalBonusInTotal `elem` modifiers
          then sum [n | ThreatRemovedModifier n <- modifiers] else 0
    push $ AssignDividedDamageAndThreat ident source damage (damageAmount damage + bonus) schemes enemies [] []
    pure g
  AssignDividedDamageAndThreat ident source damage remaining schemeMatcher enemyMatcher removed dealt -> do
    let given scheme = sum [n | (target,n) <- removed, target == scheme]
        next damage' remaining' removed' dealt' = push $
          AssignDividedDamageAndThreat ident source damage' remaining' schemeMatcher enemyMatcher removed' dealt'
    if remaining > 0 then do
      sourceModifiers <- getModifiers source
      labeledThwart <- elem Ability.Thwart <$> sourceAbilitySubTypes source
      rawPatrolled <- isPatrolled ident
      forbidden <- elem CannotThwartSideSchemes <$> getModifiers (IdentityRef ident)
      let patrolled = rawPatrolled && IgnoresPatrol `notElem` sourceModifiers
          allowed scheme = do
            removable <- canRemoveThreatFrom source scheme
            permitted <- case scheme of
              SchemeMainSchemeId sid -> do
                restricted <- isRestrictedMainScheme sid
                pure $ not (restricted && patrolled && labeledThwart)
              SchemeSideSchemeId _ -> pure $ not (forbidden && labeledThwart)
            pure $ removable && permitted
      schemes <- filterM allowed =<< selectList (schemeMatcher <> if labeledThwart then SchemeWithoutThwartPrevention else AnyScheme)
      open <- forMaybeM schemes $ \scheme -> do
        threat <- selectCount SchemeThreat (SchemeWithId scheme)
        let room = subtractNatural (given scheme) threat
        pure $ guard (room > 0) $> (scheme, room)
      case open of
        [] -> next damage 0 removed dealt
        [(scheme,room)] -> next damage (remaining - min remaining room) ((scheme,min remaining room) : removed) dealt
        _ -> chooseOnePrompted ident "$label.assignDividedThreat"
          [targeting (schemeRef scheme) (next damage (remaining - 1) ((scheme,1) : removed) dealt) | (scheme,_) <- open]
    else do
      enemies <- selectList (DamageableEnemy <> enemyMatcher)
      let n = damageAmount damage
          assign enemy amount = next damage {damageAmount = n - amount} 0 removed ((enemy,amount) : dealt)
      case enemies of
        _ | n == 0 || null enemies -> do
          -- Keep damage's attack windows, but put the already chosen threat removals
          -- in its simultaneous group, before damage can reveal another target.
          damages <- traverse (\(enemy,amount) -> capture $ resolveAs ident $
            DamageEnemy (enemyRef enemy) source damage {damageAmount = amount})
            (HashMap.toList $ HashMap.fromListWith (+) dealt)
          thwarts <- concat <$> traverse (\(scheme,amount) -> capture $ resolveAs ident $
            ThwartScheme (schemeRef scheme) source amount)
            (HashMap.toList $ HashMap.fromListWith (+) removed)
          let messages = simultaneousDamage damages
              combine (Simultaneously group) = Simultaneously (thwarts <> group)
              combine message = message
          if null damages then push $ Simultaneously thwarts else pushAll $ map combine messages
        [enemy] -> assign enemy n
        _ -> chooseOnePrompted ident "$label.assignDividedDamage"
          [targeting (enemyRef enemy) (assign enemy 1) | enemy <- enemies]
    pure g
  AssignDividedDamage ident source damage matcher assigned -> do
    enemies <- selectList (DamageableEnemy <> matcher)
    let n = damageAmount damage
        assign e k = push $ AssignDividedDamage ident source damage {damageAmount = n - k} matcher ((e, k) : assigned)
    case enemies of
      _ | n == 0 || null enemies -> do
        pushAll . simultaneousDamage
          =<< traverse
            (\(e, k) -> capture $ resolveAs ident $ DamageEnemy (enemyRef e) source damage {damageAmount = k})
            (HashMap.toList $ HashMap.fromListWith (+) assigned)
        when (damageSource damage == FromPlayerAttack ident && source == IdentityRef ident) $
          checkWindows [W.Window After $ W.PlayerAttackResolved source ident (nub $ map fst assigned)]
      [e] -> assign e n
      _ -> chooseOnePrompted ident "$label.assignDividedDamage" [targeting (enemyRef e) (assign e 1) | e <- enemies]
    pure g
  -- divided threat removal: a point at a time among the schemes matching, none past a
  -- scheme's threat, then each scheme given a share thwarted (Wasp rulesheet)
  CardSpecific GameRef "assignBasicThwartWithStat" (fromJSON -> Success (ident, source :: Source, usingAttack :: Bool, normal :: Natural, remaining :: Natural, assigned :: [(SchemeId,Natural)], granted :: Natural)) -> do
    schemes <- selectList (SchemeAHeroCanBasicThwart ident)
    let given scheme = sum [n | (target,n) <- assigned, target == scheme]
    open <- forMaybeM schemes $ \scheme -> do
      threat <- selectCount SchemeThreat (SchemeWithId scheme)
      conditional <- if usingAttack then pure normal else getCharacterThwartAgainstScheme (IdentityCharacter ident) scheme
      let extra = if usingAttack then 0 else subtractNatural normal conditional
          increment = subtractNatural granted extra
          room = subtractNatural (given scheme) threat
      pure $ guard (room > 0 && remaining + increment > 0) $> (scheme, room, increment)
    let finish = for_ (HashMap.toList $ HashMap.fromListWith (+) assigned) $ \(scheme,n) ->
          resolveAs ident $ BasicThwartScheme (schemeRef scheme) source n
        assign scheme amount increment = cardSpecific GameRef "assignBasicThwartWithStat"
          (ident, source, usingAttack, normal, remaining + increment - amount, (scheme,amount) : assigned, granted + increment)
    case open of
      [] -> finish
      [(scheme,room,increment)] | remaining > 0 -> assign scheme (min (remaining + increment) room) increment
      _ -> chooseOnePrompted ident "$label.assignDividedThreat" $
        [targeting (schemeRef scheme) (assign scheme 1 increment) | (scheme,_,increment) <- open]
          <> [labeled "$label.done" finish | remaining == 0]
    pure g
  AssignDividedThreat ident source basic upTo n matcher assigned -> do
    labeledThwart <- elem Ability.Thwart <$> sourceAbilitySubTypes source
    let given s = sum [k | (s', k) <- assigned, s' == s]
    sourceModifiers <- getModifiers source
    rawPatrolled <- isPatrolled ident
    let patrolled = rawPatrolled && IgnoresPatrol `notElem` sourceModifiers && not (basic && BasicThwartIgnoresPatrol `elem` sourceModifiers)
    -- a thwart, the main scheme not while a minion with patrol is engaged (RR Patrol)
    forbidden <- elem CannotThwartSideSchemes <$> getModifiers (IdentityRef ident)
    let thwartable = \case
          SchemeMainSchemeId sid -> do
            restricted <- isRestrictedMainScheme sid
            pure $ not (restricted && patrolled && (basic || labeledThwart))
          SchemeSideSchemeId _ -> pure $ not (forbidden && (basic || labeledThwart))
    schemes <- filterM (\scheme -> (&&) <$> thwartable scheme <*> canRemoveThreatFrom source scheme)
      =<< selectList (matcher <> if basic || labeledThwart then SchemeWithoutThwartPrevention else AnyScheme)
    open <- forMaybeM schemes \s -> do
      threat <- selectCount SchemeThreat (SchemeWithId s)
      let room = subtractNatural (given s) threat
      pure $ guard (room > 0) $> (s, room)
    let assign s k = push $ AssignDividedThreat ident source basic upTo (n - k) matcher ((s, k) : assigned)
        done = AssignDividedThreat ident source basic upTo 0 matcher assigned
    case open of
      _ | n == 0 || null open ->
        for_ (HashMap.toList $ HashMap.fromListWith (+) assigned) \(s, k) ->
          resolveAs ident $ (if basic then BasicThwartScheme else ThwartScheme) (schemeRef s) source k
      [(s, room)] | not upTo -> assign s (min n room)
      _ ->
        chooseOnePrompted ident "$label.assignDividedThreat" $
          [targeting (schemeRef s) (assign s 1) | (s, _) <- open]
            <> [labeled "$label.done" (push done) | upTo]
    pure g
  RevealedEncounterCard ident card -> resolveEncounterReveal True W.RevealedFromEncounterDeck ident card g
  RevealedEncounterCardWithoutWhenRevealed ident card -> resolveEncounterReveal False W.RevealedFromEncounterDeck ident card g
  RevealedEncounterCardFrom source ident card -> resolveEncounterReveal True source ident card g
  RevealedEncounterCardWithoutWhenRevealedFrom source ident card -> resolveEncounterReveal False source ident card g
  AttachEncounterCard card enemy -> do
    let attachment = createAttachment card
    attachTo (toId attachment) enemy
    pure $ g & (entitiesL . attachmentsL %~ insertMap (toId attachment) attachment) & (boostCardsL %~ HashMap.filter (/= card))
  -- a boost card shuffled into the encounter deck is not discarded with the others
  ShuffleIntoEncounterDeck cards -> pure $ g
    {gameRemovedFromGameEncounterCards = filter (`notElem` cards) gameRemovedFromGameEncounterCards}
    & boostCardsL %~ HashMap.filter (`notElem` cards)
  PutEncounterCardIntoPlay _ card | isJust (inPlayRef g $ EncounterCard card) -> pure g
  PutEncounterCardIntoPlay ident card -> case cdCardType (getCardDef card) of
    SideSchemeType -> do
      let scheme = createSideScheme card
      sideSchemeStep (toId scheme) SideSchemePlaceInitialThreat
      checkWindows [W.Window After $ W.SideSchemeEnteredPlay (toId scheme)]
      pure $ g & entitiesL . sideSchemesL %~ insertMap (toId scheme) scheme
        & removedEntitiesL . sideSchemesL %~ deleteMap (toId scheme)
        & intoAreaOf ident (\a -> a {areaSideSchemes = areaSideSchemes a <> [toId scheme]})
    MinionType -> do
      let
        minion = createMinion ident card
      checkWindows [W.Window When $ W.MinionEnteredPlay $ toId minion]
      rest <- capture do
        cardSpecific (MinionRef $ toId minion) "applyToughness" ()
        engageMinion (toId minion) ident
        checkWindows [W.Window After $ W.MinionEnteredPlay $ toId minion]
      push $ EntersPlayUnlessUnique ident False (toTarget minion) rest
      pure $ g & (entitiesL . minionsL %~ insertMap (toId minion) minion)
        & removedEntitiesL . minionsL %~ deleteMap (toId minion)
        & attacksThisTurnL %~ filter ((/= EnemyMinionId (toId minion)) . snd)
    -- put into play, not revealed: no surge
    EnvironmentType -> do
      let environment = createEnvironment card
      spellOwnedBy ident environment
      environmentStep (toId environment) EnvironmentEntered
      pure $ g & environmentIntoPlay ident environment
    -- an encounter support, as the Milano, is put into play under the player's control,
    -- its card owned by no player
    kind | kind `elem` [SupportType,UpgradeType] -> do
      g <$ push (PutCardIntoPlay ident (MkPlayerCard (ecCardId card) (ecCardDef card) Nothing (Just ident)) NoPayment Nothing False)
    -- attached as its own "attach to" says, not revealed: no "When Revealed" and no surge
    AttachmentType -> do
      let attachment = createAttachment card
      rest <- capture $ do
        attachmentStep (toId attachment) (RevealAttachment ident)
        checkWindows [W.Window W.After $ W.EncounterAttachmentEntered $ toId attachment]
      push $ EntersPlayUnlessUnique ident False (toTarget attachment) rest
      pure $ g & (entitiesL . attachmentsL %~ insertMap (toId attachment) attachment)
    _ -> do
      revealEncounterCardWithoutWindows ident card
      pure g
  CardSpecific GameRef "aosControlledMinion" (fromJSON -> Success ident) -> case lookup ident (gamePlayers g) of
    Nothing -> pure g
    Just player -> do
      let
        attrs = toAttrs player
        reshuffle = resetDeck ident
      case unDeck (playerIdentityDeck attrs) of
        []
          | null (unDiscard $ playerIdentityDiscard attrs) -> pure g
          | otherwise -> do
              reshuffle
              cardSpecific GameRef "aosControlledMinion" ident
              pure g
        card : rest -> do
          let
            minionId = MinionId $ unCardId $ pcCardId card
            drone =
              overMinion (facedownCardL ?~ card) $
                lookupMinion "aosControlled" ident minionId
          removeFromGame (CardIdRef $ pcCardId card)
          engageMinion minionId ident
          checkWindows [W.Window After $ W.MinionEnteredPlay minionId]
          when (null rest) reshuffle
          pure $ g & (entitiesL . minionsL %~ insertMap minionId drone)
  PutTopOfDeckIntoPlayAsDrone ident -> case lookup ident (gamePlayers g) of
    Nothing -> pure g
    Just player -> do
      let
        attrs = toAttrs player
        reshuffle = resetDeck ident
      case unDeck (playerIdentityDeck attrs) of
        []
          | null (unDiscard $ playerIdentityDiscard attrs) -> pure g
          | otherwise -> do
              reshuffle
              putTopOfDeckIntoPlayAsDrone ident
              pure g
        card : rest -> do
          let
            minionId = MinionId $ unCardId $ pcCardId card
            drone =
              overMinion (facedownCardL ?~ card) $
                lookupMinion "xdrone" ident minionId
          removeFromGame (CardIdRef $ pcCardId card)
          engageMinion minionId ident
          checkWindows [W.Window After $ W.MinionEnteredPlay minionId]
          when (null rest) reshuffle
          pure $ g & (entitiesL . minionsL %~ insertMap minionId drone)
  -- an ally flipped to its other face: the new face's entity, what was on the card
  -- kept (RR Flip)
  AllyMessage aid FlipAlly | Just ally <- lookup aid (gameAllies g) -> do
    let a = toAttrs ally
        ident = getAllyController ally
        flipped = lookupAlly (flipCardCode $ toCardCode a) ident aid
    allyStep aid (AllyOwnedBy $ allyOwner a)
    allyStep aid (AllyPaidWith $ Ally.allyPaidWith a)
    when (Ally.allyDamage a > 0) $ allyStep aid (AllySetDamage $ Ally.allyDamage a)
    when (Ally.allyExhausted a) $ exhaust aid
    when (Ally.allyCounters a > 0) $ push $ PlaceCounters (AllyRef aid) (Ally.allyCounters a)
    for_ [(Ally.allyStunned a, StunnedStatus), (Ally.allyConfused a, ConfusedStatus), (Ally.allyTough a, ToughStatus)] \(on, s) ->
      when on $ push $ GiveStatus (AllyRef aid) s
    for_ (allyUpgrades a) (allyStep aid . AttachedUpgradeToAlly)
    pure $ g & (entitiesL . alliesL %~ insertMap aid flipped)
  -- Hela (and Enchantress): an ally treated as a minion
  PutAllyIntoPlayAsMinion ident card -> do
    let mid = MinionId $ unCardId $ pcCardId card
    kept <- matchesUniqueInPlay ident (MinionRef mid) (getCardDef card)
    if kept
      then g <$ push (KeptOutByUnique (MinionRef mid))
      else do
        removeFromGame (CardIdRef $ pcCardId card)
        engageMinion mid ident
        checkWindows [W.Window After $ W.MinionEnteredPlay mid]
        pure $ g & (entitiesL . minionsL %~ insertMap mid (allyAsMinion ident card (Ally.toAttrs $ createAlly ident card)))
  CardSpecific GameRef "neAttachDefeatedMinion" (fromJSON -> Success (mid, aid)) -> do
    case (lookup (mid :: MinionId) (gameMinions g), lookup (aid :: AllyId) (gameAllies g)) of
      (Just source, Just _) -> do
        minionDisengages (minionEngagedIdentity $ toAttrs source) mid
        allyBecomesMinion aid
        cardSpecific GameRef "neMarkPossessedHost" (mid, MinionId $ unAllyId aid)
        pure $ g & entitiesL . minionsL . ix mid %~ overMinion (\a -> a {Minion.minionAttachedTo = Just $ MinionId $ unAllyId aid})
      _ -> g <$ minionStep mid MinionLeavesPlay
  CardSpecific GameRef "neMarkPossessedHost" (fromJSON -> Success (_mid :: MinionId, host :: MinionId)) ->
    pure $ g & entitiesL . minionsL . ix host %~ overMinion (\a -> a
      {Minion.minionCardDef = (Minion.minionCardDef a) {cdTraits = HashSet.insert Possessed (cdTraits $ Minion.minionCardDef a)}})
  AllyBecomesMinion aid -> case lookup aid (gameAllies g) of
    Nothing -> pure g
    Just ally -> do
      let a = toAttrs ally
          ident = getAllyController ally
          mid = MinionId $ unAllyId aid
          card = MkPlayerCard (CardId $ unAllyId aid) (Ally.allyCardDef a) (Just $ allyOwner a) (Just ident)
          attachments = HashSet.fromList [toId at | at <- HashMap.elems (gameAttachments g),
            Attachment.attachmentAlly (toAttrs at) == Just aid]
      -- Treating the ally as a minion does not make its physical card leave play;
      -- Attach To is checked when attached, not when its host's type changes.
      for_ (allyUpgrades a) $ \uid -> upgradeStep uid (UpgradeHostBecameMinion mid)
      for_ attachments $ \at -> attachmentStep at (AttachmentHostBecameMinion mid)
      identityStep ident (AllyRemoved aid)
      engageMinion mid ident
      let minion = overMinion (\m -> m
            { Minion.minionOriginalAlly = Just $ toJSON ally
            , Minion.minionOriginalCard = Just $ toCard ally
            , Minion.minionSchemeAvailable = case ally of Ally.Ally inner -> Ally.allyHasThwart inner
            , Minion.minionAttackAvailable = case ally of Ally.Ally inner -> Ally.allyHasAttack inner
            , Minion.minionStunned = Ally.allyStunned a
            , Minion.minionConfused = Ally.allyConfused a
            , Minion.minionTough = Ally.allyTough a
            , Minion.minionAdditionalStun = Ally.allyAdditionalStun a
            , Minion.minionAdditionalConfuse = Ally.allyAdditionalConfuse a
            , Minion.minionAdditionalTough = Ally.allyAdditionalTough a
            , Minion.minionAttachments = attachments
            }) (allyAsMinion ident card a)
      pure $ g & (entitiesL . alliesL %~ deleteMap aid) & (entitiesL . minionsL %~ insertMap mid minion)
        & \game -> game {gameCharacterThreat=[(if character == AllyCharacter aid then MinionCharacter mid else character,n) | (character,n) <- game.gameCharacterThreat]}
  TakeControlOfMinion controller mid -> case lookup mid (gameMinions g) of
    Nothing -> pure g
    Just minion -> do
      let m = toAttrs minion
          aid = AllyId $ unMinionId mid
          printed = Minion.minionCardDef m
          def = printed {cdCardType = AllyType, cdKeywords = if toCardCode printed == "60169" then cdKeywords printed else mempty,
            cdTraits = HashSet.insert Controlled (cdTraits printed)}
          base = toAttrs $ lookupAlly (if toCardCode printed == "60169" then "60169" else "xcontrolledminion") controller aid
          a = base
            { Ally.allyCardDef = def
            , Ally.allyExhausted = Minion.minionExhausted m
            , Ally.allyOwner = case toCard minion of
                PlayerCard card -> fromMaybe controller (pcOwner card)
                EncounterCard _ -> controller
            , Ally.allyDamage = Minion.minionDamage m
            , Ally.allyCounters = Minion.minionCounters m
            , Ally.allyHitPoints = Minion.minionHitPoints m
            , Ally.allyThwart = Thw $ unSch $ Minion.minionScheme m
            , Ally.allyAttack = Minion.minionAttack m
            , Ally.allyAdditionalStun = Minion.minionAdditionalStun m
            , Ally.allyAdditionalConfuse = Minion.minionAdditionalConfuse m
            , Ally.allyAdditionalTough = Minion.minionAdditionalTough m
            , Ally.allyStunned = Minion.minionStunned m
            , Ally.allyConfused = Minion.minionConfused m
            , Ally.allyTough = Minion.minionTough m
            , Ally.allyUpgrades = minionUpgrades m
            , Ally.allyHeldCards = Minion.minionHeldCards m
            , Ally.allyHeldTreacheries = Minion.minionHeldTreacheries m
            , Ally.allyOriginalCard = Just $ toCard minion
            , Ally.allyOriginalMinion = Just $ toJSON minion
            }
      cancelEnemyActivation mid
      for_ (minionUpgrades m) $ \uid -> upgradeStep uid (UpgradeAttachedToAlly aid)
      for_ (minionAttachments m) $ \at -> attachmentStep at (AttachmentHostBecameAlly aid)
      minionDisengages (minionEngagedIdentity m) mid
      identityStep controller (AllyCreated aid)
      pure $ g & (entitiesL . minionsL %~ deleteMap mid)
        & (entitiesL . alliesL %~ insertMap aid (if toCardCode printed == "60169" then setBlackCatAllyAttrs controller aid a else Ally.Ally $ ControlledMinion a))
        & \game -> game {gameCharacterThreat = [(if character == MinionCharacter mid then AllyCharacter aid else character, n) | (character,n) <- game.gameCharacterThreat]}
  RestoreControlledMinion aid -> case lookup aid (gameAllies g) of
    Just ally | Just original <- Ally.allyOriginalMinion (toAttrs ally) -> case fromJSON original of
      Error err -> error $ "invalid controlled minion snapshot: " <> err
      Success (minion :: Minion) -> do
        let a = toAttrs ally
            mid = MinionId $ unAllyId aid
            controller = getAllyController ally
            attachments = HashSet.fromList [toId at | at <- HashMap.elems (gameAttachments g),
              Attachment.attachmentAlly (toAttrs at) == Just aid]
            restored = overMinion (\m -> m
              { Minion.minionExhausted = Ally.allyExhausted a
              , Minion.minionDamage = Ally.allyDamage a
              , Minion.minionAdditionalStun = Ally.allyAdditionalStun a
              , Minion.minionAdditionalConfuse = Ally.allyAdditionalConfuse a
              , Minion.minionAdditionalTough = Ally.allyAdditionalTough a
              , Minion.minionStunned = Ally.allyStunned a
              , Minion.minionConfused = Ally.allyConfused a
              , Minion.minionTough = Ally.allyTough a
              , Minion.minionUpgrades = allyUpgrades a
              , Minion.minionAttachments = attachments
              , Minion.minionHeldCards = Ally.allyHeldCards a
              , Minion.minionCounters = Ally.allyCounters a
              , Minion.minionHeldTreacheries = Ally.allyHeldTreacheries a
              , Minion.minionEngagedIdentity = controller
              }) minion
        for_ (allyUpgrades a) $ \uid -> upgradeStep uid (UpgradeHostBecameMinion mid)
        for_ attachments $ \at -> attachmentStep at (AttachmentHostBecameMinion mid)
        identityStep controller (AllyRemoved aid)
        -- Losing ally-only hit points can defeat it before engagement resolves.
        -- A surviving restored minion engages normally, including quickstrike,
        -- without entering play or resolving When Revealed again.
        identityStep controller (MinionEngaged mid)
        minionStep mid MinionCheckHitPoints
        engageMinionIfInPlay mid controller
        pure $ g & (entitiesL . alliesL %~ deleteMap aid)
          & (entitiesL . minionsL %~ insertMap mid restored)
          & \game -> game {gameCharacterThreat = [(if character == AllyCharacter aid then MinionCharacter mid else character,n) | (character,n) <- game.gameCharacterThreat]}
    _ -> pure g
  CardSpecific GameRef "engageMinionIfInPlay" (fromJSON -> Success (mid, you)) -> do
    when (HashMap.member mid $ gameMinions g) $ engageMinion mid you
    pure g
  MinionBecomesAlly mid -> do
    controls <- filterM (fmap not . isBlanked)
      [u | u <- HashMap.elems (gameUpgrades g), toCardCode (getCardDef u) == "34009",
        upgradeAttachedEnemy (toAttrs u) == Just (EnemyMinionId mid)]
    case controls of
      control : _ -> g <$ takeControlOfMinion (getUpgradeController control) mid
      [] -> case lookup mid (gameMinions g) of
        Just minion | Just card <- Minion.minionFacedownCard (toAttrs minion) -> do
          let a = toAttrs minion
              owner = fromMaybe (minionEngagedIdentity a) (pcOwner card)
              controller = fromMaybe owner (pcController card)
              original = case Minion.minionOriginalAlly a of
                Nothing -> createAlly controller card
                Just value -> case fromJSON value of
                  Success saved -> saved
                  Error err -> error $ "invalid converted ally snapshot: " <> err
              originalAttrs = toAttrs original
              ally = setAllyAttrs original originalAttrs
                { Ally.allyExhausted=Minion.minionExhausted a
                , Ally.allyDamage=Minion.minionDamage a
                , Ally.allyStunned=Minion.minionStunned a
                , Ally.allyConfused=Minion.minionConfused a
                , Ally.allyTough=Minion.minionTough a
                , Ally.allyAdditionalStun=Minion.minionAdditionalStun a
                , Ally.allyAdditionalConfuse=Minion.minionAdditionalConfuse a
                , Ally.allyAdditionalTough=Minion.minionAdditionalTough a
                , Ally.allyCounters=Minion.minionCounters a
                , Ally.allyHeldCards=Minion.minionHeldCards a
                , Ally.allyHeldTreacheries=Minion.minionHeldTreacheries a
                , Ally.allyUpgrades=minionUpgrades a
                }
              aid = toId ally
          for_ (minionUpgrades a) $ \uid -> upgradeStep uid (UpgradeAttachedToAlly aid)
          for_ (minionAttachments a) $ \at -> attachmentStep at (AttachmentHostBecameAlly aid)
          minionDisengages (minionEngagedIdentity a) mid
          -- This is still the same physical card in play. Its facedown treacheries
          -- remain attached, rather than being discarded by the change of allegiance.
          identityStep controller (AllyCreated aid)
          pure $ g & (entitiesL . minionsL %~ deleteMap mid) & (entitiesL . alliesL %~ insertMap aid ally)
            & \game -> game {gameCharacterThreat=[(if character == MinionCharacter mid then AllyCharacter aid else character,n) | (character,n) <- game.gameCharacterThreat]}
        _ -> pure g
  EndCheckWindows -> pure g
  EntersPlayUnlessUnique ident revealed target rest -> do
    let def = case target of
          MinionRef k -> getCardDef <$> lookup k (gameMinions g)
          AttachmentRef k -> getCardDef <$> lookup k (gameAttachments g)
          _ -> Nothing
    matched <- maybe (pure False) (matchesUniqueInPlay ident target) def
    if matched
      then do
        push $ KeptOutByUnique target
        removeFromPlay target
        when revealed $ dealEncounterCard ident
      else pushAll rest
    pure g
  KeptOutByUnique _ -> pure g
  IdentityEndedTurn ident ->
    pure $
      g
        & usedAbilitiesL . ix ident
          %~ filter
            ((/= PerTurn 1) . baseLimit . abilityLimit . fst)
  IdentityMessage ident (PlayedCard card _) ->
    pure $
      g
        & playedInWindowsL . each <>~ [getCardDef card]
        & playedThisRoundL <>~ [(ident, getCardDef card)]
        & playedThisPhaseL <>~ [(ident, getCardDef card)]
        & playedThisTurnL <>~ [(ident, getCardDef card)]
  IdentityMessage _ BeginTurn -> pure $ g & playedThisTurnL .~ [] & attacksThisTurnL .~ []
  EndRound ->
    pure $
      g
        & roundL +~ 1
        & playedThisRoundL .~ []
        & usedAbilitiesL . each
          %~ filter
            ((/= PerRound 1) . baseLimit . abilityLimit . fst)
  GameOver status -> do
    clearQueue
    pure $ g & stateL .~ Finished status
  -- A player card treated as a minion still returns to its owner's hand. Remove
  -- its entity before discarding its attachments so their expiring type changes
  -- cannot recreate an ally that has already left play.
  ReturnToHand target@(MinionRef mid) -> case lookup mid (gameMinions g) of
    Nothing -> pure g
    Just minion -> case toCard minion of
      EncounterCard _ -> g <$ push (ResolveMinionLeavingPlay mid $ RemoveFromPlay target)
      PlayerCard card -> do
        modifiers <- getModifiers target
        if RemovedFromGameInsteadOfLeavingPlay `elem` modifiers
          then g <$ removeFromGame target
          else do
            let attrs = toAttrs minion
                owner = fromMaybe (minionEngagedIdentity attrs) (pcOwner card)
            cancelEnemyActivation mid
            frameworkDiscard $ for_ (minionUpgrades attrs) removeFromPlay
            frameworkDiscard $ for_ (minionAttachments attrs) removeFromPlay
            for_ (Minion.minionHeldCards attrs) discardCard
            for_ (Minion.minionHeldTreacheries attrs) discardCard
            minionDisengages (minionEngagedIdentity attrs) mid
            addToHand owner card {pcController = Just owner}
            pure $ g & (entitiesL . minionsL %~ deleteMap mid)
              & (removedEntitiesL . minionsL %~ insertMap mid minion)
  ReturnToHand target@(AllyRef aid)
    | Just ally <- lookup aid (gameAllies g), EncounterCard _ <- toCard ally ->
      g <$ removeFromPlay target
  ReturnToHand target -> case target of
    -- to its owner's hand
    AllyRef aid -> do
      removed <- elem RemovedFromGameInsteadOfLeavingPlay <$> getModifiers target
      if removed then g <$ push (RemoveFromGame target) else do
        for_ (lookup aid $ gameAllies g) $ \ally -> do
          let controller = getAllyController ally
              owner = allyOwner (toAttrs ally)
          frameworkDiscard $ for_ (allyUpgrades $ toAttrs ally) removeFromPlay
          for_ (Ally.allyHeldCards $ toAttrs ally) discardCard
          for_ (Ally.allyHeldTreacheries $ toAttrs ally) discardCard
          identityStep controller (AllyRemoved aid)
          let card = case Ally.allyOriginalCard (toAttrs ally) of
                Just (PlayerCard original) -> original {pcController = Just owner}
                _ -> MkPlayerCard (CardId $ unAllyId aid) (getCardDef ally) (Just owner) (Just owner)
          addToHand owner card
        pure $ g & (entitiesL . alliesL %~ deleteMap aid)
          & (removedEntitiesL . alliesL %~ maybe id (insertMap aid) (lookup aid $ gameAllies g))
    SupportRef sid | maybe False (elem Permanent . cdKeywords . supportCardDef . toAttrs) (lookup sid $ gameSupports g) -> pure g
    SupportRef sid -> do
      for_ (lookup sid $ gameSupports g) \support -> do
        let owner = supportOwner $ toAttrs support
        for_ (supportHeldCards $ toAttrs support) discardCard
        identityStep (getSupportController support) (SupportRemoved sid)
        addToHand owner $ MkPlayerCard (CardId $ unSupportId sid) (supportCardDef $ toAttrs support) (Just owner) (Just owner)
      pure $ g & (entitiesL . supportsL %~ deleteMap sid)
          & (removedEntitiesL . supportsL %~ maybe id (insertMap sid) (lookup sid $ gameSupports g))
    -- to its owner's hand
    UpgradeRef uid | maybe False isPermanent (lookup uid $ gameUpgrades g) -> pure g
    UpgradeRef uid -> do
      for_ (lookup uid $ gameUpgrades g) $ \upgrade -> do
        let owner = upgradeOwner $ toAttrs upgrade
        upgradeRemoved uid
        for_ (upgradeHeldCards $ toAttrs upgrade) discardCard
        addToHand owner $ MkPlayerCard (CardId $ unUpgradeId uid) (getCardDef upgrade) (Just owner) (Just owner)
      pure $ g & (entitiesL . upgradesL %~ deleteMap uid)
          & (removedEntitiesL . upgradesL %~ maybe id (insertMap uid) (lookup uid $ gameUpgrades g))
    -- a played event goes back from its controller's discard pile, if it is still there
    EventRef eid -> do
      for_ (lookup eid $ gameEvents g) \event -> do
        let controller = eventController (toAttrs event)
        for_ [c | PlayerCard c <- [toCard (toAttrs event)]] \c -> do
          let inDiscard = maybe False (elem c . unDiscard . playerIdentityDiscard . toAttrs) (lookup controller $ gamePlayers g)
          when inDiscard $ addToHand controller c
      pure g
    _ -> error "unhandled"
  PendingActivations [(_, msgs)] -> g <$ pushAll msgs
  PendingActivations pending -> do
    first <- selectJust FirstPlayer
    chooseOneAtATimePrompted first "$label.chooseActivationOrder" [targeting v (pushAll msgs) | (v, msgs) <- pending]
    pure g
  -- the token goes where a redirect on the scheme sends it (The Master of Time), and
  -- stays in that scheme's area
  PlaceAccelerationToken ms -> do
    mods <- getModifiers (MainSchemeRef ms)
    gliders <- getGliderSchemes
    let ms' = fromMaybe ms (headMay gliders <|> lastMay [to | RedirectAccelerationTokens to <- mods, to /= ms])
        aid = maybe defaultAreaId areaId $ areaOfTarget g (MainSchemeRef ms')
    if null gliders
      then pure $ overArea aid (\a -> a {areaAccelerationTokens = areaAccelerationTokens a + 1}) g
      else g <$ mainSchemeStep ms' (AddMainSchemeAcceleration 1)
  CreateArea aid players -> pure $ g & areasL %~ (movePlayers players aid . (<> [newArea aid []]))
  -- players come together: copies of a unique card are discarded down to one
  MovePlayerToArea ident aid -> do
    push $ DiscardDuplicateUniques aid
    pure $ g & areasL %~ movePlayers [ident] aid
  CombineAreas from into -> do
    push $ DiscardDuplicateUniques into
    pure $ g & areasL %~ combine from into
  -- "When players combine game areas, they must discard copies of unique cards until
  -- only one of each remains in that game area. If the players cannot agree which one to
  -- discard, the first player decides." (TOAFK insert p.7) An identity is never
  -- discarded: a card sharing its name goes.
  DiscardDuplicateUniques aid -> do
    uniques <- getUniqueCardsIn aid
    first <- selectJust FirstPlayer
    let
      groups [] = []
      groups ((t, n) : rest) =
        let (same, other) = partition (uniqueMatch n . snd) rest
         in ((t, n) : same) : groups other
      isIdentity = \case
        IdentityRef _ -> True
        _ -> False
    for_ [map fst grp | grp@(_ : _ : _) <- groups uniques] \copies ->
      case filter isIdentity copies of
        _ : _ -> for_ (filter (not . isIdentity) copies) discardControlled
        [] -> chooseOnePrompted first "$label.keepUniqueCopy" [targeting kept (for_ (filter (/= kept) copies) discardControlled) | kept <- copies]
    pure g
  SetActiveVillain vid -> do
    aid <- getAreaOfTarget vid
    pure $ g & overArea aid \a -> a {areaActiveVillain = Just vid}
  SetSignatureScheme vid sid -> pure $ g & signatureSchemesL %~ insertMap vid sid
  FocusCards cards -> pure $ g & focusedCardsL .~ cards
  UnfocusCards -> pure $ g & focusedCardsL .~ []
  ShowCardsTo ident cards -> pure $ g & privateCardsL . at ident ?~ cards
  HideCardsFrom ident -> pure $ g & privateCardsL . at ident .~ Nothing
  -- everyone may respond, the active player first, then the others in player order;
  -- forced abilities resolve before anyone is asked
  CheckWindows windows -> do
    active <- getActivePlayerId
    let order = active : filter (/= active) gamePlayerOrder
    checkWindowsFor order windows
    pure g
  CardSpecific GameRef "checkWindowsInCurrentOccurrence" (fromJSON -> Success windows) -> do
    case gameWindows of
      [] -> checkWindows windows >> pure g
      current : rest -> do
        -- New targets belong to this attack's occurrence, but constants on
        -- those targets still observe the additional attack windows.
        after <- traverseOf entitiesL (runMessage $ CheckWindows windows) g
        active <- getActivePlayerId
        checkWindowsFor (active : filter (/= active) gamePlayerOrder) windows
        pure after {gameWindows = (current <> windows) : rest}
  CheckWindowsFor [] _ -> pure g
  -- a player eliminated while the window goes round is no longer asked
  CheckWindowsFor (ident : rest) windows | not (HashMap.member ident (gamePlayers g)) -> do
    checkWindowsFor rest windows
    pure g
  CheckWindowsFor (ident : rest) originalWindows -> do
    windows <- traverse refreshDamageInterruptWindow originalWindows
    abilities <- getAbilitiesForMatching ident (\a -> asYou ident $ anyM (`abilityInWindow` a) windows)
    attrs <- getIdentity ident
    -- a treachery's reveal, or an encounter card's, is cancelled by taking it off the
    -- queue: its window then closes, so a second cancel is not offered
    -- likewise damage prevented in full: nothing more answers it
    let attackQueued enemy = isQueued \case
          VillainMessage vid (VillainBeginAttack _) -> enemy == EnemyVillainId vid
          VillainMessage vid (VillainBeginAttackWithoutBoost _) -> enemy == EnemyVillainId vid
          MinionMessage mid (MinionBeginAttack _) -> enemy == EnemyMinionId mid
          _ -> False
        open = \case
          W.Window W.When (W.IdentityAttackTarget you enemy) -> isQueued (\case
            VillainMessage vid (VillainDamaged _ damage) -> enemy == EnemyVillainId vid && damageSource damage == FromPlayerAttack you
            MinionMessage mid (MinionDamaged _ damage) -> enemy == EnemyMinionId mid && damageSource damage == FromPlayerAttack you
            EventMessage _ (EventAttacked who target _) -> who == you && target == enemy
            _ -> False)
          W.Window W.When (W.UsedRecovery iid) -> isQueued (\case IdentityMessage iid' (SideMessage Recovered) -> iid == iid'; IdentityMessage iid' (SideMessage (RecoveredWithBonus _)) -> iid == iid'; _ -> False)
          W.Window W.Would (W.MinionLeavingPlay mid) -> isQueued \case
            ResolveMinionLeavingPlay mid' _ -> mid == mid'
            _ -> False
          W.Window W.Would (W.IdentityDefeat iid) -> isQueued (== IdentityMessage iid IdentityDefeated)
          W.Window W.Would (W.IdentityDefeatFromDamage iid _ _) -> isQueued (== IdentityMessage iid IdentityDefeated)
          W.Window W.Would (W.UpgradeReadying uid) -> isQueued (== UpgradeMessage uid ReadiedUpgrade)
          -- an attack replaced in its would window (Webbed Up) is no longer imminent,
          -- so nothing more answers it (RR "Replacement Effect")
          W.Window W.Would (W.EnemyAttack enemy _) -> attackQueued enemy
          W.Window W.Would (W.EnemyAttackAlly enemy _ _) -> attackQueued enemy
          W.Window W.Would (W.EnemyAttackOnEnemy enemy _) -> attackQueued enemy
          W.Window W.Would (W.CharacterRemovingThreat _ scheme _) -> isQueued \message ->
            maybe False (\(_,target,n,_,_) -> target == scheme && n > 0) (threatRemovalMessage message)
          W.Window W.Would (W.CharacterPlacingThreat _ scheme _) -> isQueued \case
            CardSpecific GameRef key (fromJSON -> Success (_ :: Source,_ :: W.ThreatSource,target :: SchemeId,n :: Natural))
              | key `elem` ["placeThreatAfterCharacterWindow","placeReportingThreatAfterCharacterWindow","placeVillainThreatAfterCharacterWindow"] -> target == scheme && n > 0
            _ -> False
          W.Window W.Would (W.ThreatPlaced _ scheme _) -> isQueued \case
            MainSchemeMessage sid (MainSchemePlaceThreat n) -> scheme == SchemeMainSchemeId sid && n > 0
            SideSchemeMessage sid (SideSchemePlaceThreat n) -> scheme == SchemeSideSchemeId sid && n > 0
            _ -> False
          W.Window _ (W.RevealTreachery tid _) -> isQueued \case
            TreacheryMessage tid' (RevealTreachery _) -> tid' == tid
            _ -> False
          W.Window timing (W.EncounterCardRevealed _ _ card) | timing /= W.After -> isQueued \case
            CardSpecific GameRef key (fromJSON -> Success (_ :: W.RevealSource, _ :: IdentityId, card' :: EncounterCard)) | key `elem` ["revealAfterWould", "revealAfterWouldWithoutWhenRevealed"] -> card' == card
            RevealedEncounterCardFrom _ _ card' -> card' == card
            RevealedEncounterCardWithoutWhenRevealedFrom _ _ card' -> card' == card
            RevealedEncounterCard _ card' -> card' == card
            RevealedEncounterCardWithoutWhenRevealed _ card' -> card' == card
            _ -> False
          W.Window _ (W.DamagedVillain vid _) -> isQueued $ \case
            VillainMessage vid' (VillainDamaged _ _) -> vid' == vid
            _ -> False
          W.Window _ (W.VillainDamageFrom vid _ _) -> isQueued $ \case
            VillainMessage vid' (VillainDamaged _ _) -> vid' == vid
            _ -> False
          W.Window _ (W.VillainGainingStatus vid status) -> isQueued $ \case
            CardSpecific (VillainRef other) "gainVillainStatus" (fromJSON -> Success pending) -> vid == other && status == pending
            _ -> False
          W.Window _ (W.IdentityTakeDamage iid _) -> isQueued \case
            IdentityMessage iid' (IdentityDamaged _ _) -> iid' == iid
            _ -> False
          W.Window _ (W.IdentityTakeDamageFrom iid _ _) -> isQueued \case
            IdentityMessage iid' (IdentityDamaged _ _) -> iid' == iid
            _ -> False
          W.Window _ (W.WouldBeDiscardedFromPlay card) ->
            isQueued (== DiscardedCard (either PlayerCard EncounterCard card))
          _ -> pure True
    openWindows <- filterM open windows
    -- "you" on what is offered is the player it is offered to; while another player
    -- resolves a card with peril, no card is played (RR Peril)
    let perilled = maybe False (/= ident) gamePeril
    playableCards <-
      if perilled then pure [] else asYou ident $ concatMapM
        (\w -> map (,Just w) <$> getWindowPlayableCards w attrs)
        openWindows
    validAbilities <-
      asYou ident $ filterM
        ( andM
            . sequence
              [ \a -> anyM (`abilityInWindow` a) openWindows
              , withinUseLimit ident
              , passesCriteria ident
              , passesCanAffordCost ident
              ]
        )
        abilities
    -- after a response the window is open again to everyone
    active <- getActivePlayerId
    let order = active : filter (/= active) gamePlayerOrder
    again <- Run <$> capture (checkWindowsFor order windows)
    let forcedAbiltiies = filter isForcedAbility validAbilities
        respond a = pure $ UseAbility (a & choicesL <>~ [again])
    case (forcedAbiltiies, validAbilities, playableCards) of
      ([], [], []) -> checkWindowsFor rest windows
      ([], as, cs) ->
        chooseOnePrompted ident "$label.respond" $
          labeled "$label.noResponse" (checkWindowsFor rest windows)
            : map respond as
              <> [pure $ TargetLabel (toRef c) [PlayCard c mwindow, again] | (c, mwindow) <- cs]
      ([keyword],_,_) | engineAttackResponse keyword -> do
        resolveAs ident $ UseAbility (keyword & choicesL <>~ [again])
      (forced, _, _) -> chooseOnePrompted ident "$label.resolveForced" $ map respond forced
    let updated window = fromMaybe window $ headMay [new | (old,new) <- zip originalWindows windows,old == window]
    pure g {gameWindows = case gameWindows of [] -> []; current : older -> map updated current : older}
  DeclareDefense ident enemyId defensePriority -> do
    open <- selectAny (EnemyWithId enemyId <> UndefendedEnemy)
    forbidden <- elem AttacksCannotBeDefended <$> getModifiers (enemyRef enemyId)
    unless forbidden $ case defendingIdentity g enemyId of
      Just defender -> offerClaimedBasicDefense defender enemyId
      Nothing -> when open $ declareDefenseWithout ident enemyId defensePriority []
    pure g
  -- A defense event can claim the attack before basic defense is declared. That
  -- same identity may still exhaust for its basic DEF; the defender cannot change.
  DeclareDefenseWithout _ enemyId _ declined | Just defender <- defendingIdentity g enemyId -> do
    unless (defender `elem` declined) $ offerClaimedBasicDefense defender enemyId
    pure g
  -- each player decides for themselves whether a character they control defends (RR
  -- "Defend"): the attacked player first, who may ask another player before deciding;
  -- once they decline, each other player in turn is asked
  -- "you must defend with an ally you control, if able" (Melter, Ramming Speed): the
  -- attacked player chooses one of their allies able to defend, and may not decline, nor
  -- may another player defend instead; with none able, the attack is defended as any other
  DeclareDefenseWithout ident enemyId AllyIfAble [] -> do
    forbidden <- elem AttacksCannotBeDefended <$> getModifiers (enemyRef enemyId)
    restricted <- any (\case OnlyIdentityCanDefend _ -> True; _ -> False) <$> getModifiers (enemyRef enemyId)
    used <- pure $ maybe [] simultaneousDefenders gameSimultaneousMinionAttack
    own <- if forbidden || restricted then pure [] else filterM (canAllyDefendEnemy enemyId) =<< (filter ((`notElem` used) . AllyCharacter) <$> selectList (UnexhaustedAlly <> AllyControlledBy (IdentityWithId ident)))
    permitted <- filterM allyCanPayBasicDefense own
    if null permitted
      then declareDefenseWithout ident enemyId AnyDefense []
      else chooseOnePromptedAbout ident "$label.declareDefender" enemyId [pure (AllyDefend a enemyId) | a <- permitted]
    pure g
  DeclareDefenseWithout ident enemyId defensePriority declined -> do
    forbidden <- elem AttacksCannotBeDefended <$> getModifiers (enemyRef enemyId)
    restrictions <- getModifiers (enemyRef enemyId)
    let restricted = any (\case OnlyIdentityCanDefend _ -> True; _ -> False) restrictions
    used <- pure $ maybe [] simultaneousDefenders gameSimultaneousMinionAttack
    allies <- if forbidden || restricted then pure [] else filterM (canAllyDefendEnemy enemyId) =<< (filter ((`notElem` used) . AllyCharacter) <$> selectList UnexhaustedAlly)
    identities <- if forbidden then pure mempty else select $ HeroIdentity <> IdentityCanDefendReady
    basicAllies <- filterM allyCanPayBasicDefense allies
    controllers <- for basicAllies \a -> (a,) <$> getsGame (fmap getAllyController . lookup a . gameAllies)
    let
      defensesOf owner =
        [ if owner == ident
          then pure (Defend enemyId)
          else targeting owner $ identityStep owner (SideMessage (Defended enemyId))
        | owner `member` identities
        ]
          <> [pure (AllyDefend a enemyId) | (a, Just controller) <- controllers, controller == owner]
      decline owner = declareDefenseWithout ident enemyId defensePriority (owner : declined)
      ask owner =
        chooseOnePromptedAbout owner "$label.askedToDefend" enemyId $
          defensesOf owner <> [labeled "$label.declineToDefend" (decline owner)]
      (before, from) = break (== ident) gamePlayerOrder
      others = [o | o <- from <> before, o /= ident, o `notElem` declined, notNull (defensesOf o)]
    if ident `elem` declined
      then for_ (headMay others) ask
      else
        chooseOnePromptedAbout ident "$label.declareDefender" enemyId $
          labeled "$label.noDefender" (decline ident)
            : defensesOf ident
              <> [targeting o (ask o) | o <- others]
    pure g
  _ -> pure g

runCompetitiveEntityMessage :: (RunMessage a, HasGame m, HasQueue m, MonadThrow m, MonadRandom m) => Target -> Message -> a -> m a
runCompetitiveEntityMessage target msg entity = do
  g <- getGame
  let phaseBroadcast = case msg of
        BeginPhase _ -> True
        AccelerateMainScheme -> True
        VillainPhaseStep _ -> True
        PassFirstPlayer -> True
        _ -> False
      permitted = case gameCompetition g of
        Nothing -> True
        Just c -> not phaseBroadcast ||
          (isNothing (CW.teamOutcome c $ CW.phaseTeam $ CW.competitivePhase c) && location c == Just (CW.teamArea $ CW.phaseTeam $ CW.competitivePhase c))
      location c = case (target,msg) of
        (EffectRef _, BeginPhase _) -> Data.List.lookup target (CW.competitiveEffectAreas c) <|> (areaId <$> areaOfTarget g target)
        _ -> areaId <$> areaOfTarget g target
  if permitted then runMessage msg entity else pure entity

instance RunMessage Entities where
  runMessage msg entities =
    traverseOf (playersL . each) (\p -> asYou (toId p) $ runCompetitiveEntityMessage (IdentityRef $ toId p) msg p) entities
      >>= traverseOf (alliesL . each) (\a -> asYou (getAllyController a) do
        mission <- getsGame (\g -> missionAlly g $ toRef a)
        if mission then do
          attrs <- runMessage msg (toAttrs a)
          pure $ Ally.Ally (MissionAlly attrs)
        else runCompetitiveEntityMessage (AllyRef $ toId a) msg a)
      >>= traverseOf (supportsL . each) (\a -> asYou (getSupportController a) $ runCompetitiveEntityMessage (SupportRef $ toId a) msg a)
      >>= traverseOf (upgradesL . each) (\a -> asYou (getUpgradeController a) $ runCompetitiveEntityMessage (UpgradeRef $ toId a) msg a)
      >>= traverseOf (treacheriesL . each) (\entity -> runCompetitiveEntityMessage (TreacheryRef $ toId entity) msg entity)
      >>= traverseOf (obligationsL . each) (\entity -> runCompetitiveEntityMessage (ObligationRef $ toId entity) msg entity)
      >>= traverseOf (villainsL . each) (\entity -> runCompetitiveEntityMessage (VillainRef $ toId entity) msg entity)
      >>= traverseOf (eventsL . each) (\a -> asYou (eventController $ toAttrs a) $ runCompetitiveEntityMessage (EventRef $ toId a) msg a)
      >>= traverseOf (effectsL . each) (\entity -> runCompetitiveEntityMessage (EffectRef $ toId entity) msg entity)
      >>= traverseOf (attachmentsL . each) (\entity -> runCompetitiveEntityMessage (AttachmentRef $ toId entity) msg entity)
      >>= traverseOf (minionsL . each) (\entity -> runCompetitiveEntityMessage (MinionRef $ toId entity) msg entity)
      >>= traverseOf (sideSchemesL . each) (\scheme -> asPrintedPlayerCount (toTarget scheme) $ runCompetitiveEntityMessage (SideSchemeRef $ toId scheme) msg scheme)
      >>= traverseOf (mainSchemesL . each) (\entity -> runCompetitiveEntityMessage (MainSchemeRef $ toId entity) msg entity)
      >>= traverseOf (environmentsL . each) (\entity -> runCompetitiveEntityMessage (EnvironmentRef $ toId entity) msg entity)

instance RunMessage Game where
  runMessage original@(AllyMessage aid power) g | power `elem` [AllyAttacked,AllyThwarted],not (any ((== aid) . friendlyBoostAlly) $ g.gameFriendlyBoosts) = runQueueT do
    let attacking = power == AllyAttacked
    keyword <- case lookup aid (gameAllies g) of
      Nothing -> pure False
      Just ally -> do
        blanked <- isBlanked (AllyRef aid)
        mods <- getModifiers ally
        pure $ Villainous `elem` ([k | not blanked,k <- toList $ cdKeywords $ getCardDef ally] <> [k | KeywordModifier k <- mods])
    replaced <- characterHasCondition (if attacking then StunnedStatus else ConfusedStatus) (AllyCharacter aid)
    if keyword && not replaced then cardSpecific GameRef "beginFriendlyBoost" (aid,attacking,original) >> pure g else dispatchGameMessage original g
  runMessage original g | Just translated <- friendlyBoostMessage g original = dispatchGameMessage translated g
  runMessage (ResolveBoostAssignment original) g = dispatchGameMessage original g
  runMessage original g | Just enemy <- boostAssignmentEnemy original = runQueueT do
    checkWindows [W.Window W.Would $ W.EnemyReceivingBoost enemy]
    push $ ResolveBoostAssignment original
    pure g
  runMessage (ResolveReadiedUpgrade uid) g = dispatchGameMessage (UpgradeMessage uid ReadiedUpgrade) g
  runMessage (UpgradeMessage uid ReadiedUpgrade) g = runQueueT do
    modifiers <- getModifiers (UpgradeRef uid)
    case lookup uid (gameUpgrades g) of
      Just upgrade | upgradeExhausted (toAttrs upgrade), CannotReady `notElem` modifiers -> do
        checkWindows [W.Window W.Would $ W.UpgradeReadying uid]
        push $ ResolveReadiedUpgrade uid
      _ -> pure ()
    pure g
  runMessage (ResolveCharacterThreatRemoval original) g = dispatchGameMessage original g
  runMessage original g | Just (source,scheme,n,basic,thwarting) <- threatRemovalMessage original = do
    actor <- threatSourceCharacter source
    allowed <- characterThreatRemovalAllowed basic thwarting source scheme
    amount <- min n <$> getSchemeThreatAmount scheme
    case actor of
      Just character | allowed && amount > 0 -> runQueueT do
        checkWindows [W.Window W.Would $ W.CharacterRemovingThreat character scheme amount]
        push $ ResolveCharacterThreatRemoval original
        pure g
      _ -> dispatchGameMessage original g
  runMessage message@(CheckWindows windows) g
    | Just enemy <- headMay [enemy | W.Window W.After window <- windows, enemy <- case window of
        W.HeroDefends _ attacker -> [attacker]
        W.AllyDefendedAgainst _ attacker -> [attacker]
        _ -> []] = do
        pending <- isQueued (attackCompletionWindowFor enemy)
        if pending then do
          replaceMatchingMessage (\case CheckWindows after -> [CheckWindows $ nub $ windows <> after]; other -> [other]) (attackCompletionWindowFor enemy)
          pure g
        else dispatchGameMessage message g
  runMessage message g | Just (enemy,attacker) <- keywordRetaliation message = do
    amount <- retaliationAmount enemy
    queue <- withQueue (toSnd id)
    let (before,remaining) = break (attackCompletionFor enemy attacker) queue
    case if amount > 0 then remaining else [] of
      first : after | CheckWindows windows <- queuedMessage first -> do
        let (adjacent,rest) = span (attackCompletionFor enemy attacker) after
            joined = nub $ windows <> concat [ws | message <- adjacent, CheckWindows ws <- [queuedMessage message]] <> [W.Window W.After $ W.RetaliationPending enemy attacker]
        withQueue_ $ const $ before <> rewrapQueuedMessages first [CheckWindows joined] <> rest
        pure g
      _ -> dispatchGameMessage message g
  runMessage msg g
    | Just uid <- senseUpgradeDeparture msg
    , Just upgrade <- lookup uid (gameUpgrades g)
    , toCardCode (toAttrs upgrade) `elem` ["60002","60003","60004","60005","60006"] = do
        let owner = upgradeOwner $ toAttrs upgrade
        blanked <- isBlanked (IdentityRef owner)
        modifiers <- getModifiers (UpgradeRef uid)
        if blanked then dispatchGameMessage msg g
        else if CannotLeavePlay `elem` modifiers || CannotLeavePlayByCardAbilities `elem` modifiers then pure g
        else runQueueT do
          push $ RemoveFromPlayAfterWindow (UpgradeRef uid)
          for_ [card | PlayerCard card <- [toCard upgrade]] $ \card -> cardSpecific owner "fneSenseReturned" card
          pure g
  runMessage (IdentityMessage you step) g
    | gamePhase g == VillainPhase
    , competitivePhaseStoppedFor g you
    , step `elem` [VillainAndMinionsActivate,MinionsActivate,RevealEncounterCards] = pure g
  runMessage msg@(BeginPhase VillainPhase) g = do
    modifiers <- getModifiers GameRef
    if SkipNextVillainPhase `elem` modifiers then runQueueT do
      for_ (HashMap.elems $ gameEffects g) $ \effect ->
        when (modifierProviderAllowed g GameRef (toTarget $ toAttrs effect) && SkipNextVillainPhase `elem` Effect.effectModifiers (toAttrs effect)) (disableEffect $ toId effect)
      if isJust (gameCompetition g) then cardSpecific GameRef "cwAdvancePhase" () else endRound
      pure g
    else dispatchGameMessage msg g
  -- Forced actions finish before scenario cleanup, hand refill or phase windows.
  runMessage msg@(EndPhase PlayerPhase) g = do
    pending <- concatMapM (\you -> map (you,) <$> getAbilitiesForMatching you (\ab ->
      if abilityType ab /= Ability.ForcedAction then pure False else do
        meets <- passesCriteria you ab
        if meets then canAffordCost you (abilitySource ab) (abilityCost ab) else pure False)) =<< getPhasePlayers
    case pending of
      [] -> dispatchGameMessage msg g
      (you, _) : _ -> runQueueT $ do
        chooseOne you [pure $ UseAbility ab | (owner, ab) <- pending, owner == you]
        endPhase PlayerPhase
        pure g
  runMessage (WithSimultaneousAttackContext cursor (Ask who question)) g = runQueueT do
    push $ Ask who question
    pure $ selectSimultaneousMinionAttack cursor g
  runMessage (WithSimultaneousAttackContext _ (IdentityMessage who _)) g | not (HashMap.member who $ gamePlayers g) = pure g
  runMessage (WithSimultaneousAttackContext cursor message) g = do
    let selected = selectSimultaneousMinionAttack cursor g
    (after, following) <- runReaderT (captureQueueT $ runMessage message selected) (refreshTraits selected)
    let beganNested = case (message, gameSimultaneousMinionAttack g) of
          (MinionMessage mid step, Just group) | mid == simultaneousMinion group -> case step of
            MinionAttacks {} -> True
            MinionAttacksWithChange {} -> True
            MinionAttacksAlly {} -> True
            MinionAttacksEnemy {} -> True
            _ -> False
          _ -> False
    pushAll $ if beganNested then following else map (inSimultaneousAttackContext (WithSimultaneousAttackContext cursor message)) following
    pure after
  runMessage (WithThreatPlacementResult source original) g = do
    let scheme = case original of
          MainSchemeMessage sid (MainSchemePlaceThreat _) -> Just $ SchemeMainSchemeId sid
          SideSchemeMessage sid (SideSchemePlaceThreat _) -> Just $ SchemeSideSchemeId sid
          CardSpecific (MainSchemeRef sid) "resolveThreatPlacement" _ -> Just $ SchemeMainSchemeId sid
          _ -> Nothing
    before <- maybe (pure 0) getSchemeThreatAmount scheme
    (after,following) <- runReaderT (captureQueueT $ dispatchGameMessage original g) (refreshTraits g)
    now <- maybe (pure 0) (\target -> runReaderT (getSchemeThreatAmount target) (refreshTraits after)) scheme
    let placed = subtractNatural before now
    runQueueT $ do
      pushAll $ map (reportingThreatPlacement source) following
      for_ scheme $ \target -> when (placed > 0) $ cardSpecific source "threatPlaced" (target,placed)
    pure after
  runMessage (ResolveEnemyDamage original) g = do
    dealt <- runReaderT (enemyDamageDealtWindows original) (refreshTraits g)
    (after, following) <- runReaderT (captureQueueT $ dispatchGameMessage original g) (refreshTraits g)
    -- Generated defeat/When Defeated/departure work runs before damage's after
    -- responses (RR Damage, step 9), including when tough prevented all taking.
    pushAll $ following <> [CheckWindows dealt | notNull dealt]
    pure after
  runMessage (PrepareEnemyDamage original) g = runQueueT do
    checkWindows =<< enemyDamageTakenWindows original
    push $ ResolveEnemyDamage original
    pure g
  runMessage (PrepareEnemyDamageGroup messages cursor) g = runQueueT do
    if cursor >= length messages then push $ TakeEnemyDamageGroup messages 0 [] else do
      let original = messages !! cursor
      for_ (enemyDamageMessage original) $ \(enemy, source, damage) -> do
        barred <- elem CannotBeDealtDamage <$> getModifiers (enemyRef enemy)
        unless barred $ push $ inSimultaneousAttackContext original $ CheckWindows [W.Window W.When $ W.EnemyDamageWouldBeDealt enemy source damage]
      push $ PrepareEnemyDamageGroup messages (cursor + 1)
    pure g
  runMessage (TakeEnemyDamageGroup messages cursor deferred) g = runQueueT do
    if cursor >= length messages then do
      let (dealt,other) = partition isEnemyDealtDamageWindow deferred
      pushAll $ coalesceSimultaneousDefeats other <> dealt
    else do
      let original = messages !! cursor
          selected = messageAttackContext original g
      barred <- runReaderT (damageDealingForbidden original) (refreshTraits selected)
      if barred then push $ TakeEnemyDamageGroup messages (cursor + 1) deferred else do
        windows <- runReaderT (enemyDamageTakenWindows original) (refreshTraits selected)
        unless (null windows) $ push $ inSimultaneousAttackContext original $ CheckWindows windows
        push $ ResolveEnemyDamageGroup messages cursor deferred
    pure g
  runMessage (ResolveEnemyDamageGroup messages cursor deferred) g = do
    let original = messages !! cursor
        selected = messageAttackContext original g
    dealt <- runReaderT (enemyDamageDealtWindows original) (refreshTraits selected)
    (after, following) <- runReaderT (captureQueueT $
      if isJust (enemyDamageMessage original) then dispatchGameMessage original selected else runMessage original selected) (refreshTraits selected)
    let completed = following <> [CheckWindows dealt | notNull dealt]
    push $ TakeEnemyDamageGroup messages (cursor + 1) (deferred <> map (inSimultaneousAttackContext original) completed)
    pure after
  runMessage (ResolveWhenDefeated original) g = dispatchGameMessage original g
  runMessage (ResolveMinionLeavingPlay mid original) g
    | not (HashMap.member mid $ gameMinions g) = pure g
    | otherwise = runQueueT do
        after <- dispatchGameMessage original g
        when (not $ HashMap.member mid $ gameMinions after) $ for_ (lookup mid $ gameMinions g) $ \minion -> case toCard minion of
          EncounterCard card -> checkWindows [W.Window After $ W.EncounterMinionLeftPlay card]
          _ -> pure ()
        pure after
  runMessage (AllyMessage aid AllyLeavePlayByDefeat) g = runQueueT do
    for_ (lookup aid $ gameAllies g) $ \ally -> do
      let def = getCardDef ally
          victory = any (\case Victory _ -> True; _ -> False) (cdKeywords def)
      allyLeavesByDefeat aid victory
    pure g
  runMessage (PrepareAllyDepartureByDefeat aid original) g = runQueueT do
    for_ (lookup aid $ gameAllies g) $ \ally -> do
      let def = getCardDef ally
      checkWindows [W.Window W.When $ W.AllyLeavingPlay (getAllyController ally) aid def]
      for_ (allyUpgrades $ toAttrs ally) $ \uid -> do
        upgrade <- getsGame (lookup uid . gameUpgrades)
        when (maybe False (any (\case Victory _ -> True; _ -> False) . cdKeywords . getCardDef) upgrade) $ addToVictoryDisplay uid
      push $ ResolveAllyLeavingPlay (getAllyController ally) aid def original
      cardSpecific GameRef "endFrameworkDiscard" ()
    pure $ if HashMap.member aid (gameAllies g)
      then g {gameResolvingAbilitySubTypes = (GameRef, [], False) : gameResolvingAbilitySubTypes g}
      else g
  runMessage (Simultaneously msgs) g
    | any (isJust . enemyDamageMessage) flattened || any (\case WithSimultaneousAttackContext {} -> True; _ -> False) flattened = runQueueT do
        push $ PrepareEnemyDamageGroup flattened 0
        pure g
    | otherwise = foldlM (flip runMessage) g (reverse msgs)
   where
    flattened = concatMap flatten msgs
    flatten (Simultaneously messages) = concatMap flatten messages
    flatten message = [message]
  runMessage (ResolveAllyLeavingPlay ident aid def original) g
    | not (HashMap.member aid $ gameAllies g) = pure g
    | otherwise = runQueueT do
        after <- dispatchGameMessage original g
        unless (HashMap.member aid $ gameAllies after) do
          checkWindows [W.Window W.After $ W.AllyLeavingPlay ident aid def]
          when (toCardCode def `elem` ["50105b","50106b","50107b","50108b"]) $
            for_ (lookup aid $ gameAllies g) $ \ally -> case toCard ally of
              PlayerCard card -> do
                cancelMatchingMessage (\case DiscardedCard (PlayerCard other) -> pcCardId card == pcCardId other; _ -> False)
                cardSpecific GameRef "aosReturnRescuedInhuman" card
              _ -> pure ()
        pure after
  runMessage (MinionMessage mid MinionDefeated) g
    | Just m <- lookup mid (gameMinions g), isJust (Minion.minionAttachedTo $ toAttrs m) = pure g
  -- Player-backed minions keep their original owner while serving as boost cards.
  runMessage (DiscardedCard (EncounterCard card)) g
    | Just original <- Data.List.lookup (ecCardId card) (gamePlayerBoostCards g) =
        runMessage (DiscardedCard (PlayerCard original))
          (g {gamePlayerBoostCards = filter ((/= ecCardId card) . fst) (gamePlayerBoostCards g)})
  -- Labor cards have their own deck and never enter the encounter discard.
  runMessage (DiscardedCard (EncounterCard card)) g
    | toCardCode card `elem` ["59002", "59003", "59004"] = runQueueT do
        players <- selectList (IdentityIs HCDeck.hc59001a)
        for_ players $ \you -> cardSpecific you "hcReturnLabor" card
        pure g
  runMessage msg g
    | Just (enemy, source, damage) <- enemyDamageMessage msg = runQueueT do
        barred <- damageDealingForbidden msg
        unless barred $ do
          checkWindows [W.Window W.When $ W.EnemyDamageWouldBeDealt enemy source damage]
          push $ PrepareEnemyDamage msg
        pure g
    | Just mid <- departingMinion msg, HashMap.member mid (gameMinions g) = runQueueT do
        checkWindows [W.Window W.Would $ W.MinionLeavingPlay mid]
        push $ ResolveMinionLeavingPlay mid msg
        pure g
    | Just target <- whenDefeatedTarget msg g = runQueueT do
        you <- getYou
        chooseOnePromptedAbout you "$label.resolveWhenDefeated" target
          [targeting target (push $ ResolveWhenDefeated msg)]
        pure g
    | Just aid <- departingAlly msg, Just ally <- lookup aid (gameAllies g) = runQueueT do
        modifiers <- getModifiers (AllyRef aid)
        unless (CannotLeavePlayByCardAbilities `elem` modifiers) $ do
          let original = if RemovedFromGameInsteadOfLeavingPlay `elem` modifiers then RemoveFromGame (AllyRef aid) else msg
          checkWindows [W.Window W.When $ W.AllyLeavingPlay (getAllyController ally) aid (getCardDef ally)]
          push $ ResolveAllyLeavingPlay (getAllyController ally) aid (getCardDef ally) original
        pure g
    | otherwise = dispatchGameMessage msg g

-- Piercing has an explicit tough-discard step before its incoming damage
-- windows. Expand that step while collecting the simultaneous damage messages.
expandSimultaneousAttackDamage :: (HasQueue m, MonadThrow m, MonadRandom m) => Game -> [Message] -> m (Game, [Message])
expandSimultaneousAttackDamage state = go state []
 where
  go current collected [] = pure (current, collected)
  go current collected (message : rest) = case message of
    CardSpecific (IdentityRef _) "discardToughForPiercing" _ -> do
      (after, following) <- runReaderT (captureQueueT $ dispatchGameMessage message current) (refreshTraits current)
      go after collected (following <> rest)
    _ -> go current (collected <> [message]) rest

runSimultaneousMinionStep :: (HasGame m, HasQueue m, MonadThrow m, MonadRandom m) => Text -> Int -> Game -> m Game
runSimultaneousMinionStep stage cursor original = runQueueT $ case gameSimultaneousMinionAttack saved of
  Nothing -> pure saved
  Just group | not (HashMap.member (simultaneousMinion group) $ gameMinions saved) -> do
    pure $ restoreSimultaneousMinionAttack (simultaneousPrevious group) saved
  Just group -> do
    let mid = simultaneousMinion group
        enemy = EnemyMinionId mid
        entries = simultaneousAttacks group
        next nextStage = cardSpecific GameRef "simultaneousMinionStep" (nextStage :: Text, 0 :: Int)
    if cursor >= length entries then case stage of
      "announce" -> next "defend" >> pure saved
      "defend" -> next "boost" >> pure saved
      "boost" -> next "calculate" >> pure saved
      "calculate" -> next "damage" >> pure saved
      _ -> pure saved
    else case headMay $ drop cursor entries of
      Nothing -> pure saved
      Just (you, _, _, _, _) -> do
        let selected = selectSimultaneousMinionAttack cursor saved
            current = fromMaybe (error "simultaneous minion disappeared") $ lookup mid (gameMinions selected)
            attrs = toAttrs current
            captureStep = cardSpecific GameRef "captureSimultaneousMinionStep" (stage, cursor)
        case stage of
          "announce" -> do
            checkWindows [W.Window W.When $ W.EnemyAttack enemy you]
            villainous <- elem Villainous <$> runReaderT (Minion.getModifiedKeywords attrs) selected
            when villainous $ dealBoost mid
            captureStep
            pure selected
          "defend" -> do
            declareDefense you enemy (Minion.minionDefensePriority attrs)
            captureStep
            pure selected
          "boost" -> do
            minionStep mid MinionFlipBoostCards
            captureStep
            pure selected
          "calculate" -> do
            base <- runReaderT (Minion.getModifiedAttackSigned attrs) selected
            attack' <- traverse (\attack0 -> runReaderT (undefendedIfDefenderGone attack0) selected) (Minion.minionAttacking attrs)
            let refreshed = fmap (withAttackBase $ base + toInteger (Minion.minionBoost attrs)) attack'
                calculated = selected & entitiesL . minionsL . ix mid %~ overMinion (\a -> a {Minion.minionAttacking = refreshed})
            captureStep
            pure calculated
          "damage" -> do
            (after, outputs) <- foldlM (captureIncoming mid) (selected, []) [0 .. length entries - 1]
            let grouped = [let (before, now, afterMessages) = splitSimultaneousAttack messages
                               wrap message = WithSimultaneousAttackContext index message
                            in (map wrap before, map wrap now, map wrap afterMessages)
                          | (index, messages) <- zip [0 :: Int ..] outputs]
                (befores, damage, afters) = unzip3 grouped
            pushAll $ concat befores <> [Simultaneously $ concat damage] <> concat afters
            cardSpecific GameRef "finishSimultaneousMinionAttacks" ()
            pure after
          _ -> pure selected
 where
  saved = saveSimultaneousMinionAttack original
  captureIncoming mid (state, outputs) index = do
    let rotated = selectSimultaneousMinionAttack index state
        selected = case gameSimultaneousMinionAttack rotated >>= headMay . drop index . simultaneousAttacks of
          Just (_, attack', _, _, (reduction, basic)) -> case attackCharacter attack' of
            IdentityCharacter who -> rotated & entitiesL . playersL . ix who %~ \(PlayerIdentity attrs) ->
              PlayerIdentity attrs {playerIdentityDamageReduction = reduction, playerIdentityDefended = basic}
            _ -> rotated
          _ -> rotated
    case lookup mid (gameMinions selected) >>= Minion.minionAttacking . toAttrs of
      Nothing -> pure (selected, outputs)
      Just attack' -> do
        incoming <- capture $ wasAttacked attack'
        (after, following) <- runReaderT (captureQueueT $ foldlM (flip dispatchGameMessage) selected incoming) (refreshTraits selected)
        (expanded, damage) <- expandSimultaneousAttackDamage after following
        pure (expanded, outputs <> [damage])

dispatchGameMessage :: (HasGame m, HasQueue m, MonadThrow m, MonadRandom m) => Message -> Game -> m Game
dispatchGameMessage (WithSimultaneousAttackContext cursor message) g =
  let selected = selectSimultaneousMinionAttack cursor g
   in runReaderT (dispatchGameMessage message selected) (refreshTraits selected)
dispatchGameMessage original g
  | isJust (gameSimultaneousMinionAttack g) =
      let selected = saveSimultaneousMinionAttack g
       in runReaderT (dispatchGameMessageCore original selected) (refreshTraits selected)
  | otherwise = dispatchGameMessageCore original g

dispatchGameMessageCore :: (HasGame m, HasQueue m, MonadThrow m, MonadRandom m) => Message -> Game -> m Game
dispatchGameMessageCore msg g = do
  prevented <- case characterDamageMessage msg of
    Just (_,source,damage) -> attackDamagePreventedFrom source damage
    Nothing -> pure False
  when prevented $ runQueueT $ for_ (characterDamageMessage msg) $ \(target,_,damage) ->
    when (damageAmount damage > 0 && piercesTough damage) $ removeAllTough target
  changed <- if prevented then pure g else dispatchGameMessageCoreBody msg g
  let after = rememberFearNoEvilRemoval msg g $ rememberCompetitiveEffects g changed
  case msg of
    AddToVictoryDisplay _ -> do
      let beforeIds = map ecCardId (gameVictoryDisplay g) <> map pcCardId (gamePlayerVictoryDisplay g)
          added = [getCardDef card | card <- map EncounterCard (gameVictoryDisplay after) <> map PlayerCard (gamePlayerVictoryDisplay after), toCardId card `notElem` beforeIds]
      runQueueT $ for_ added $ \def -> checkWindows [W.Window W.After $ W.VictoryCardAdded def]
    _ -> pure ()
  when (any ((== "60151b") . toCardCode . getCardDef) (HashMap.elems $ gameMainSchemes after)) $
    runQueueT $ when (any (\(sid,support) -> DailyBugle `HashSet.member` cdTraits (Support.supportCardDef $ toAttrs support) && not (HashMap.member sid $ gameSupports after)) (HashMap.toList $ gameSupports g)) $ gameOver Lost
  when (any (isJust . mainSchemeOwner . toAttrs) (HashMap.elems $ gameMainSchemes after) && any (\ident -> not $ HashMap.member ident $ gamePlayers after) (HashMap.keys $ gamePlayers g)) $
    runQueueT $ gameOver Lost
  blackCats <- foldM (\state aid -> runReaderT (runGameMessage (RestoreControlledMinion aid) state) (refreshTraits state)) after
    [aid | (aid,ally) <- HashMap.toList (gameAllies after),toCardCode (getCardDef ally) == "60169",isJust $ Ally.allyOriginalMinion $ toAttrs ally,fromMaybe 0 (Data.List.lookup (AllyCharacter aid) $ gameCharacterThreat after) /= 4]
  synchronizeDeceivedAllies blackCats

dispatchGameMessageCoreBody :: (HasGame m, HasQueue m, MonadThrow m, MonadRandom m) => Message -> Game -> m Game
dispatchGameMessageCoreBody message g = case message of
  RemoveFromPlay target | canonicalAttachmentTarget g target /= target ->
    dispatchGameMessageCoreBodyUnchecked (RemoveFromPlay $ canonicalAttachmentTarget g target) g
  RemoveFromGame target | canonicalAttachmentTarget g target /= target ->
    dispatchGameMessageCoreBodyUnchecked (RemoveFromGame $ canonicalAttachmentTarget g target) g
  ReturnToHand target | canonicalAttachmentTarget g target /= target ->
    dispatchGameMessageCoreBodyUnchecked (ReturnToHand $ canonicalAttachmentTarget g target) g
  PutCardIntoPlay you card _ _ _ -> do
    allowed <- playerCardEntryAllowed (card {pcController = Just you})
    if allowed then dispatchGameMessageCoreBodyUnchecked message g else pure g
  _ -> dispatchGameMessageCoreBodyUnchecked message g

dispatchGameMessageCoreBodyUnchecked :: (HasGame m, HasQueue m, MonadThrow m, MonadRandom m) => Message -> Game -> m Game
dispatchGameMessageCoreBodyUnchecked (ShuffleIntoEncounterDeck cards) g
  | let live = [(mid,card) | card <- cards, let mid = MinionId $ unCardId $ ecCardId card
                 , Just minion <- [lookup mid (gameMinions g)], toCard minion == EncounterCard card]
  , not (null live) = runQueueT do
      traverse_ (shuffleMinionIntoEncounterDeck . fst) live
      let remaining = filter (`notElem` map snd live) cards
      unless (null remaining) $ shuffleIntoEncounterDeck remaining
      pure g
-- With no activating enemy, effects on that enemy have no target.
dispatchGameMessageCoreBodyUnchecked (DealBoost (AttachmentRef aid)) g
  | any (\(mid,_,_) -> unMinionId mid == unAttachmentId aid) (gameInPlayBoosts g)
  , not (HashMap.member aid $ gameAttachments g) = pure g
dispatchGameMessageCoreBodyUnchecked original g =
  let normalized = resolveMessageCardCosts (startingPlayerCountIn g) original
      msg = case normalized of
        CheckWindows windows ->
          let completions =
                [ W.Window W.After $ W.EnemyAttackDefeatedCharacter enemy (AllyCharacter aid)
                | W.Window W.After (W.AllyCardDefeated _ card) <- windows
                , let aid = AllyId $ unCardId $ pcCardId card
                , Just enemy <- [Data.List.lookup aid $ gamePendingAttackDefeats g]
                ]
           in CheckWindows (windows <> completions)
        _ -> normalized
   in runPreGameMessage msg g
    >>= traverseOf scenarioL (runMessage msg)
    >>= traverseOf entitiesL (dispatchEntitiesMessage msg)
    >>= dispatchInPlayMinionBoosts msg
    >>= traverseOf removedEntitiesL (\removed -> do
      -- Eliminated identities remain available as snapshots, but no longer act.
      scopes <- getsGame gameInPlayBoosts
      let source = inPlayBoostCallbackTarget msg
          held = HashMap.filterWithKey (\mid _ -> source == Just (MinionRef mid) && any (\(other,_,_) -> other == mid) scopes) (entitiesMinions removed)
      updated <- runMessage msg removed {entitiesPlayers = mempty,entitiesMinions = entitiesMinions removed `HashMap.difference` held}
      pure updated {entitiesPlayers = entitiesPlayers removed,entitiesMinions = entitiesMinions updated <> held})
    >>= traverseOf boostEntitiesL (runMessage (Boost msg))
    >>= traverseOf (activeCostL . ix 0) (\c -> asYou (activeCostIdentityId c) $ runMessage msg c)
    >>= runHandCardMessage msg
    >>= runGameMessage msg
    >>= stalwartSweep
    >>= cleanUpAttachedMinions g
    >>= cleanUpGenericUpgrades
    >>= pure . pruneMissionEntities

-- | An ability a card has in its owner's hand ('handCardAbilities') is resolved by the
-- card, built as it would be in play; it stays in hand until what it does moves it.
runHandCardMessage :: (HasGame m, HasQueue m, MonadThrow m, MonadRandom m) => Message -> Game -> m Game
runHandCardMessage msg g = runQueueT $ case msg of
  RanAbility you (CardIdRef cid) _ windows _ -> do
    for_ [card | W.DiscardedFromPlayerDeck owner cards <- windows, owner == you, card <- cards, pcCardId card == cid] $ \card -> do
      Domino.runDiscardResponse you card
    for_ (handCard g cid) \(owner, c) -> case cdCardType (getCardDef c) of
      AllyType -> void $ asYou owner $ runMessage msg (createAlly owner c)
      EventType -> void $ asYou owner $ runMessage msg (createEvent owner c)
      ObligationType -> void $ asYou owner $ runMessage msg (handObligation c)
      _ -> pure ()
    pure g
  _ -> pure g

addPlayer :: (MonadGame env m) => PlayerIdentity -> m ()
addPlayer player =
  withGame_ $
    (entitiesL . playersL %~ insertMap (toId player) player)
      . (playerOrderL <>~ [toId player])
      . joinFirstArea (toId player)

createPlayer :: (MonadGame env m) => CardCode -> Deck -> m ()
createPlayer cardCode deck = do
  playerIdentity <- initPlayer cardCode deck
  addPlayer playerIdentity

-- | Run messages until the queue empties or someone has to answer a question, saying
-- nothing: for games played out of sight, such as a look ahead.
runGameMessages :: (MonadGame env m) => m ()
runGameMessages = runGameMessagesWith (const $ pure ())

-- | Run messages until the queue empties or someone has to answer a question, telling
-- the function given about each message before it runs: that is where the log is told.
-- Preview queries instantiate this loop at GameAppT. Specializing that call clones
-- the dispatcher: GHC 9.14 allocated 53 GB in specialization alone. Keep the body
-- optimized, but preserve the call boundary instead of specializing the engine.
{-# NOINLINE runGameMessagesWith #-}
runGameMessagesWith :: (MonadGame env m) => (Message -> m ()) -> m ()
runGameMessagesWith before = go (0 :: Int)
 where
  go n = do
    g <- getGame
    if
      | notNull (gameQuestion g) || isFinished g -> pure ()
      | n >= messageLimit -> error "message limit reached: the game is probably looping"
      | otherwise ->
          pop >>= \case
            Nothing -> pure ()
            Just (Ask ident asked) | competitiveFinishedPhaseQuestion g ident asked -> go (n + 1)
            Just (Ask ident asked) | not (HashMap.member ident $ gamePlayers g), not (competitiveReplyChoice g ident asked), not (campaignDefeatChoice g ident asked) -> go (n + 1)
            Just (IdentityMessage ident _) | not (HashMap.member ident $ gamePlayers g) -> go (n + 1)
            Just (Ask ident asked) -> do
              patrolled <- isPatrolled ident
              cleaned <- withoutPatrolledThwarts g patrolled (withoutPermanentTargets g $ withoutDepartedActivationTargets g asked)
              case cleaned of
                -- nothing is asked when there is nothing to choose
                question | not (hasChoices question) -> go (n + 1)
                question -> do
                  withGame_ $ (questionL .~ fromList [(ident, question)]) . (questionVersionL +~ 1)
                  -- a player chosen when there is only the one is chosen for them
                  when (onlyPlayer question) $ answerQuestionWith before ident 0
            Just other -> do
              debug other
              before other
              withGameM $ runMessage other
              go (n + 1)
  -- The seat still handles the campaign outcome after its last hero is defeated.
  -- Ordinary gameplay questions for eliminated identities continue to be skipped.
  campaignDefeatChoice g ident question =
    ident `elem` gameSeats g && HashMap.null (gamePlayers g) && case gameCampaign g of
      Just c | c.kind == Campaign.FearNoEvilCampaign, isJust c.campaignLog.fearNoEvil.selectedScenario -> case question of
        Read _ _ -> True
        QuestionLabel "$label.fneFailCompletedScenario" _ _ -> True
        _ -> False
      _ -> False
  competitiveReplyChoice g ident question = case gameCompetition g of
    Nothing -> False
    Just c -> ident `elem` [CW.teamFirstPlayer c CW.Registration,CW.teamFirstPlayer c CW.Resistance] && enemyChoice question
   where
    enemyChoice = \case
      QuestionLabel "$label.cwEnemyTeamChooses" _ _ -> True
      QuestionLabel _ _ q -> enemyChoice q
      Searched _ q -> enemyChoice q
      Read _ q -> enemyChoice q
      _ -> False
  isFinished g = case gameState g of
    Finished _ -> True
    _ -> False

-- | Answer the question a player was asked with the choice at the index given, and play
-- on to the next question.
answerQuestionWith :: (MonadGame env m) => (Message -> m ()) -> IdentityId -> Int -> m ()
answerQuestionWith before ident n = do
  g <- getGame
  question <- maybe (throwM NoQuestion) pure $ lookup ident (gameQuestion g)
  withGame_ $ questionL .~ mempty
  (msgs, remaining) <- answer question
  pushAll $ msgs <> [Ask ident q | Just q <- [remaining]]
  runGameMessagesWith before
 where
  -- what the choice does, and what is left to ask
  answer = \case
    QuestionLabel l t q -> second (fmap (QuestionLabel l t)) <$> answer q
    Read r q -> second (fmap (Read r)) <$> answer q
    Searched cs q -> second (fmap (Searched cs)) <$> answer q
    ChooseOne qs -> (,Nothing) <$> pick qs
    ChooseN k qs -> do
      msgs <- pick qs
      let rest = without qs
      pure (msgs, guard (k > 1 && notNull rest) $> ChooseN (k - 1) rest)
    ChooseOneAtATime qs -> do
      msgs <- pick qs
      let rest = without qs
      pure (msgs, guard (notNull rest) $> ChooseOneAtATime rest)
    ChoosePlayerOrder (Unsorted unsorted) (Sorted sorted) -> case atMay unsorted n of
      Nothing -> throwM InvalidChoice
      Just chosen -> do
        let sorted' = sorted <> [chosen]
        pure case filter (/= chosen) unsorted of
          [] -> ([SetPlayerOrder sorted'], Nothing)
          [lastOne] -> ([SetPlayerOrder (sorted' <> [lastOne])], Nothing)
          rest -> ([], Just $ ChoosePlayerOrder (Unsorted rest) (Sorted sorted'))
  pick qs = maybe (throwM InvalidChoice) (choiceMessages ident) (atMay qs n)
  without qs = take n qs <> drop (n + 1) qs

-- | Run a debug message now, even while a question waits: it and whatever it leads to go
-- first, then the waiting question is asked again and play carries on from there.
runDebugWith :: (Message -> GameAppT ()) -> Message -> GameAppT ()
runDebugWith before msg = do
  asked <- getsGame gameQuestion
  withGame_ $ questionL .~ mempty
  pushAll $ msg : [Ask ident q | (ident, q) <- HashMap.toList asked]
  runGameMessagesWith before

gameSelectExtendedCard ::
  (MonadThrow m, HasGame m, HasQueue m) =>
  ExtendedCardMatcher ->
  m (HashSet PlayerCard)
gameSelectExtendedCard m = do
  let
    excludedCards = getExcludedCards m
    msgs = map (RemoveFromGame . toRef) excludedCards
  players <- HashMap.elems <$> getsGame gamePlayers
  g <- getGame
  tempApp <- newApp g msgs
  let
    allCards =
      concatMap (unHand . playerIdentityHand . toAttrs) players
        <> concatMap (unDiscard . playerIdentityDiscard . toAttrs) players
        <> concatMap (unDeck . playerIdentityDeck . toAttrs) players
        <> [c | PlayerCard c <- scenarioSetAsideCards (toAttrs $ g.gameScenario)]
  result <- runGameApp tempApp $ do
    runGameMessages
    go False players allCards m
  pure $ HashSet.fromList result
 where
  getExcludedCards = \case
    NotCard c -> [c]
    ExtendedCardMatches ms -> concatMap getExcludedCards ms
    _ -> []
  go asHand players cards = \case
    NotCard c -> pure $ filter (/= c) cards
    AffordableCardBy identityMatcher extendedCardMatcher -> do
      identities <- selectList identityMatcher
      let players' = filter ((`elem` identities) . toId) players
      -- the cards may be anyone's: only who affords them is narrowed
      inner <- go asHand players cards extendedCardMatcher
      concatMapM (\ident -> filterM ((if asHand then isPlayableFromHand else isPlayable) ident) inner) players'
    PlayableIgnoringCostBy identityMatcher extendedCardMatcher -> do
      identities <- selectList identityMatcher
      let players' = filter ((`elem` identities) . toId) players
      inner <- go asHand players cards extendedCardMatcher
      concatMapM (\ident -> filterM ((if asHand then isPlayableFromHandWith else isPlayableWith) True ident) inner) players'
    BasicCardMatches cardMatcher -> do
      pure $ filter (cardMatch cardMatcher) cards
    InSetAsideOf identityMatcher extendedCardMatcher -> do
      owners <- selectList identityMatcher
      aside <- getSetAsidePlayerCards
      go asHand players [c | c <- aside, c `elem` cards, maybe False (`elem` owners) (pcOwner c <|> pcController c)] extendedCardMatcher
    CardWithId cid -> pure $ filter ((== cid) . pcCardId) cards
    InDeckOf identityMatcher extendedCardMatcher -> do
      identities <- selectList identityMatcher
      let cards' = filter (`elem` cards) $ concatMap (unDeck . playerIdentityDeck . toAttrs) $ filter ((`elem` identities) . toId) players
      go asHand players cards' extendedCardMatcher
    InDiscardOf identityMatcher extendedCardMatcher -> do
      identities <- selectList identityMatcher
      let
        players' = filter ((`elem` identities) . toId) players
        cards' =
          filter (`elem` cards) $
            concatMap (unDiscard . playerIdentityDiscard . toAttrs) players'
      go asHand players cards' extendedCardMatcher
    PlayableFromHandOf identityMatcher extendedCardMatcher -> do
      identities <- selectList identityMatcher
      candidates <- concatMapM getPlayableHandCards identities
      go True players (filter (`elem` cards) candidates) extendedCardMatcher
    InHandOf identityMatcher extendedCardMatcher -> do
      identities <- selectList identityMatcher
      let
        cards' =
          filter (`elem` cards) $
            concatMap (unHand . playerIdentityHand . toAttrs) $
              filter ((`elem` identities) . toId) players
      go asHand players cards' extendedCardMatcher
    TopmostCardInDiscardOf identityMatcher cardMatcher -> do
      identities <- selectList identityMatcher
      let players' = filter ((`elem` identities) . toId) players
      pure $
        mapMaybe
          ( find (and . sequence [(`elem` cards), cardMatch cardMatcher])
              . unDiscard
              . playerIdentityDiscard
              . toAttrs
          )
          players'
    ExtendedCardMatches matchers -> foldlM (go asHand players) cards matchers
    CanEnterPlay -> do
      inPlay <- getUniqueTitlesInPlay
      let named c = (title $ cdName $ getCardDef c, maybe [] pure $ subtitle $ cdName $ getCardDef c)
      allowed <- filterM playerCardEntryAllowed cards
      pure $ filter (\c -> not (cdUnique $ getCardDef c) || not (any (uniqueMatch (named c)) inPlay)) allowed

-- | Cheap hand/trait/payment filters precede isolated play checks in smoke games.
playableClawsEvents :: (HasGame m, HasQueue m) => IdentityId -> Source -> m [PlayerCard]
playableClawsEvents ident source = do
  g <- getGame
  app <- newApp g []
  runGameApp app do
    runQueueT $ case source of
      UpgradeRef uid -> exhaust uid
      _ -> pure ()
    runGameMessages
    cards <- filter (cardMatch (CardWithType EventType <> CardWithTrait Marvel.Trait.Attack)) <$> getPlayableHandCards ident
    payable <- filterM (\card -> canAffordCost ident source (DamageCost (fromIntegral $ max 0 $ fromMaybe 0 $ cdCost $ getCardDef card))) cards
    filterM (canPlayCardIgnoringResourceCost ident) payable

playableCardsWithReductionAfterExhausting :: (HasGame m, HasQueue m) => IdentityId -> Target -> CardMatcher -> Natural -> m [PlayerCard]
playableCardsWithReductionAfterExhausting ident source matcher amount = do
  g <- getGame
  app <- newApp g []
  runGameApp app do
    runQueueT $ case source of
      SupportRef sid -> exhaust sid
      UpgradeRef uid -> exhaust uid
      AllyRef aid -> exhaust aid
      IdentityRef pid -> exhaust pid
      _ -> pure ()
    runGameMessages
    cards <- filter (cardMatch matcher) <$> getPlayableHandCards ident
    filterM (canPlayCardWithReduction ident amount) cards

-- Ignoring a printed When Defeated keeps the normal defeat and its windows.
dispatchEntitiesMessage :: (HasGame m, HasQueue m, MonadThrow m, MonadRandom m) => Message -> Entities -> m Entities
dispatchEntitiesMessage msg@(MinionMessage mid MinionDefeated) entities
  | Just minion <- lookup mid (entitiesMinions entities), Minion.minionIgnoreWhenDefeated (toAttrs minion) = do
      rest <- runMessage msg entities {entitiesMinions = deleteMap mid $ entitiesMinions entities}
      updated <- runQueueT $ MinionRunner.runMinionMessage MinionDefeated (toAttrs minion)
      pure rest {entitiesMinions = insertMap mid (Minion.overMinion (const updated) minion) (entitiesMinions rest)}
dispatchEntitiesMessage msg entities = do
  scopes <- getsGame gameInPlayBoosts
  let source = inPlayBoostCallbackTarget msg
      withheld = [mid | (mid,_,_) <- scopes, source == Just (MinionRef mid)]
      held = HashMap.filterWithKey (\mid _ -> mid `elem` withheld) (entitiesMinions entities)
  updated <- runMessage msg entities {entitiesMinions = foldr deleteMap (entitiesMinions entities) withheld}
  pure updated {entitiesMinions = entitiesMinions updated <> held}

-- | A scoped boost view keeps its callbacks and delayed boost text independent
-- from the real minion's live state. It never enters the actual boost pile.
dispatchInPlayMinionBoosts :: (HasGame m, HasQueue m, MonadThrow m, MonadRandom m) => Message -> Game -> m Game
dispatchInPlayMinionBoosts msg g = foldM dispatch g chosen
 where
  scopes = zip [0 :: Int ..] (gameInPlayBoosts g)
  matching target = take 1 [(index,scope) | (index,scope@(mid,_,_)) <- scopes, target == MinionRef mid]
  chosen = case msg of
    RevealedAsBoost target _ -> matching target
    CheckWindows windows | any (\case W.Window _ (W.CountingBoostIcons _) -> True; _ -> False) windows ->
      [(index,scope) | (index,scope@(mid,_,_)) <- scopes, HashMap.member mid (gameMinions g)]
    CheckWindows _ -> scopes
    _ -> maybe [] matching (inPlayBoostCallbackTarget msg)
  dispatch current (index,(mid,you,view)) = do
    before <- withQueue $ \queue -> (queue,queue)
    let scoped = current {gameBoostPlayerOverride = Just you,gameActivePlayer = you,gameYou = Just you}
    updated <- runReaderT (runMessage (Boost msg) view) scoped
    after <- withQueue $ \queue -> (queue,queue)
    let handled = updated /= view || before /= after
        withView = current {gameInPlayBoosts = [if i == index then (mid,you,updated) else scope | (i,scope) <- zip [0 :: Int ..] (gameInPlayBoosts current)]}
    case (inPlayBoostCallbackTarget msg,lookup mid $ gameMinions current) of
      (Just _,Just live) | not handled -> do
        ordinary <- runMessage msg live
        pure $ withView & entitiesL . minionsL %~ insertMap mid ordinary
      (Just _,Nothing) | not handled, Just removed <- lookup mid (entitiesMinions $ gameRemovedEntities current) -> do
        ordinary <- runMessage msg removed
        pure $ withView & removedEntitiesL . minionsL %~ insertMap mid ordinary
      _ -> pure withView

-- Baron Zemo's constant type change applies at message boundaries, including
-- blanking, removal of threat, and Zemo leaving play. The physical card stays put.
synchronizeDeceivedAllies :: (HasGame m,HasQueue m,MonadThrow m,MonadRandom m) => Game -> m Game
synchronizeDeceivedAllies g
  | null zemos && null deceived = pure g
  | otherwise = do
      active <- runReaderT (anyM (fmap not . isBlanked) zemos) (refreshTraits g)
      let threatened character = fromMaybe 0 (Data.List.lookup character $ gameCharacterThreat g) > 0
          revert = [mid | mid <- deceived,not active || not (threatened $ MinionCharacter mid)]
          convert = [aid | (aid,_) <- HashMap.toList (gameAllies g),active,threatened $ AllyCharacter aid]
      reverted <- foldM (\state mid -> runReaderT (runGameMessage (MinionBecomesAlly mid) state) (refreshTraits state)) g revert
      foldM (\state aid -> do
        next <- runReaderT (runGameMessage (AllyBecomesMinion aid) state) (refreshTraits state)
        let mid = MinionId $ unAllyId aid
        pure $ next & entitiesL . minionsL . ix mid %~ overMinion (\a -> a {Minion.minionCardDef=(Minion.minionCardDef a) {cdTraits=HashSet.insert Deceived $ cdTraits $ Minion.minionCardDef a}})) reverted convert
 where
  zemos = [minion | minion <- HashMap.elems (gameMinions g),toCardCode (getCardDef minion) == "60145"]
  deceived = [mid | (mid,minion) <- HashMap.toList (gameMinions g),Deceived `HashSet.member` cdTraits (getCardDef minion),isJust $ Minion.minionFacedownCard $ toAttrs minion]

-- Only existing scalar token fields can be changed; unknown keys, negative or
-- fractional counters, and incompatible values leave the whole edit unchanged.
debugPatch :: (ToJSON a, FromJSON a) => Text -> Object -> a -> a
debugPatch prefix values original = case toJSON original of
  Object fields | all valid (KeyMap.toList values) -> case fromJSON (Object $ KeyMap.union translated fields) of
    Success updated -> updated
    Error _ -> original
   where
    field key = Key.fromText $ prefix <> (if prefix == "" then Key.toText key else upperFirst (Key.toText key))
    upperFirst text = case T.uncons text of
      Nothing -> text
      Just (first, rest) -> T.toUpper (T.singleton first) <> rest
    translated = KeyMap.fromList [(field key, value) | (key, value) <- KeyMap.toList values]
    valid (key, value) = KeyMap.member (field key) fields && case value of
      Bool _ -> key `elem` ["exhausted", "stunned", "confused", "tough", "additionalStun", "additionalConfuse"]
      Number _ -> key `elem` ["damage", "threat", "counters", "uses", "accelerationTokens", "additionalTough"] && case fromJSON @Natural value of
        Success _ -> True
        Error _ -> False
      _ -> False
  _ -> original
