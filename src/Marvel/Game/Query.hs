{-# OPTIONS_GHC -Wno-orphans #-}

module Marvel.Game.Query where

import Control.Monad ( replicateM_ )
import Control.Monad.Reader
import Data.Functor.Identity ( runIdentity )
import Data.IORef
import Data.List ( find, nub, nubBy )
import Data.Maybe ( maybeToList )
import Data.Ord ( Down(..) )
import Marvel.Ability
import Marvel.ActiveCost
import Marvel.AgeOfApocalypse.MissionTypes
import Marvel.Ally
import Marvel.Ally.Types
    ( allyOwner,
      allyUpgrades,
      getAllyController,
      getAllyDamage,
      getAllyUses )
import Marvel.Area
import Marvel.Aspect ( Aspect )
import Marvel.Attachment.Types
    ( Field(AttachmentExhausted, AttachmentCounters) )
import Marvel.Attack
import Marvel.Boost ( boostCount )
import Marvel.Campaign ( Campaign )
import Marvel.CampaignPlayers.Helpers ( echoReplayCards )
import Marvel.Card
import Marvel.Card.WhenDefeated ( hasWhenDefeated )
import Marvel.Card.WhenRevealed ( hasWhenRevealed )
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Deck
import Marvel.Difficulty
import Marvel.Discard
import Marvel.Effect.CardPlayReduction ( CardPlayReduction(..) )
import Marvel.Effect.Types ( Effect )
import Marvel.EncounterSet
import Marvel.Entity
import Marvel.Environment.Types
    ( Field(EnvironmentCounters), Environment )
import Marvel.Event.Types
    ( Attrs(eventCardDef, eventController, eventOwner, eventPaidWith,
            eventAttacked, eventThwarted, eventAttackStarted),
      Event(..),
      eventTargets,
      eventAbilitySubTypes,
      eventExhaustionCostValid )
import Marvel.Exception
import Marvel.Game.App ( newApp, runGameApp )
import Marvel.Game.Base
import Marvel.Game.Class
import Marvel.Game.Setup
import Marvel.Game.Source
    ( gameSelectCountScheme,
      gameSelectScheme,
      gameSelectSideScheme,
      gameSelectSupport )
import Marvel.GameValue ( GameValue(..), fromGameValue )
import Marvel.Hand
import Marvel.Hp
import Marvel.I18n ( ikeyWith, textVar )
import Marvel.Id
import Marvel.Identity hiding (alliesL, minionsL, supportsL, upgradesL)
import Marvel.Keyword
import Marvel.MainScheme
import Marvel.MainScheme.Types
    ( Attrs(mainSchemeOwner, mainSchemeVillain, mainSchemeCounters,
            mainSchemeHasGlider, mainSchemeAccelerationTokens,
            mainSchemeHeldCards),
      Field(MainSchemeCounters),
      getMainSchemeThreat )
import Marvel.Matchers hiding (ExhaustedIdentity, MainScheme)
import Marvel.Message.Lifted hiding (ExhaustedAlly, createEffect, canMoveSchemeThreatFrom)
import Marvel.Minion
import Marvel.Minion.Types
    ( Attrs(minionEngagedIdentity, minionStunned, minionConfused,
            minionTough),
      getMinionDamage,
      getMinionEngagedIdentity,
      getMinionPrintedHitPoints,
      minionAttackDetails,
      overMinion,
      Minion )
import Marvel.Modifier
import Marvel.Name
import Marvel.Obligation
import Marvel.Obligation.Types
    ( Attrs(obligationOwner), Obligation )
import Marvel.Payment
import Marvel.Phase
import Marvel.Prelude
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Resource
import Marvel.Scenario.Types
    ( Attrs(scenarioMission, scenarioCardDecks, scenarioSetAsideCards,
            scenarioEncounterDeck, scenarioDiscard, scenarioDecks),
      deckKeys,
      getCompetitionSetup,
      getScenarioDifficulty,
      IsScenario(overScenarioAttrs),
      Scenario(..) )
import Marvel.SideScheme.Types
    ( Attrs(sideSchemeHeldCards, sideSchemeCounters,
            sideSchemeAccelerationTokens),
      getSideSchemeThreat,
      isCrisis )
import Marvel.Stats ( Sch(..), Thw(..), Atk(..) )
import Marvel.Support.Types
    ( Attrs(supportCardDef, supportHeldCards, supportCanPlayHeldEvents,
            supportOwner),
      Support,
      getSupportController,
      getSupportUses )
import Marvel.Trait hiding (Finished)
import Marvel.Upgrade.Types
    ( Attrs(upgradeOccupiedSlots, upgradeAttachedMainScheme,
            upgradeAttachedSideScheme, upgradeOwner, upgradeFaceDown,
            upgradeHeldCardsFacedown, upgradeAttachedAlly,
            upgradeAttachedEnemy, upgradeHeldCards, upgradeResourceSlots),
      getUpgradeController,
      getUpgradeUses,
      IsUpgrade(upgradeTargets),
      Upgrade(..) )
import Marvel.Villain.Types
    ( villainAttackDetails, villainDamage, villainIsTough )
import Marvel.Window ( Window, WindowTiming(..), windowMatches )
import Marvel.Ability qualified as Ability
import Marvel.Ally.Types qualified as Ally
import Marvel.AgeOfApocalypse.Settings qualified as AoaSettings
import Marvel.Attachment.Types qualified as Attachment
import Marvel.CivilWar.Types qualified as CW
import Marvel.Campaign qualified as Campaign
import Marvel.CampaignPlayers.Helpers qualified as CampaignPlayerHelpers
import Data.List qualified as Data.List
import Data.Aeson.Diff qualified as Diff
import Marvel.Resource.Resources.Domino qualified as Domino
import Marvel.Effect.Types qualified as Effect
import Marvel.Hercules.Helpers qualified as HCHelpers
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Marvel.Hero.Runner qualified as Hero
import Marvel.JessicaJones.Helpers qualified as JLHelpers
import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Environment.Types qualified as MGEnvironment
import Marvel.PlayerCard qualified as MGPlayer
import Marvel.MainScheme.Types qualified as MS
import Marvel.Matchers qualified as Matchers
import Marvel.Minion.Types qualified as Minion
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Types qualified as Obligation
import Marvel.SideScheme.Types qualified as SideScheme
import Marvel.Support.Types qualified as Support
import Marvel.Treachery.Types qualified as Treachery
import Marvel.Upgrade.Types qualified as Upgrade
import Marvel.Villain.Types qualified as Villain
import Marvel.Window qualified as W

getPlayers :: (HasGame m) => m [IdentityId]
getPlayers = do
  you <- getYou
  getsGame \g -> case gameAreas g of
    _ : _ : _ | not (gameAllAreas g) || isJust (gameCompetition g) ->
      let here = areaId <$> areaOfTarget g (IdentityRef you)
       in filter (\p -> (areaId <$> areaOfTarget g (IdentityRef p)) == here) (gamePlayerOrder g)
    _ -> gamePlayerOrder g

-- | Every player, in turn order, whatever area they are in.
getAllPlayers :: (HasGame m) => m [IdentityId]
getAllPlayers = getsGame gamePlayerOrder

getActivePlayerId :: (HasGame m) => m IdentityId
getActivePlayerId = do
  competitive <- getCompetition
  if isJust competitive then getsGame gameActivePlayer else toId <$> getActivePlayer

getPlayerCount :: (HasGame m) => m Int
getPlayerCount = do
  competitive <- getCompetition
  length <$> if isJust competitive then getPlayers else getAllPlayers

-- | The players who started the scenario, the eliminated among them (RR Per Player Icon).
getStartingPlayerCount :: (HasGame m) => m Int
getStartingPlayerCount = getsGame startingPlayerCountIn

startingPlayerCountIn :: Game -> Int
startingPlayerCountIn g = fromMaybe
  (maybe (max (length $ gameSeats g) (length $ gamePlayerOrder g)) (length . CW.competitiveRegistrationPlayers) (gameCompetition g))
  (gamePlayerCountContext g)

getCompetition :: HasGame m => m (Maybe CW.Competition)
getCompetition = getsGame gameCompetition

-- | The players acting in this phase; eliminated players and a finished team's
-- board do not take more turns while the other side has its reply.
getPhasePlayers :: HasGame m => m [IdentityId]
getPhasePlayers = do
  competitive <- getCompetition
  case competitive of
    Nothing -> getAllPlayers
    Just c | isJust (CW.teamOutcome c $ CW.phaseTeam $ CW.competitivePhase c) -> pure []
    Just c -> getsGame $ filter (`elem` CW.teamPlayers c (CW.phaseTeam $ CW.competitivePhase c)) . gamePlayerOrder

competitiveBoardFinishedFor :: Game -> IdentityId -> Bool
competitiveBoardFinishedFor g you = isJust $ do
  c <- gameCompetition g
  team <- CW.playerTeam c you
  CW.teamOutcome c team

competitivePhaseStoppedFor :: Game -> IdentityId -> Bool
competitivePhaseStoppedFor g you = competitiveBoardFinishedFor g you
  && maybe False (\c -> CW.playerTeam c you == Just (CW.phaseTeam $ CW.competitivePhase c)) (gameCompetition g)

competitiveFinishedPhaseQuestion :: Game -> IdentityId -> Question -> Bool
competitiveFinishedPhaseQuestion g you question
  | not (competitivePhaseStoppedFor g you) = False
  | otherwise = case question of
      QuestionLabel label _ inner ->
        (label == "$label.yourTurn" && gamePhase g == PlayerPhase)
          || (label == "$label.chooseMinionToActivate" && gamePhase g == VillainPhase)
          || competitiveFinishedPhaseQuestion g you inner
      Read _ inner -> competitiveFinishedPhaseQuestion g you inner
      Searched _ inner -> competitiveFinishedPhaseQuestion g you inner
      _ -> False

getPhaseAreas :: HasGame m => m [Area]
getPhaseAreas = do
  competitive <- getCompetition
  areas <- getAreas
  pure $ case competitive of
    Nothing -> areas
    Just c -> filter ((== CW.teamArea (CW.phaseTeam $ CW.competitivePhase c)) . areaId) areas

-- | Resolve printed values for physical cards using the original table size.
-- Registry definitions remain reusable across games with different player counts.
resolvePlayerCardDef :: HasGame m => CardDef -> m CardDef
resolvePlayerCardDef def = flip resolvePlayerCountCost def <$> getStartingPlayerCount

resolvePlayerCardCosts :: HasGame m => PlayerCard -> m PlayerCard
resolvePlayerCardCosts card = do
  def <- resolvePlayerCardDef (pcCardDef card)
  pure card {pcCardDef = def}

lookupPlayerCardInGame :: HasGame m => CardCode -> m CardDef
lookupPlayerCardInGame = resolvePlayerCardDef . MGPlayer.lookupPlayerCard

getPlayerCardCatalogue :: HasGame m => m (HashMap CardCode CardDef)
getPlayerCardCatalogue = do
  players <- getStartingPlayerCount
  pure $ resolvePlayerCountCost players <$> MGPlayer.allPlayerCards

-- | Each unique card in play, and each identity, as a title and the subtitles and
-- alter-ego titles that go with it: a unique card matching one cannot enter play.
-- | In "your" game area: a unique card in one area places no limitation on the others
-- (TOAFK insert p.7).
-- | The unique cards a unique player card would match (RR Unique Icon): the players'
-- cards in your area, and the unique minions and villains in play (the Nebula ally and the
-- Nebula minion match).
getUniqueTitlesInPlay :: (HasGame m) => m [(Text, [Text])]
getUniqueTitlesInPlay = do
  players <- map snd <$> (getUniqueCardsIn . areaId =<< getYourArea)
  competitive <- getCompetition
  encounter <- if isJust competitive then pure [] else getsGame (map snd . uniqueEncounterCardsInPlay)
  exempt <- getUniqueExemptTitles
  pure $ filter ((`notElem` exempt) . fst) (players <> encounter)

-- | The titles the unique rule does not apply to while a card says so (Avengers Tower's
-- Stronghold side, Tower Defense).
getUniqueExemptTitles :: (HasGame m) => m [Text]
getUniqueExemptTitles = mapMaybe (\case UniqueRuleDoesNotApply t -> Just t; _ -> Nothing) <$> getModifiers GameRef

-- | The unique encounter cards in play: minions, villains, attachments and
-- environments.
uniqueEncounterCardsInPlay :: Game -> [(Target, (Text, [Text]))]
uniqueEncounterCardsInPlay g =
  [ (t, (title n, maybe [] pure (subtitle n)))
  | (t, def) <-
      [(MinionRef k, getCardDef m) | (k, m) <- HashMap.toList (gameMinions g)]
        <> [(VillainRef k, getCardDef v) | (k, v) <- HashMap.toList (gameVillains g)]
        <> [(AttachmentRef k, getCardDef a) | (k, a) <- HashMap.toList (gameAttachments g)]
        <> [(EnvironmentRef k, getCardDef e) | (k, e) <- HashMap.toList (gameEnvironments g)]
        <> [(SideSchemeRef k, getCardDef e) | (k, e) <- HashMap.toList (gameSideSchemes g)]
  , cdUnique def
  , let n = cdName def
  ]

-- | Whether the unique card entering play (already made, as the target) matches another
-- card in play: the player's cards in their area, or a unique encounter card.
matchesUniqueInPlay :: (HasGame m) => IdentityId -> Target -> CardDef -> m Bool
matchesUniqueInPlay ident self def
  | not (cdUnique def) = pure False
  | otherwise = do
      players <- getUniqueCardsIn . areaId =<< getAreaOf ident
      competitive <- getCompetition
      aid <- areaId <$> getAreaOf ident
      encounter <- getsGame $ \g -> filter (\(target,_) -> isNothing competitive || (areaId <$> areaOfTarget g target) == Just aid) $ uniqueEncounterCardsInPlay g
      exempt <- getUniqueExemptTitles
      let named = (title (cdName def), maybe [] pure (subtitle (cdName def)))
          playerCard = cdCardType def `elem` [HeroType,AlterEgoType,AllyType,SupportType,UpgradeType,EventType,ResourceType,PlayerSideSchemeType]
          candidates = if isNothing competitive then players <> encounter else if playerCard then players else encounter
      pure $ fst named `notElem` exempt && any (uniqueMatch named . snd) [x | x@(t, _) <- candidates, t /= self]

-- | The unique cards in play in the area, by title and its other names: the players'
-- allies, supports and upgrades, and their identities.
getUniqueCardsIn :: (HasGame m) => AreaId -> m [(Target, (Text, [Text]))]
getUniqueCardsIn aid = getsGame \g ->
  let
    split = case gameAreas g of
      _ : _ : _ -> True
      _ -> False
    here t = not split || (areaId <$> areaOfTarget g t) == Just aid
    named n = (title n, maybe [] pure (subtitle n))
    unique xs = [(t, named $ cdName def) | (t, def) <- xs, cdUnique def, here t]
    -- its hero title, and its alter-ego title as the other name: Ant-Man's two hero
    -- forms share the one title
    identity (k, p) =
      let sides = HashMap.elems $ playerIdentitySides $ toAttrs p
          heroes = nub [title (cdName $ getCardDef h) | HeroSide h <- sides]
          alterEgos = [title (cdName $ getCardDef a) | AlterEgoSide a <- sides]
       in case heroes of
            t : ts | here (IdentityRef k) -> [(IdentityRef k, (t, ts <> alterEgos))]
            _ -> []
   in
    unique [(AllyRef k, getCardDef a) | (k, a) <- HashMap.toList (gameAllies g)]
      <> unique [(SupportRef k, supportCardDef (toAttrs s)) | (k, s) <- HashMap.toList (gameSupports g)]
      <> unique [(UpgradeRef k, getCardDef u) | (k, u) <- HashMap.toList (gameUpgrades g)]
      <> concatMap identity (HashMap.toList $ gamePlayers g)
      <> [ (VillainRef vid, named $ cdName $ getCardDef leader)
         | isJust (gameCompetition g)
         , let opponent = if aid == CW.teamArea CW.Registration then CW.teamArea CW.Resistance else CW.teamArea CW.Registration
         , Just vid <- [activeVillainIn g opponent]
         , Just leader <- [lookup vid $ gameVillains g]
         ]

-- | Who "you" is: the player 'asYou' names, or else the active player.
getYou :: (HasGame m) => m IdentityId
getYou = getsGame \g -> fromMaybe (g.gameActivePlayer) (gameYou g)

-- | Look at the game as the player given: "you" on a player card is its controller, or
-- the player who would play or trigger it (RR "You").
asYou :: (HasGame m) => IdentityId -> ReaderT Game m a -> m a
asYou ident body = getGame >>= runReaderT body . (youL ?~ ident)

-- | An affordability query carries the ability being paid for without changing the
-- stored game. Resource abilities can then distinguish a named ability from a card.
asPayingForAbility :: HasGame m => Ability -> ReaderT Game m a -> m a
asPayingForAbility ability body = do
  g <- getGame
  let context = (, abilityIndex ability) . toCardCode <$> abilityCardDef g (refocus $ abilitySource ability)
  runReaderT body g {gamePayingForAbility = context}


-- * Game areas
getAreas :: (HasGame m) => m [Area]
getAreas = getsGame gameAreas

-- | The area a game element is in, by its ref: a minion is where the player it is
-- engaged with is, a player's card where that player is, and what no area lists is in
-- the first.
areaOfTarget :: Game -> Target -> Maybe Area
areaOfTarget g = \case
  IdentityRef i | Just c <- gameCompetition g, Just team <- CW.playerTeam c i -> listing ((== CW.teamArea team) . areaId)
  IdentityRef i -> listing (elem i . areaPlayers)
  VillainRef v -> listing (elem v . areaVillains)
  EnemyRef (EnemyVillainId v) -> areaOfTarget g (VillainRef v)
  EnemyRef (EnemyMinionId m) -> areaOfTarget g (MinionRef m)
  CharacterRef (IdentityCharacter i) -> areaOfTarget g (IdentityRef i)
  CharacterRef (AllyCharacter a) -> areaOfTarget g (AllyRef a)
  CharacterRef (VillainCharacter v) -> areaOfTarget g (VillainRef v)
  CharacterRef (MinionCharacter m) -> areaOfTarget g (MinionRef m)
  MainSchemeRef s -> listing (elem s . areaMainSchemes)
  SideSchemeRef s -> listing (elem s . areaSideSchemes)
  SchemeRef (SchemeMainSchemeId s) -> areaOfTarget g (MainSchemeRef s)
  SchemeRef (SchemeSideSchemeId s) -> areaOfTarget g (SideSchemeRef s)
  EnvironmentRef e -> listing (elem e . areaEnvironments)
  MinionRef m -> player . getMinionEngagedIdentity =<< lookup m (gameMinions g)
  AllyRef a -> player . getAllyController =<< lookup a (gameAllies g)
  SupportRef s -> player . getSupportController =<< lookup s (gameSupports g)
  UpgradeRef u -> lookup u (gameUpgrades g) >>= \upgrade ->
    let attrs = toAttrs upgrade
        host = if isNothing (gameCompetition g) then Nothing else
          (EnemyRef <$> upgradeAttachedEnemy attrs)
            <|> (AllyRef <$> upgradeAttachedAlly attrs)
            <|> (IdentityRef <$> Upgrade.upgradeAttachedIdentity attrs)
            <|> (SideSchemeRef <$> upgradeAttachedSideScheme attrs)
            <|> (MainSchemeRef <$> upgradeAttachedMainScheme attrs)
     in maybe (player $ getUpgradeController upgrade) (areaOfTarget g) host
  EventRef eid -> player . eventController . toAttrs =<< lookup eid (gameEvents g)
  CardIdRef cid | isJust (gameCompetition g) ->
    (player . fst =<< (handCard g cid <|> pendingPaymentCard g cid)) <|> headMay (gameAreas g)
  EffectRef eid | Just competition <- gameCompetition g ->
    let recorded = (\aid -> listing ((== aid) . areaId)) <$> Data.List.lookup (EffectRef eid) (CW.competitiveEffectAreas competition)
        sourceArea = do
          effect <- lookup eid (gameEffects g)
          let attrs = toAttrs effect
          case Effect.effectMatcher attrs of
            SourceEntity source | source /= GameRef -> areaOfTarget g (refocus source)
            MainSchemeEntity sid -> areaOfTarget g (MainSchemeRef sid)
            _ -> join recorded <|> areaOfTarget g (refocus $ Effect.effectSource attrs)
     in sourceArea <|> join recorded

  -- an obligation is in its player's play area, an attachment where what it is on is
  ObligationRef o -> player =<< Obligation.obligationOwner . toAttrs =<< lookup o (gameObligations g)
  AttachmentRef a | Just u <- attachmentUpgrade g a -> areaOfTarget g (UpgradeRef $ toId u)
  AttachmentRef a -> lookup a (gameAttachments g) >>= \x ->
    let at = toAttrs x
     in headMay $
          catMaybes
            [ areaOfTarget g . EnemyRef =<< Attachment.attachmentEnemy at
            , player =<< Attachment.attachmentIdentity at
            , areaOfTarget g . AllyRef =<< Attachment.attachmentAlly at
            , areaOfTarget g . SideSchemeRef =<< Attachment.attachmentSideScheme at
            , areaOfTarget g . MainSchemeRef =<< Attachment.attachmentMainScheme at
            ]
  _ -> headMay (gameAreas g)
 where
  listing p = areaListing p (gameAreas g)
  player = areaOfTarget g . IdentityRef

getAreaOfTarget :: (HasGame m, IsRef a) => a -> m AreaId
getAreaOfTarget a = getsGame \g -> maybe defaultAreaId areaId (areaOfTarget g (toTarget a))

-- | What of the map is in the area, by its key.
keysInArea :: (HasGame m) => (k -> Target) -> (Game -> HashMap k v) -> AreaId -> m [k]
keysInArea ref entities aid = getsGame \g -> [k | k <- HashMap.keys (entities g), (areaId <$> areaOfTarget g (ref k)) == Just aid]

-- | In the area, whoever's area "you" are in.
getMainSchemesIn :: (HasGame m) => AreaId -> m [MainSchemeId]
getMainSchemesIn = keysInArea MainSchemeRef gameMainSchemes

getMinionsIn :: (HasGame m) => AreaId -> m [MinionId]
getMinionsIn = keysInArea MinionRef gameMinions

getObligationsIn :: (HasGame m) => AreaId -> m [ObligationId]
getObligationsIn = keysInArea ObligationRef gameObligations

-- | Obligations in this player's play area. An obligation attached to an
-- encounter-area side scheme stays with that scheme when the player leaves.
getObligationsOf :: HasGame m => IdentityId -> m [ObligationId]
getObligationsOf ident = getsGame $ map toId . filter (\obligation ->
  let attrs = toAttrs obligation
   in Obligation.obligationOwner attrs == Just ident && isNothing (Obligation.obligationSideScheme attrs)) . HashMap.elems . gameObligations

getSideSchemesIn :: (HasGame m) => AreaId -> m [SideSchemeId]
getSideSchemesIn = keysInArea SideSchemeRef gameSideSchemes

-- | The player's nemesis minions, out of play: set aside with their nemesis set, or in
-- the encounter deck or its discard pile. Each copy of a nemesis minion is one (Venom's
-- Enraged Symbiotes, rulesheet FAQ).
getNemesisMinionsOutOfPlay :: (HasGame m) => IdentityId -> m [EncounterCard]
getNemesisMinionsOutOfPlay ident = do
  identity <- getsGame $ lookup ident . gamePlayers
  case identity of
    Nothing -> pure []
    Just player -> do
      key <- getDeckKeyFor ident
      competitive <- isJust <$> getCompetition
      aside <- if competitive then getSetAsideEncounterCardsForDeck key else getSetAsideEncounterCards
      pile <- getPile key
      let set = getNemesisSet (toCardCode player)
          cards = aside <> pileDeck pile <> pileDiscard pile
      pure [card | card <- cards, cdEncounterSet (getCardDef card) == Just set, Minions.isNemesisMinion (getCardDef card)]

-- | Whether a patrol minion is engaged with the player: they cannot thwart the main
-- scheme with the cards they control, by a basic thwart or an ability labeled thwart (RR
-- Patrol, Thwart); removing threat otherwise is not a thwart.
isPatrolled :: (HasGame m) => IdentityId -> m Bool
isPatrolled ident = do
  ignores <- elem IgnoresPatrol <$> getModifiers (IdentityRef ident)
  if ignores then pure False else selectAny $ MinionEngagedWith (IdentityWithId ident) <> MinionWithKeyword Patrol

-- | The card code of the main scheme in play.
getMainSchemeCode :: (HasGame m) => MainSchemeId -> m (Maybe CardCode)
getMainSchemeCode mid = getsGame (fmap (toCardCode . getCardDef) . lookup mid . gameMainSchemes)

-- | The area the player is in.
getAreaOf :: (HasGame m) => IdentityId -> m Area
getAreaOf ident = getsGame \g -> fromMaybe (newArea defaultAreaId []) (areaOfTarget g (IdentityRef ident))

-- | The area "you" are in: what "the villain", "the main scheme" and "the encounter
-- deck" are there.
getYourArea :: (HasGame m) => m Area
getYourArea = getAreaOf =<< getYou

-- | Whether the game element is in "your" area. With one area everything is.
inYourArea :: (HasGame m) => Target -> m Bool
-- an environment is in every area (RR FAQ, The Once and Future Kang)
inYourArea (EnvironmentRef _) = pure True
inYourArea t = do
  you <- getYou
  getsGame \g -> if isMissionTarget g t && not (gameMissionTargets g) then False else case gameAreas g of
    _ : _ : _ | not (gameAllAreas g) -> (areaId <$> areaOfTarget g t) == (areaId <$> areaOfTarget g (IdentityRef you))
    _ -> True

-- | Looking in every area: whether a game element matches is so wherever it is, as only
-- what may be chosen or affected is "your" area's.
inAnyArea :: (HasGame m) => ReaderT Game m a -> m a
inAnyArea body = getGame >>= runReaderT body . (allAreasL .~ True)

-- | Only those in "your" area: players cannot target game elements in other areas.
inYourAreaOnly :: (HasGame m) => (a -> Target) -> [a] -> m [a]
inYourAreaOnly ref = filterM (inYourArea . ref)

-- | The villains in play in the area, as it lists them.
villainsIn :: Game -> AreaId -> [VillainId]
villainsIn g aid = case break ((== aid) . areaId) (gameAreas g) of
  (before, a : _) -> filter inPlay (areaVillains a) <> (if null before then unlisted else [])
  _ -> []
 where
  inPlay v = HashMap.member v (gameVillains g)
  unlisted = filter (`notElem` concatMap areaVillains (gameAreas g)) (HashMap.keys $ gameVillains g)

getVillainsInArea :: (HasGame m) => AreaId -> m [VillainId]
getVillainsInArea aid = getsGame (`villainsIn` aid)

-- | The area's active villain: the one it names while he is in play, or else the first
-- of its villains.
activeVillainIn :: Game -> AreaId -> Maybe VillainId
activeVillainIn g aid = case [a | a <- gameAreas g, areaId a == aid] of
  a : _ | Just v <- areaActiveVillain a, HashMap.member v (gameVillains g) -> Just v
  _ -> headMay (villainsIn g aid)

getActiveVillainIn :: (HasGame m) => AreaId -> m (Maybe VillainId)
getActiveVillainIn aid = getsGame (`activeVillainIn` aid)

-- | What "the encounter deck" is for the player: their area's active villain's own deck,
-- when he has one, or else the encounter deck.
getDeckKeyFor :: (HasGame m) => IdentityId -> m DeckKey
getDeckKeyFor ident = do
  aid <- areaId <$> getAreaOf ident
  getsGame \g -> case activeVillainIn g aid of
    Just v | HashMap.member (VillainDeckKey v) (scenarioDecks $ toAttrs $ g.gameScenario) -> VillainDeckKey v
    _ | Just competition <- gameCompetition g
      , isJust (CW.leaderTeam $ toId $ g.gameScenario)
      , Just team <- CW.playerTeam competition ident -> CW.pendingEncounterDeck team
    _ -> EncounterDeckKey

-- | The counters on the main scheme: Zola's test counters, say.
getMainSchemeCounters :: (HasGame m) => MainSchemeId -> m Natural
getMainSchemeCounters ms = getsGame (maybe 0 (mainSchemeCounters . toAttrs) . lookup ms . gameMainSchemes)

-- | The player cards set aside out of play, as an encounter set's Captive allies.
getSetAsidePlayerCards :: (HasGame m) => m [PlayerCard]
getSetAsidePlayerCards = getsGame \g -> [c | PlayerCard c <- scenarioSetAsideCards (toAttrs $ g.gameScenario)]

-- | The deck and its discard pile, each top card first.
-- | Every encounter deck in the game: the encounter deck, then any others.
getDeckKeys :: (HasGame m) => m [DeckKey]
getDeckKeys = getsGame $ deckKeys . toAttrs . gameScenario

-- | The encounter cards set aside, out of play.
getSetAsideEncounterCards :: (HasGame m) => m [EncounterCard]
getSetAsideEncounterCards = getsGame $ onlyEncounterCards . scenarioSetAsideCards . toAttrs . gameScenario

getSetAsideEncounterCardsForDeck :: HasGame m => DeckKey -> m [EncounterCard]
getSetAsideEncounterCardsForDeck key = getsGame $ \g ->
  let scenario = toAttrs $ g.gameScenario
   in [card | EncounterCard card <- scenarioSetAsideCards scenario
      ,fromMaybe EncounterDeckKey (lookup (ecCardId card) $ scenarioCardDecks scenario) == key]


getPile :: (HasGame m) => DeckKey -> m Pile
getPile = \case
  EncounterDeckKey -> getsGame \g -> let a = toAttrs (g.gameScenario) in Pile (scenarioEncounterDeck a) (scenarioDiscard a)
  key -> getsGame $ fromMaybe emptyPile . lookup key . scenarioDecks . toAttrs . gameScenario

-- | The pure step: the game with the area changed.
overArea :: AreaId -> (Area -> Area) -> Game -> Game
overArea aid f = areasL %~ map \a -> if areaId a == aid then f a else a

-- | The main scheme enters play in the area: its starting threat, then its "When
-- Revealed".
addMainSchemeIn :: (ReverseQueue m) => AreaId -> CardCode -> Game -> m Game
addMainSchemeIn aid cardCode g = do
  mainSchemeId <- getRandom
  case lookupMainScheme cardCode mainSchemeId of
    Just x -> do
      mainSchemeStep mainSchemeId MainSchemePlaceInitialThreat
      mainSchemeStep mainSchemeId RevealMainScheme
      pure $ g & (entitiesL . mainSchemesL . at mainSchemeId ?~ x) & overArea aid (\a -> a {areaMainSchemes = areaMainSchemes a <> [mainSchemeId]})
    Nothing -> throwM $ MissingCardCode "AddMainScheme" cardCode

-- | The game element is put into the area the player is in.
intoAreaOf :: IdentityId -> (Area -> Area) -> Game -> Game
intoAreaOf ident f g = maybe g (\a -> overArea (areaId a) f g) (areaOfTarget g (IdentityRef ident))

-- | The player the villain's activation under way is against: who resolves its boost
-- cards, the "you" of their boost abilities.
getVillainActivationPlayer :: (HasGame m) => VillainId -> m (Maybe IdentityId)
getVillainActivationPlayer vid = getsGame \g -> Villain.villainActivationPlayer . toAttrs =<< lookup vid (gameVillains g)

-- | The villain's attack under way, if one is.
getVillainAttack :: (HasGame m) => VillainId -> m (Maybe Attack)
getVillainAttack vid = getsGame \g -> Villain.villainAttackDetails =<< lookup vid (gameVillains g)

-- | The code the scenario is set up by.
getScenarioCode :: (HasGame m) => m CardCode
getScenarioCode = getsGame (toId . gameScenario)

-- | The villain's ATK, as modified.
getVillainAttackValue :: (HasGame m) => VillainId -> m Natural
getVillainAttackValue vid = getsGame (lookup vid . gameVillains) >>= maybe (pure 0) (Villain.getModifiedAttack . toAttrs)

-- | The villain's SCH, as modified.
getVillainSchemeValue :: (HasGame m) => VillainId -> m Natural
getVillainSchemeValue vid = getsGame (lookup vid . gameVillains) >>= maybe (pure 0) (Villain.getModifiedScheme . toAttrs)

-- | The main scheme that is the villain's own ("Proxima Midnight's Scheme", Tower
-- Defense), which he schemes onto alone.
getPairedMainScheme :: (HasGame m) => VillainId -> m (Maybe MainSchemeId)
getPairedMainScheme vid = getsGame \g -> do
  name <- title . cdName . getCardDef <$> lookup vid (gameVillains g)
  headMay [k | (k, ms) <- HashMap.toList (gameMainSchemes g), mainSchemeVillain (toAttrs ms) == Just name]

-- | How much of an attack's damage would go past the character's remaining hit points:
-- the hero's defense taken off first, none past a tough status card.
getExcessDamage :: (HasGame m) => CharacterId -> Natural -> m Natural
getExcessDamage character damage = case character of
  AttachmentCharacter aid -> subtractNatural <$> getEnemyRemainingHitPoints (EnemyAttachmentId aid) <*> pure damage
  IdentityCharacter ident ->
    getsGame (lookup ident . gamePlayers) >>= \case
      Nothing -> pure 0
      Just p -> do
        hp <- getModifiedHp p
        let a = toAttrs p
            dealt = subtractNatural a.playerIdentityDamageReduction damage
            remaining = subtractNatural a.playerIdentityDamage hp
        pure $ if a.playerIdentityTough && dealt > 0 then 0 else subtractNatural remaining dealt
  AllyCharacter aid ->
    getsGame (lookup aid . gameAllies) >>= \case
      Nothing -> pure 0
      Just ally -> do
        hp <- Ally.getModifiedHitPoints (toAttrs ally)
        let remaining = subtractNatural (getAllyDamage ally) hp
        pure $ if (toAttrs ally).allyTough then 0 else subtractNatural remaining damage
  _ -> pure 0
-- | Every trait of each environment in play.
getEnvironmentTraits :: (HasGame m) => m (HashSet Trait)
getEnvironmentTraits = getsGame $ foldMap (cdTraits . getCardDef) . HashMap.elems . gameEnvironments

-- | The counters on "your" main scheme, as None Shall Pass's delay counters.
getYourMainSchemeCounters :: (HasGame m) => m Natural
getYourMainSchemeCounters = do
  schemes <- filterM (inYourArea . MainSchemeRef . toId) . HashMap.elems =<< getsGame gameMainSchemes
  pure $ sum $ map (mainSchemeCounters . toAttrs) schemes


-- | How many times a "When Revealed" the player reveals resolves: once, and once more
-- for each ability that says so (Media Coverage).
whenRevealedTimes :: (HasGame m) => IdentityId -> m Int
whenRevealedTimes ident = (+ 1) . fromIntegral . count (== ResolveWhenRevealedAgain) <$> getModifiers ident

-- | The player a boost card turned up for the enemy's activation is resolved by: the
-- one the villain's activation is against, or the player whose turn it is.
getBoostedPlayer :: (HasGame m) => EnemyId -> m IdentityId
getBoostedPlayer enemy = getsGame gameBoostPlayerOverride >>= maybe (getActualBoostedPlayer enemy) pure

getActualBoostedPlayer :: HasGame m => EnemyId -> m IdentityId
getActualBoostedPlayer enemy = do
  friendly <- getsGame $ \g -> case enemy of EnemyMinionId mid -> friendlyProxyAlly g mid >>= (fmap getAllyController . (`lookup` gameAllies g)); _ -> Nothing
  attacking <- case enemy of
    EnemyAttachmentId _ -> pure Nothing
    EnemyVillainId vid -> getVillainAttack vid
    EnemyMinionId mid -> getsGame (\g -> (Minion.minionAttacking . toAttrs =<< lookup mid (gameMinions g)) <|> friendlyBoostAttackView g mid)
  defender <- case attackCharacter <$> attacking of
    Just (IdentityCharacter who) -> pure $ Just who
    Just (AllyCharacter aid) -> getSourceController (AllyRef aid)
    _ -> pure Nothing
  case friendly <|> defender of
    Just who -> pure who
    Nothing -> case enemy of
      EnemyAttachmentId _ -> getActivePlayerId
      EnemyVillainId vid -> maybe getActivePlayerId pure =<< getVillainActivationPlayer vid
      EnemyMinionId mid -> getsGame (fmap (\m -> fromMaybe (minionEngagedIdentity $ toAttrs m) (Minion.minionActivationPlayer $ toAttrs m)) . lookup mid . gameMinions) >>= maybe getActivePlayerId pure

-- | The obligations in the player's play area.
countObligationsOf :: (HasGame m) => IdentityId -> m Natural
countObligationsOf ident =
  getsGame $ fromIntegral . length . filter ((== Just ident) . Obligation.obligationOwner . toAttrs) . HashMap.elems . gameObligations

-- | The villain's stage.
getVillainStage :: (HasGame m) => VillainId -> m Natural
getVillainStage vid = getsGame $ maybe 0 (Villain.villainStage . toAttrs) . lookup vid . gameVillains

-- | Whether the damage, from the source, is of an attack all of whose damage to the
-- villain is prevented (Master of Illusions).
villainPreventsAttackDamage :: (HasGame m) => VillainId -> Source -> Damage -> m Bool
villainPreventsAttackDamage vid source damage = getsGame $ maybe False (\v -> Villain.preventedAttackDamage (toAttrs v) source damage) . lookup vid . gameVillains

-- | The villain's remaining hit points.
getVillainHitPoints :: (HasGame m) => VillainId -> m Natural
getVillainHitPoints vid = getsGame $ maybe 0 (fromIntegral . max 0 . unHp . Villain.villainHp . toAttrs) . lookup vid . gameVillains

-- | The enemy's printed hit points: a villain's for the number of players.
getEnemyPrintedHitPoints :: (HasGame m) => EnemyId -> m Natural
getEnemyPrintedHitPoints = \case
  EnemyAttachmentId _ -> pure 3
  EnemyVillainId vid ->
    getsGame (lookup vid . gameVillains) >>= \case
      Nothing -> pure 0
      Just v -> fromIntegral <$> fromGameValue (unHp $ Villain.villainStartingHp $ toAttrs v)
  EnemyMinionId mid -> getsGame $ maybe 0 (unHp . getMinionPrintedHitPoints) . lookup mid . gameMinions

-- | The schemes to which this upgrade is attached, independent of scheme type.
getUpgradeAttachedSchemes :: (HasGame m) => UpgradeId -> m [SchemeId]
getUpgradeAttachedSchemes uid = getsGame $ maybe [] (\u ->
  maybe [] pure (SchemeMainSchemeId <$> upgradeAttachedMainScheme (toAttrs u))
    <> maybe [] pure (SchemeSideSchemeId <$> upgradeAttachedSideScheme (toAttrs u))) . lookup uid . gameUpgrades

-- | The counters on the environment, main scheme or attachment.
obligationIsInPlay :: (HasGame m) => ObligationId -> m Bool
obligationIsInPlay oid = getsGame (HashMap.member oid . gameObligations)

getCountersOn :: (HasGame m, IsRef a) => a -> m Natural
getCountersOn a = getsGame \g -> case counterTarget a of
  EnvironmentRef eid -> maybe 0 (field EnvironmentCounters) (lookup eid $ gameEnvironments g)
  VillainRef vid -> maybe 0 (field Villain.VillainCounters) (lookup vid $ gameVillains g)
  MinionRef mid -> maybe 0 Minion.getMinionDisplayCounters (lookup mid $ gameMinions g)
  MainSchemeRef mid -> maybe 0 (field MainSchemeCounters) (lookup mid $ gameMainSchemes g)
  AttachmentRef aid -> maybe 0 (field AttachmentCounters) (lookup aid $ gameAttachments g)
  IdentityRef ident -> maybe 0 (playerIdentityCounters . toAttrs) (lookup ident $ gamePlayers g)
  ObligationRef oid -> maybe 0 Obligation.getObligationDisplayCounters (lookup oid $ gameObligations g)
  SupportRef sid -> maybe 0 getSupportUses (lookup sid $ gameSupports g)
  AllyRef aid -> maybe 0 getAllyUses (lookup aid $ gameAllies g)
  UpgradeRef uid -> maybe 0 getUpgradeUses (lookup uid $ gameUpgrades g)
  SideSchemeRef sid -> maybe 0 (sideSchemeCounters . toAttrs) (lookup sid $ gameSideSchemes g)
  _ -> 0

-- | How many cards in play have the trait: of every type, each identity one card.
countCardsInPlayWithTrait :: forall m. (HasGame m) => Trait -> m Natural
countCardsInPlayWithTrait t = do
  g <- getGame
  let printed :: [CardDef] -> Natural
      printed = count (member t . cdTraits)
      defs :: (HasCardDef a) => EntityMap a -> [CardDef]
      defs = map getCardDef . HashMap.elems
      traited :: (HasTraits a) => EntityMap a -> m Natural
      traited = fmap (count (member t)) . traverse getTraits . HashMap.elems
  characters <- sum <$> sequence [traited (gamePlayers g), traited (gameAllies g), traited (gameMinions g)]
  pure $
    characters
      + printed (map (supportCardDef . toAttrs) $ HashMap.elems $ gameSupports g)
      + printed
        ( defs (gameVillains g)
            <> defs (gameUpgrades g)
            <> defs (gameAttachments g)
            <> defs (gameTreacheries g)
            <> defs (gameObligations g)
            <> defs (gameSideSchemes g)
            <> defs (gameMainSchemes g)
            <> defs (gameEnvironments g)
        )

getActivePlayer :: (HasGame m) => m PlayerIdentity
getActivePlayer = getsGame $ \g ->
  case lookup (g ^. activePlayerL) (g ^. entitiesL . playersL) of
    Just p -> p
    Nothing -> error "Missing player"

-- | A side scheme that cannot leave play (The Sleeper Awakened) stays where it is.
whenCanLeavePlay :: (HasGame m, IsRef target) => Game -> target -> m Game -> m Game
whenCanLeavePlay g sid body = do
  stays <- elem CannotLeavePlay <$> getModifiers (toTarget sid)
  if stays then pure g else body

-- | A card leaving play for its discard pile: one bound for The Collection goes there
-- instead, and otherwise "when a card would be placed into a discard pile from play"
-- is answered first (Collector).
discardFromPlay :: (ReverseQueue m, IsCard card) => Game -> card -> m ()
discardFromPlay g c
  | toCardId card `elem` gameCollecting g = placeInCollection card
  | otherwise = do
      checkWindows [W.Window W.Would $ W.WouldBeDiscardedFromPlay either']
      discardCard card
      cardSpecific GameRef "afterDiscardFromPlay" card
 where
  card = toCard c
  either' = case card of
    PlayerCard pc -> Left pc
    EncounterCard ec -> Right ec

-- | An attachment alias names the same physical upgrade; never a second entity.
attachmentUpgrade :: Game -> AttachmentId -> Maybe Upgrade
attachmentUpgrade g aid@(AttachmentId uuid)
  | HashMap.member aid (gameAttachments g) = Nothing
  | otherwise = do
      u <- lookup (UpgradeId uuid) (gameUpgrades g)
      _ <- Upgrade.getUpgradeAttachmentTraits u
      pure u

canonicalAttachmentTarget :: Game -> Target -> Target
canonicalAttachmentTarget g target = case target of
  AttachmentRef aid | Just u <- attachmentUpgrade g aid -> UpgradeRef (toId u)
  _ -> target

-- | The card a game element in play was made from, by its ref.
cardInPlay :: Game -> Target -> Maybe Card
cardInPlay g raw = case friendlyBoostTarget g raw of
  VillainRef k@(VillainId uuid) -> (\villain -> EncounterCard $ MkEncounterCard (CardId uuid) (getCardDef villain)) <$> lookup k (gameVillains g)
  MainSchemeRef k -> toCard <$> lookup k (gameMainSchemes g)
  AllyRef k -> toCard <$> lookup k (gameAllies g)
  SupportRef k -> toCard . toAttrs <$> lookup k (gameSupports g)
  UpgradeRef k -> toCard <$> lookup k (gameUpgrades g)
  TreacheryRef k -> toCard <$> lookup k (gameTreacheries g)
  MinionRef k -> toCard <$> lookup k (gameMinions g)
  AttachmentRef k -> (toCard <$> lookup k (gameAttachments g)) <|> (toCard <$> attachmentUpgrade g k)
  SideSchemeRef k -> toCard <$> lookup k (gameSideSchemes g)
  EnvironmentRef k -> toCard <$> lookup k (gameEnvironments g)
  ObligationRef k -> toCard <$> lookup k (gameObligations g)
  _ -> Nothing

-- | The card in play, by its ref: the entity made from it.
inPlayRef :: Game -> Card -> Maybe Target
inPlayRef g card =
  headMay $
    catMaybes
      [ AllyRef k <$ lookup k (gameAllies g)
      , SupportRef k' <$ lookup k' (gameSupports g)
      , UpgradeRef k'' <$ lookup k'' (gameUpgrades g)
      , MinionRef m <$ lookup m (gameMinions g)
      , AttachmentRef a <$ lookup a (gameAttachments g)
      , SideSchemeRef s' <$ lookup s' (gameSideSchemes g)
      , EnvironmentRef e <$ lookup e (gameEnvironments g)
      , ObligationRef o <$ lookup o (gameObligations g)
      ]
 where
  u = unCardId (toCardId card)
  k = AllyId u
  k' = SupportId u
  k'' = UpgradeId u
  m = MinionId u
  a = AttachmentId u
  s' = SideSchemeId u
  e = EnvironmentId u
  o = ObligationId u

-- | The current identity defender, including one established by a defense event.
defendingIdentity :: Game -> EnemyId -> Maybe IdentityId
defendingIdentity g enemy = do
  details <- case enemy of
    EnemyVillainId vid -> lookup vid (gameVillains g) >>= villainAttackDetails
    EnemyMinionId mid -> lookup mid (gameMinions g) >>= minionAttackDetails
    EnemyAttachmentId _ -> Nothing
  case attackCharacter details of
    IdentityCharacter who | attackDefended details -> Just who
    _ -> Nothing

offerClaimedBasicDefense :: ReverseQueue m => IdentityId -> EnemyId -> m ()
offerClaimedBasicDefense who enemy = do
  canDefend <- identityMatches (HeroIdentity <> IdentityCanDefendReady) who
  alreadyDefended <- getsGame $ maybe False (playerIdentityDefended . toAttrs) . lookup who . gamePlayers
  forbidden <- elem AttacksCannotBeDefended <$> getModifiers (enemyRef enemy)
  when (canDefend && not alreadyDefended && not forbidden) $ chooseOnePromptedAbout who "$label.declareDefender" enemy
    [ pure (Defend enemy)
    , labeled "$label.declineToDefend" (pure ())
    ]

-- | A boost-effect interrupt before reveal still turns the original card faceup.
-- Icons, After windows, entity identity and eventual discard remain unchanged.
revealBoostCardFor :: (ReverseQueue m) => Bool -> EncounterCard -> EnemyId -> Game -> m Game
revealBoostCardFor allowAbility card enemyId g = do
  player <- getActivePlayerId
  let
    -- after it is turned faceup, before its boost ability resolves
    turnedFaceup target = do
      checkWindows [W.Window After $ W.FlippedBoostCard card enemyId]
      when allowAbility $ revealedAsBoost target enemyId
      push $ BoostCardResolved enemyId card
  case cdCardType (getCardDef card) of
    TreacheryType -> do
      let treachery = createTreachery card
      turnedFaceup (toTarget treachery)
      pure $
        g
          & (boostEntitiesL . treacheriesL %~ insertMap (toId treachery) treachery)
          & (boostCardsL . at (toTarget treachery) ?~ card)
    MinionType -> do
      let minion = createMinion player card
      turnedFaceup (toTarget minion)
      pure $
        g
          & (boostEntitiesL . minionsL %~ insertMap (toId minion) minion)
          & (boostCardsL . at (toTarget minion) ?~ card)
    AttachmentType -> do
      let attachment = createAttachment card
      turnedFaceup (toTarget attachment)
      pure $
        g
          & ( boostEntitiesL
                . attachmentsL
                %~ insertMap (toId attachment) attachment
            )
          & (boostCardsL . at (toTarget attachment) ?~ card)
    ObligationType -> do
      let obligation = createObligation card
      turnedFaceup (toTarget obligation)
      pure $
        g
          & ( boostEntitiesL
                . obligationsL
                %~ insertMap (toId obligation) obligation
            )
          & (boostCardsL . at (toTarget obligation) ?~ card)
    SideSchemeType -> do
      let sideScheme = createSideScheme card
      turnedFaceup (toTarget sideScheme)
      pure $
        g
          & ( boostEntitiesL
                . sideSchemesL
                %~ insertMap (toId sideScheme) sideScheme
            )
          & (boostCardsL . at (toTarget sideScheme) ?~ card)
    EnvironmentType -> do
      let environment = createEnvironment card
      turnedFaceup (toTarget environment)
      pure $
        g
          & (boostEntitiesL . environmentsL %~ insertMap (toId environment) environment)
          & (boostCardsL . at (toTarget environment) ?~ card)
    -- a card with no boost ability, a player's event shuffled in among them (a Cosmic
    -- Entity), is discarded with the others: to the encounter discard pile (RR FAQ)
    _ -> do
      checkWindows [W.Window After $ W.FlippedBoostCard card enemyId]
      push $ BoostCardResolved enemyId card
      pure $ g & (boostCardsL . at (CardIdRef $ ecCardId card) ?~ card)

perPhase :: Limit -> Bool
perPhase = \case
  PerPhase _ -> True
  _ -> False

-- | Work out an event's selected ability before checking its labels. Keep the
-- original card intact: zone membership and payment refer to that physical card.
effectivePlayerCardDef :: (HasGame m) => IdentityId -> PlayerCard -> m CardDef
effectivePlayerCardDef ident card
  | cdCardType def == EventType = case createEvent ident card of
      Event event -> do
        labels <- eventAbilitySubTypes event
        cost <- if toCardCode card == "61026" then ExhaustCharacterCost <$> JLHelpers.joysExhaustMatcher else pure (cdAbilityCost def)
        pure def {cdAbilitySubTypes = labels, cdAbilityCost = cost}
  | otherwise = pure def
 where
  def = getCardDef card

getWindowPlayableCards ::
  ( MonadThrow m
  , MonadRandom m
  , HasQueue m
  , HasGame m
  , Projection m PlayerIdentity
  ) =>
  Window ->
  PlayerIdentity ->
  m [PlayerCard]
getWindowPlayableCards window player = do
  held <- getHeldPlayableCards (toId player)
  hand <- getPlayableHandCards (toId player)
  filterM (isWindowPlayable window player) (hand <> held)

isWindowPlayable ::
  ( MonadThrow m
  , MonadRandom m
  , HasQueue m
  , HasGame m
  , Projection m PlayerIdentity
  ) =>
  Window ->
  PlayerIdentity ->
  PlayerCard ->
  m Bool
isWindowPlayable window original c = do
  -- Most cards cannot respond to this window. Reject them before preparing
  -- top-deck play or checking entry restrictions, both of which query modifiers.
  passedWindow <- maybe (pure False) (\matcher -> windowMatches matcher window GameRef) (cdResponseWindow def)
  if not passedWindow then pure False else do
    entryAllowed <- playerCardEntryAllowed c
    attrs <- previewHandPlay original c
    if not entryAllowed then pure False else do
      effectiveDef <- effectivePlayerCardDef ident c
      blockedDefense <- if Ability.Defense `elem` cdAbilitySubTypes effectiveDef
        then getsGame $ isDefendingOtherSimultaneousAttack ident
        else pure False
      if blockedDefense then pure False else do
        affordable <- notNull <$> playableTargets attrs c
        passedCriteria <- passesCardCriteria ident c (cdCriteria effectiveDef <> toAdditionalCriteria effectiveDef)
        passedPlayLimits <- checkPlayLimits attrs c
        forbidden <- any (\case CannotPlayCards matcher -> cardMatch matcher c; _ -> False) <$> getModifiers ident
        permitted <- canResolvePlayerCardAbilities ident
        canDefend <- if toCardCode c == "45031" then case window of
          W.Window _ (W.EnemyAttack enemy _) -> notElem AttacksCannotBeDefended <$> getModifiers (enemyRef enemy)
          _ -> pure False
          else pure True
        pure $ permitted && canDefend && not forbidden && affordable && passedCriteria && passedPlayLimits
 where
  ident = toId original
  def = getCardDef c

abilityInWindow :: (HasGame m) => Window -> Ability -> m Bool
abilityInWindow window a =
  maybe
    (pure False)
    (\matcher -> windowMatches matcher window source)
    (abilityWindow a)
 where
  source = abilitySource a

-- | Cancel only the reveal abilities: the card still enters its ordinary zone.
-- RR1.8 treats incite and surge as When Revealed abilities too.
resolveEncounterReveal :: (ReverseQueue m) => Bool -> W.RevealSource -> IdentityId -> EncounterCard -> Game -> m Game
resolveEncounterReveal resolveWhenRevealed source ident card g = do
  -- each "When Revealed" resolves once, and once more for each ability that says so
  -- (Media Coverage); incite and surge are "When Revealed" abilities (RR)
  times <- if resolveWhenRevealed then whenRevealedTimes ident else pure 0
  revealModifiers <- getRevealingCardModifiers card
  let dynamicPeril = KeywordModifier Peril `elem` revealModifiers
  when dynamicPeril $ push (SetPeril $ Just ident)
  let whenRevealed = replicateM_ times
  -- a card with a "When Revealed" waits for the player to resolve it, so each player
  -- sees what is happening; then incite, its threat first, then the rest of the card (a
  -- treachery's after the window as it is revealed, which may cancel it)
  let revealingIf hasEffect t = do
        when (resolveWhenRevealed && hasEffect && hasWhenRevealed (toCardCode card)) $ askToResolveWhenRevealed ident t
        whenRevealed (incite card)
      revealing = revealingIf True
  -- surge is resolved after the rest of the card; treacheries surge as they resolve
  let surges = when (Marvel.Keyword.Surge `member` cdKeywords (getCardDef card)) (whenRevealed $ surge ident)
  result <- case cdCardType (getCardDef card) of
    AllyType -> do
      let playerCard = MkPlayerCard (ecCardId card) (getCardDef card) (Just ident) (Just ident)
          aid = AllyId $ unCardId $ ecCardId card
      if not resolveWhenRevealed && hasWhenRevealed (toCardCode card) then discardCard card else do
        putIntoPlay ident playerCard
        when (hasWhenRevealed $ toCardCode card) $ whenRevealed $ do
          askToResolveWhenRevealed ident (AllyRef aid)
          allyStep aid (AllyRevealed ident)
        surges
      pure g
    AttachmentType -> do
      let attachment = createAttachment card
      modifiers <- runReaderT (getModifiers attachment) (g & entitiesL . attachmentsL %~ insertMap (toId attachment) attachment)
      rest <- capture do
        revealing (toTarget attachment)
        if not resolveWhenRevealed && hasWhenRevealed (toCardCode card)
          then removeFromPlay attachment
          else attachmentStep (toId attachment) (RevealAttachment ident)
        checkWindows [W.Window W.After $ W.EncounterAttachmentEntered $ toId attachment]
        when (Marvel.Keyword.Surge `member` cdKeywords (getCardDef card) || KeywordModifier Marvel.Keyword.Surge `elem` modifiers) $ whenRevealed $ surge ident
      push $ EntersPlayUnlessUnique ident True (toTarget attachment) rest
      pure $
        g
          & (entitiesL . attachmentsL %~ insertMap (toId attachment) attachment)
    MinionType | Just minion <- lookup (MinionId $ unCardId $ ecCardId card) (gameMinions g) -> do
      -- Finding and revealing an existing minion is not a new entry (RR Find).
      unless (getMinionEngagedIdentity minion == ident) $ engageMinion (toId minion) ident
      hasEffect <- case minion of Minion.Minion m -> Minion.minionWhenRevealedHasEffect m
      revealingIf hasEffect (toTarget minion)
      focusing [toCard minion] $ whenRevealed $ minionStep (toId minion) (RevealMinion ident)
      modifiers <- getModifiers minion
      when (Marvel.Keyword.Surge `member` cdKeywords (getCardDef card) || KeywordModifier Marvel.Keyword.Surge `elem` modifiers) $ whenRevealed $ surge ident
      pure g
    MinionType -> do
      let
        minion = createMinion ident card
      -- "when it would enter play" (the Nebula minion) comes before a matching unique
      -- card keeps it out
      focusing [toCard minion] $ checkWindows [W.Window When $ W.MinionEnteredPlay $ toId minion]
      rest <- capture do
        hasEffect <- case minion of Minion.Minion m -> Minion.minionWhenRevealedHasEffect m
        revealingIf hasEffect (toTarget minion)
        cardSpecific (MinionRef $ toId minion) "applyToughness" ()
        focusing [toCard minion] $ whenRevealed $ minionStep (toId minion) (RevealMinion ident)
        engageMinion (toId minion) ident
        checkWindows [W.Window After $ W.MinionEnteredPlay $ toId minion]
        modifiers <- runReaderT (getModifiers minion) (g & entitiesL . minionsL %~ insertMap (toId minion) minion)
        when (Marvel.Keyword.Surge `member` cdKeywords (getCardDef card) || KeywordModifier Marvel.Keyword.Surge `elem` modifiers) $ whenRevealed $ surge ident
      push $ EntersPlayUnlessUnique ident True (toTarget minion) rest
      pure $ g & (entitiesL . minionsL %~ insertMap (toId minion) minion)
        & attacksThisTurnL %~ filter ((/= EnemyMinionId (toId minion)) . snd)
    TreacheryType -> do
      let treachery = createTreachery card
          peril = Peril `member` cdKeywords (getCardDef card)
      -- with peril, no one else may help while the player resolves it (RR Peril)
      when peril $ push (SetPeril (Just ident))
      -- TODO: FOCUS CARDS SHOULD BE CARDS...
      if resolveWhenRevealed then do
        focusing [toCard treachery] do
          checkWindows [W.Window When $ W.RevealTreachery (toId treachery) source]
          askToResolveWhenRevealed ident treachery
          whenRevealed $ treacheryStep (toId treachery) TreacheryIncite
          whenRevealed $ treacheryStep (toId treachery) $ RevealTreachery ident
          treacheryStep (toId treachery) $ CheckTreacheryCondition ident
        treacheryStep (toId treachery) $ ResolvedTreachery ident
      else removeFromPlay treachery
      when peril $ push (SetPeril Nothing)
      pure $
        g
          & (entitiesL . treacheriesL %~ insertMap (toId treachery) treachery)
          & treacheriesRevealedThisPhaseL <>~ [(ident, toId treachery)]
    ObligationType -> do
      let
        obligation = createObligation card
        -- "Give to the Peter Parker player": the player whose identity it comes with
        -- resolves it; with them gone it is removed from the game and another card is
        -- revealed. ponytail: every obligation so far is given to its identity's player
        owners
          -- one of an encounter set is the revealing player's (RR, Obligation)
          | isJust (cdEncounterSet (getCardDef card)) || Labor `member` cdTraits (getCardDef card) = [ident]
          | otherwise =
              [ toId p | p <- HashMap.elems (gamePlayers g), getCardDef card `elem` identityObligations p
              ]
      case owners of
        -- "ignore the card's ability, remove it from the game, and reveal an
        -- additional encounter card": not surge
        [] -> do
          drawAndRevealEncounterCard ident
          pure g
        owner : _ -> do
          whenRevealed (incite card)
          -- TODO: FOCUS CARDS SHOULD BE CARDS...
          obligationStep (toId obligation) (ObligationOwnedBy owner)
          unless (not resolveWhenRevealed && hasWhenRevealed (toCardCode card)) $
            focusing [toCard obligation] $
              obligationStep (toId obligation) $ RevealObligation owner
          obligationStep (toId obligation) $ ResolvedObligation owner
          pure $
            g
              & (entitiesL . obligationsL %~ insertMap (toId obligation) obligation)
    SideSchemeType | Just scheme <- lookup (SideSchemeId $ unCardId $ ecCardId card) (gameSideSchemes g) -> do
      -- An already-present card cannot cross into play a second time.
      sideSchemeStep (toId scheme) (SideSchemeRevealedBy ident)
      revealing (toTarget scheme)
      whenRevealed $ sideSchemeStep (toId scheme) RevealSideScheme
      surges
      pure g
    SideSchemeType -> do
      let sideScheme = createSideScheme card
      sideSchemeStep (toId sideScheme) (SideSchemeRevealedBy ident)
      revealing (toTarget sideScheme)
      sideSchemeStep (toId sideScheme) SideSchemePlaceInitialThreat
      checkWindows [W.Window W.After $ W.SideSchemeEnteredPlay (toId sideScheme)]
      whenRevealed $ sideSchemeStep (toId sideScheme) RevealSideScheme
      surges
      pure $
        g
          & (entitiesL . sideSchemesL %~ insertMap (toId sideScheme) sideScheme)
          & intoAreaOf ident (\a -> a {areaSideSchemes = areaSideSchemes a <> [toId sideScheme]})
    EnvironmentType -> do
      let environment = createEnvironment card
      revealing (toTarget environment)
      spellOwnedBy ident environment
      whenRevealed $ environmentStep (toId environment) (RevealEnvironment ident source)
      environmentStep (toId environment) EnvironmentEntered
      surges
      pure $ g & environmentIntoPlay ident environment
    -- a player's event shuffled into the encounter deck (a Cosmic Entity, Adam
    -- Warlock's starter deck): its "When Revealed" resolves for the player revealing
    -- it, once, and cannot be cancelled, so no window opens for it
    EventType -> do
      let event = createEvent ident (MkPlayerCard (ecCardId card) (ecCardDef card) (Just ident) (Just ident))
      focusing [EncounterCard card] do
        askToResolveWhenRevealed ident (EventRef $ toId event)
        eventStep (toId event) (RevealedEvent ident)
      pure $ g & (entitiesL . eventsL %~ insertMap (toId event) event)
    _ -> error "Unhandled"
  checkWindows [W.Window W.After $ W.EncounterCardRevealed source ident card, W.Window W.After $ W.EncounterCardResolved ident card]
  when dynamicPeril $ push (SetPeril Nothing)
  let encounterSource = CardIdRef (ecCardId card)
  abilityResolved encounterSource
  pure result {gameResolvingAbilitySubTypes = (encounterSource, [], False) : gameResolvingAbilitySubTypes result}

-- | Inspect a possible spending interrupt's counter generation without mutating
-- the game. This is used only to decide whether beginning a payment is legal.
withAdditionalIdentityCounters :: HasGame m => [(IdentityId, Natural)] -> ReaderT Game m a -> m a
withAdditionalIdentityCounters gains body = do
  g <- getGame
  let add state (who, n) = state & entitiesL . playersL . ix who %~ \(PlayerIdentity attrs) ->
        PlayerIdentity attrs {playerIdentityCounters = playerIdentityCounters attrs + n}
  runReaderT body $ refreshTraits $ foldl' add g gains

-- | Continuous cost-setting abilities may change the resource cost while it is
-- being paid. The original selected target and printed X remain fixed.
recalculateCardPaymentCost :: (HasGame m, MonadThrow m, Projection m PlayerIdentity) => IdentityId -> PlayerCard -> Payment -> m Cost
recalculateCardPaymentCost who card payment = do
  player <- getIdentity who
  let chosenX = if cdCost (getCardDef card) == Just (-1)
        then Just $ sum [n | XPayment n <- payments payment]
        else Nothing
  getCardCostWithX chosenX player card (paymentTarget payment)

-- | A labeled thwart's first threat-removal step marks it before opening the
-- interrupt; later instances belong to that same thwart.
sourceHasBegunLabeledThwart :: HasGame m => Source -> m Bool
sourceHasBegunLabeledThwart source = do
  g <- getGame
  case headMay [begun | (active, _, begun) <- gameResolvingAbilitySubTypes g, active == source] of
    Just begun -> pure begun
    Nothing -> pure $ case source of
      EventRef eid -> maybe False (eventThwarted . toAttrs) $ lookup eid $ gameEvents g
      _ -> False

-- | The departure is bracketed once, before the departing card sees the message.
-- The continuation goes through ordinary entity handlers without reopening it.
-- | A departure is intercepted while the minion and its attachments are still
-- present, before any attached cards or held cards are discarded.
departingMinion :: Message -> Maybe MinionId
departingMinion = \case
  RemoveFromPlay (MinionRef mid) -> Just mid
  ReturnToHand (MinionRef mid) -> Just mid
  RemoveFromGame (MinionRef mid) -> Just mid
  AddToVictoryDisplay (MinionRef mid) -> Just mid
  _ -> Nothing

-- | Printed defeat abilities pause while their card is still in play. Ordinary
-- defeats without an ability need no acknowledgement.
whenDefeatedTarget :: Message -> Game -> Maybe Target
whenDefeatedTarget msg g = case msg of
  SideSchemeMessage sid DefeatSideScheme -> do
    card <- lookup sid (gameSideSchemes g)
    guard $ hasWhenDefeated (toCardCode $ getCardDef card)
    pure $ toTarget sid
  MinionMessage mid MinionDefeated -> do
    card <- lookup mid (gameMinions g)
    -- Madame Masque asks in her source-aware defeat step, before Foul Play.
    guard $ not (Minion.minionIgnoreWhenDefeated $ toAttrs card)
    guard $ toCardCode (getCardDef card) /= "24010"
    guard $ hasWhenDefeated (toCardCode $ getCardDef card)
    pure $ toTarget mid
  VillainMessage vid VillainDefeated -> do
    card <- lookup vid (gameVillains g)
    guard $ hasWhenDefeated (toCardCode $ getCardDef card)
    pure $ toTarget vid
  AllyMessage aid AllyDefeatedByDamage -> whenDefeatedTarget (AllyMessage aid AllyDefeated) g
  AllyMessage aid AllyDefeated -> do
    card <- lookup aid (gameAllies g)
    guard $ hasWhenDefeated (toCardCode $ getCardDef card)
    pure $ toTarget aid
  _ -> Nothing

departingAlly :: Message -> Maybe AllyId
departingAlly = \case
  RemoveFromGame (CardIdRef cid) -> Just $ AllyId $ unCardId cid
  RemoveFromPlay (AllyRef aid) -> Just aid
  ReturnToHand (AllyRef aid) -> Just aid
  RemoveFromGame (AllyRef aid) -> Just aid
  AddToVictoryDisplay (AllyRef aid) -> Just aid
  _ -> Nothing

-- | Normalize cards at message boundaries, before any entity or window sees them.
-- Ordinary messages are untouched. CardSpecific already carries JSON, so inspect
-- that existing value rather than encoding/decoding the surrounding message.
resolveMessageCardCosts :: Int -> Message -> Message
resolveMessageCardCosts players = go
 where
  def = resolvePlayerCountCost players
  player card = card {pcCardDef = def $ pcCardDef card}
  encounter card = card {ecCardDef = def $ ecCardDef card}
  card = \case PlayerCard c -> PlayerCard $ player c; EncounterCard c -> EncounterCard $ encounter c
  snapshot :: (ToJSON a, FromJSON a) => a -> a
  snapshot value = case fromJSON (resolveCardCostValues players $ toJSON value) of
    Success result -> result
    Error err -> error $ "invalid normalized card snapshot: " <> err
  identity = \case
    PlayingCard c window -> PlayingCard (player c) window
    PayForCard c window target -> PayForCard (player c) window target
    PlayedCard c window -> PlayedCard (player c) window
    PaidWithCard c -> PaidWithCard $ player c
    AddToHand c -> AddToHand $ player c
    ShuffleIntoIdentityDeck cs -> ShuffleIntoIdentityDeck $ map player cs
    ChosenFromDiscard target rules lo hi cs -> ChosenFromDiscard target rules lo hi $ map player cs
    DiscardedFor target zone lo hi cs -> DiscardedFor target zone lo hi $ map card cs
    SetUpWeatherDeck cs -> SetUpWeatherDeck $ map player cs
    ChooseWeatherSupport c special -> ChooseWeatherSupport (player c) special
    PlaceOnBottomOfDeck c -> PlaceOnBottomOfDeck $ player c
    PlaceOnTopOfDeck c -> PlaceOnTopOfDeck $ player c
    other -> other
  go = \case
    IdentityMessage who msg -> IdentityMessage who $ identity msg
    SupportMessage sid (AttachPlayableEvent c) -> SupportMessage sid $ AttachPlayableEvent $ player c
    SupportMessage sid (ReserveHeldCard c) -> SupportMessage sid $ ReserveHeldCard $ player c
    CardSpecific target key value -> CardSpecific target key $ resolveCardCostValues players value
    PutCardIntoPlay who c payment window played -> PutCardIntoPlay who (player c) payment window played
    PlayCardWithReduction who c reduction -> PlayCardWithReduction who (player c) reduction
    PlayCardIgnoringResourceCost who c -> PlayCardIgnoringResourceCost who $ player c
    ResolveEventWithoutPlaying who c -> ResolveEventWithoutPlaying who $ player c
    PutAllyIntoPlayAsMinion who c -> PutAllyIntoPlayAsMinion who $ player c
    PutObligationIntoPlay who c -> PutObligationIntoPlay who $ player c
    AttachPlayableEventToAlly aid c -> AttachPlayableEventToAlly aid $ player c
    AttachPlayerCardToMinion mid c -> AttachPlayerCardToMinion mid $ player c
    ReserveAllyHeldCard aid c -> ReserveAllyHeldCard aid $ player c
    MinionMessage mid (MinionHoldsCard c) -> MinionMessage mid $ MinionHoldsCard $ player c
    Spent acid c -> Spent acid $ player c
    SpentWithResources acid c generated printed -> SpentWithResources acid (player c) generated printed
    DiscardedCard c -> DiscardedCard $ card c
    PlaceInCollection c -> PlaceInCollection $ card c
    SetAside cs -> SetAside $ map card cs
    FocusCards cs -> FocusCards $ map card cs
    ShowCardsTo who cs -> ShowCardsTo who $ map card cs
    SearchFoundCards target cs -> SearchFoundCards target $ map card cs
    WithDiscarded target zone cs -> WithDiscarded target zone $ map card cs
    WithChosen target zone cs -> WithChosen target zone $ map card cs
    WithDiscardedMatch target zone c -> WithDiscardedMatch target zone $ card c
    SwapWeatherSupport who old c special -> SwapWeatherSupport who old (player c) special
    CreatedEffect definition source matcher -> CreatedEffect (def definition) source matcher
    SearchForAndRevealScheme definition -> SearchForAndRevealScheme $ def definition
    ResolveAllyLeavingPlay who aid definition next -> ResolveAllyLeavingPlay who aid (def definition) (go next)
    SetActiveCost cost -> SetActiveCost $ snapshot cost
    other -> other

inSimultaneousAttackContext :: Message -> Message -> Message
inSimultaneousAttackContext _ question@(Ask _ _) = question
inSimultaneousAttackContext (WithSimultaneousAttackContext cursor _) message = case message of
  WithSimultaneousAttackContext {} -> message
  _ -> WithSimultaneousAttackContext cursor message
inSimultaneousAttackContext _ message = message

messageAttackContext :: Message -> Game -> Game
messageAttackContext (WithSimultaneousAttackContext cursor _) = selectSimultaneousMinionAttack cursor
messageAttackContext _ = id

isDefendingOtherSimultaneousAttack :: IdentityId -> Game -> Bool
isDefendingOtherSimultaneousAttack who g = case gameSimultaneousMinionAttack g of
  Nothing -> False
  Just group -> any (\(index, (_, attack', _, _, _)) -> index /= simultaneousCurrent group
    && attackDefended attack' && attackCharacter attack' == IdentityCharacter who) (zip [0 ..] $ simultaneousAttacks group)

-- One attack can deal damage to both its ally defender and an identity through
-- overkill. Keep every such damage instance in the group's damage step.
splitSimultaneousAttack :: [Message] -> ([Message], [Message], [Message])
splitSimultaneousAttack messages = (filter isWindow damageSteps, filter (not . isWindow) damageSteps, after)
 where
  (damageSteps, after) = span (not . followsDamage) messages
  isWindow = \case CheckWindows {} -> True; _ -> False
  followsDamage = \case
    IdentityMessage _ (IdentityRetaliate _) -> True
    IdentityMessage _ (AttackDefended _ _ _) -> True
    AllyMessage _ (AllyRetaliates _ _) -> True
    CheckWindows windows -> any (\case W.Window W.After (W.AllyDefendedAgainst _ _) -> True; _ -> False) windows
    _ -> False

-- Several simultaneous hits can reduce one identity to zero. Its single defeat
-- is interrupted once, after every hit has landed.
coalesceSimultaneousDefeats :: [Message] -> [Message]
coalesceSimultaneousDefeats = go [] [] []
 where
  go _ _ _ [] = []
  go windows defeats callbacks (message : rest) =
    let raw = queuedMessage message
        keep replacement ws ds cs = inSimultaneousAttackContext message replacement : go ws ds cs rest
     in case raw of
      CheckWindows opened ->
        let identities = [you | W.Window W.Would (W.IdentityDefeat you) <- opened]
            duplicates = filter (`elem` windows) identities
            remaining = filter (\window -> case W.windowType window of
              W.IdentityDefeat you -> you `notElem` duplicates
              W.IdentityDefeatFromDamage you _ _ -> you `notElem` duplicates
              _ -> True) opened
         in if null remaining then go windows defeats callbacks rest
            else keep (CheckWindows remaining) (nub $ windows <> identities) defeats callbacks
      IdentityMessage you IdentityDefeated | you `elem` defeats -> go windows defeats callbacks rest
      IdentityMessage you IdentityDefeated -> keep raw windows (you : defeats) callbacks
      CardSpecific GameRef "enemyDefeatedIdentity" (fromJSON -> Success (you :: IdentityId, _ :: EnemyId))
        | you `elem` callbacks -> go windows defeats callbacks rest
        | otherwise -> keep raw windows defeats (you : callbacks)
      _ -> message : go windows defeats callbacks rest

restoreSimultaneousMinionAttack :: Maybe SimultaneousMinionAttackGroup -> Game -> Game
restoreSimultaneousMinionAttack previous g = case previous of
  Just group | Just (_, attack', boosts, bonus, _) <- headMay $ drop (simultaneousCurrent group) (simultaneousAttacks group) ->
    (g & entitiesL . minionsL . ix (simultaneousMinion group) %~ overMinion (\attrs -> attrs
      {Minion.minionAttacking = Just attack', Minion.minionBoostCards = boosts, Minion.minionBoost = bonus}))
      {gameSimultaneousMinionAttack = previous}
  _ -> g {gameSimultaneousMinionAttack = previous}

saveSimultaneousMinionAttack :: Game -> Game
saveSimultaneousMinionAttack g = case gameSimultaneousMinionAttack g of
  Just group | Just minion <- lookup (simultaneousMinion group) (gameMinions g), Just current <- Minion.minionAttacking (toAttrs minion), attackSimultaneousGroupId current == Just (simultaneousGroupId group) ->
    let attrs = toAttrs minion
        save n entry@(you, _, _, _, defense) = if n == simultaneousCurrent group then (you, current, Minion.minionBoostCards attrs, Minion.minionBoost attrs, defense) else entry
     in g {gameSimultaneousMinionAttack = Just group {simultaneousAttacks = zipWith save [0 ..] (simultaneousAttacks group)}}
  _ -> g

selectSimultaneousMinionAttack :: Int -> Game -> Game
selectSimultaneousMinionAttack cursor original = case gameSimultaneousMinionAttack saved of
  Just group | Just (_, attack', boosts, bonus, _) <- headMay $ drop cursor (simultaneousAttacks group) ->
    (saved & entitiesL . minionsL . ix (simultaneousMinion group) %~ overMinion (\attrs -> attrs
      {Minion.minionAttacking = Just attack', Minion.minionBoostCards = boosts, Minion.minionBoost = bonus}))
      {gameSimultaneousMinionAttack = Just group {simultaneousCurrent = cursor}}
  _ -> saved
 where
  saved = saveSimultaneousMinionAttack original

characterDamageMessage :: Message -> Maybe (CharacterId,Source,Damage)
characterDamageMessage message = case message of
  IdentityMessage you (IdentityDamaged source damage) -> Just (IdentityCharacter you,source,damage)
  AllyMessage aid (AllyDamaged source damage) -> Just (AllyCharacter aid,source,damage)
  _ -> (\(enemy,source,damage) -> (toCharacter enemy,source,damage)) <$> enemyDamageMessage message

attackDamagePreventedFrom :: HasGame m => Source -> Damage -> m Bool
attackDamagePreventedFrom source damage
  | damageUnpreventable damage = pure False
  | damageSource damage `elem` [FromAttack,FromOverkill] || (case damageSource damage of FromPlayerAttack _ -> True; FromAllyAttack _ -> True; _ -> False) =
      elem PreventAllAttackDamage <$> getModifiers source
  | otherwise = pure False

rememberCompetitiveEffects :: Game -> Game -> Game
rememberCompetitiveEffects before after = case gameCompetition after of
  Nothing -> after
  Just competition ->
    let you = fromMaybe (gameActivePlayer before) (gameYou before)
        aid = maybe defaultAreaId areaId $ areaOfTarget before (IdentityRef you)
        added = [(EffectRef eid,aid) | eid <- HashMap.keys (gameEffects after), not (HashMap.member eid $ gameEffects before)]
        present = HashMap.keys (gameEffects after)
        kept = [(target,area) | (target@(EffectRef eid),area) <- CW.competitiveEffectAreas competition, eid `elem` present]
     in after {gameCompetition = Just competition {CW.competitiveEffectAreas = added <> kept}}

-- | A character gaining stalwart immediately discards its stun and confusion.
stalwartSweep :: (HasGame m, HasQueue m) => Game -> m Game
stalwartSweep g = do
  let villains = [(VillainRef (toId v), Villain.getModifiedKeywords a, Villain.villainStunned a, Villain.villainConfused a) | v <- HashMap.elems (gameVillains g), let a = toAttrs v, Villain.villainStunned a || Villain.villainConfused a]
      minions = [(MinionRef (toId m), Minion.getModifiedKeywords a, minionStunned a, minionConfused a) | m <- HashMap.elems (gameMinions g), let a = toAttrs m, minionStunned a || minionConfused a]
      identities = [(IdentityRef (toId p), getModifiedKeywords p, playerIdentityStunned a, playerIdentityConfused a) | p <- HashMap.elems (gamePlayers g), let a = toAttrs p, playerIdentityStunned a || playerIdentityConfused a]
      allies = [(AllyRef (toId ally), do { ms <- getModifiers a; pure $ toList (cdKeywords $ Ally.allyCardDef a) <> [k | KeywordModifier k <- ms] }, Ally.allyStunned a, Ally.allyConfused a) | ally <- HashMap.elems (gameAllies g), let a = toAttrs ally, Ally.allyStunned a || Ally.allyConfused a]
  -- Every character is checked against this same snapshot. Share its lazy trait
  -- calculation instead of rebuilding the whole trait fixpoint for each character.
  let snapshot = refreshTraits g
  for_ (villains <> minions <> identities <> allies) \(ref, keywords, stunned, confused) -> do
    stalwart <- elem Stalwart <$> runReaderT keywords snapshot
    when stalwart do
      when stunned do
        push (RemoveStatus ref StunnedStatus)
        case ref of
          VillainRef vid | maybe False (Villain.villainAdditionalStun . toAttrs) (HashMap.lookup vid $ gameVillains g) -> push (RemoveStatus ref StunnedStatus)
          IdentityRef iid | maybe False (playerIdentityAdditionalStun . toAttrs) (HashMap.lookup iid $ gamePlayers g) -> push (RemoveStatus ref StunnedStatus)
          _ -> pure ()
      when confused do
        push (RemoveStatus ref ConfusedStatus)
        case ref of
          VillainRef vid | maybe False (Villain.villainAdditionalConfuse . toAttrs) (HashMap.lookup vid $ gameVillains g) -> push (RemoveStatus ref ConfusedStatus)
          IdentityRef iid | maybe False (playerIdentityAdditionalConfuse . toAttrs) (HashMap.lookup iid $ gamePlayers g) -> push (RemoveStatus ref ConfusedStatus)
          _ -> pure ()
  pure g

-- | Whether the enemy, by its ref, is stalwart: it cannot be stunned or confused.
isStalwartEnemy :: (HasGame m) => Target -> m Bool
isStalwartEnemy target = do
  g <- getGame
  keywords <- case target of
    VillainRef v -> maybe (pure []) (Villain.getModifiedKeywords . toAttrs) (lookup v (gameVillains g))
    MinionRef m -> maybe (pure []) (Minion.getModifiedKeywords . toAttrs) (lookup m (gameMinions g))
    EnemyRef (EnemyVillainId v) -> maybe (pure []) (Villain.getModifiedKeywords . toAttrs) (lookup v (gameVillains g))
    EnemyRef (EnemyMinionId m) -> maybe (pure []) (Minion.getModifiedKeywords . toAttrs) (lookup m (gameMinions g))
    _ -> pure []
  pure $ Stalwart `elem` keywords

-- | What a card is played on, chosen before its cost is paid: the character an upgrade
-- attaches to, the ally Make the Call puts into play. Nothing for a card played on
-- nothing.
getPlayTargets ::
  (HasGame m, HasQueue m, MonadThrow m, Projection m PlayerIdentity) =>
  IdentityId ->
  PlayerCard ->
  m (Maybe [Target])
getPlayTargets ident card = case cdCardType (getCardDef card) of
  -- "cannot have cards attached" (Odin): no upgrade is played on it
  UpgradeType -> case createUpgrade ident card of
    Upgrade u -> withMissionTargets $ upgradeTargets u >>= traverse (filterM attachable)
  EventType -> case createEvent ident card of Event e -> eventTargets e
  -- "Play under any player's control. Max N per player.": whose, among those who may
  SupportType | Just (MaxPerPlayerUnderAnyControl n) <- cdLimit (getCardDef card) -> do
    players <- getPlayers
    open <- flip filterM players \p ->
      (< n) <$> selectListCount (SupportMatches [SupportNamed (cdName $ getCardDef card), SupportControlledBy $ IdentityWithId p])
    pure $ Just $ map toTarget open
  SupportType | Just (MaxTraitPerPlayerUnderAnyControl t n) <- cdLimit (getCardDef card) -> do
    open <- filterM (fmap (< n) . controlledWithTrait t) =<< getPlayers
    pure $ Just $ map toTarget open
  _ -> pure Nothing

attachable :: (HasGame m) => Target -> m Bool
attachable t = (\mods -> CannotHaveAttachedCards `notElem` mods && CannotHaveAttachedPlayerCards `notElem` mods && CannotHaveAttachedUpgrades `notElem` mods) <$> getModifiers t

isPermanent :: (HasCardDef a) => a -> Bool
isPermanent = elem Permanent . cdKeywords . getCardDef

-- | A permanent card is not a valid target for an effect that would make it leave play:
-- the effect targets another card that fits it instead (RR, Permanent). So whatever
-- asks, a choice on a permanent card in play that would discard it, remove it from play
-- or return it to hand is not offered.
withoutPermanentTargets :: Game -> Question -> Question
withoutPermanentTargets g = filterChoices (offerable g)

-- | The question with only the choices that pass.
filterChoices :: (Choice -> Bool) -> Question -> Question
filterChoices f = \case
  QuestionLabel l t q -> QuestionLabel l t (filterChoices f q)
  Searched cs q -> Searched cs (filterChoices f q)
  ChooseOne cs -> ChooseOne (filter f cs)
  ChooseN n cs -> ChooseN n (filter f cs)
  ChooseOneAtATime cs -> ChooseOneAtATime (filter f cs)
  q -> q

-- | Earlier activations may defeat a minion still listed in an order prompt.
-- Recheck the remaining choices when the question is actually asked.
withoutDepartedActivationTargets :: Game -> Question -> Question
withoutDepartedActivationTargets g = go
 where
  go (QuestionLabel label target question) = QuestionLabel label target $
    if label == "$label.chooseActivationOrder" then filterChoices present question else go question
  go (Searched cards question) = Searched cards (go question)
  go (Read text question) = Read text (go question)
  go question = question
  present (TargetLabel (MinionRef mid) _) = HashMap.member mid (gameMinions g)
  present (TargetLabel (EnemyRef (EnemyMinionId mid)) _) = HashMap.member mid (gameMinions g)
  present (CardLabel _ choice) = present choice
  present _ = True

-- | A player engaged with a minion with patrol cannot thwart the main scheme (RR Patrol):
-- a choice that would thwart it is not theirs to make. Removing threat from it otherwise
-- than by a thwart is not kept from them.
withoutPatrolledThwarts :: (HasGame m) => Game -> Bool -> Question -> m Question
withoutPatrolledThwarts _g patrolled
  | patrolled = filterChoicesM (fmap not . thwartsMain)
  | otherwise = pure
 where
  thwartsMain = \case
    Run msgs -> anyM thwartMessage msgs
    Label _ cs -> anyM thwartsMain cs
    TargetLabel _ cs -> anyM thwartsMain cs
    CardLabel _ c -> thwartsMain c
    ThwartScheme t source _ | isMain t -> do
      labeled <- elem Ability.Thwart <$> sourceAbilitySubTypes source
      ignores <- elem IgnoresPatrol <$> getModifiers source
      pure $ labeled && not ignores
    BasicThwartScheme t source _ -> do
      modifiers <- getModifiers source
      pure $ isMain t && BasicThwartIgnoresPatrol `notElem` modifiers && IgnoresPatrol `notElem` modifiers
    _ -> pure False
  thwartMessage = \case
    MainSchemeMessage mid (MainSchemeBasicThwarted source _) -> do
      modifiers <- getModifiers source
      restricted <- isRestrictedMainScheme mid
      pure $ restricted && BasicThwartIgnoresPatrol `notElem` modifiers && IgnoresPatrol `notElem` modifiers
    MainSchemeMessage mid (MainSchemeThwarted source _) -> do
      restricted <- isRestrictedMainScheme mid
      ignores <- elem IgnoresPatrol <$> getModifiers source
      pure $ restricted && not ignores
    _ -> pure False
  isMain = \case
    MainSchemeRef _ -> True
    SchemeRef (SchemeMainSchemeId _) -> True
    _ -> False

filterChoicesM :: (Monad m) => (Choice -> m Bool) -> Question -> m Question
filterChoicesM f = \case
  QuestionLabel l t q -> QuestionLabel l t <$> filterChoicesM f q
  Searched cs q -> Searched cs <$> filterChoicesM f q
  ChooseOne cs -> ChooseOne <$> filterM f cs
  ChooseN n cs -> ChooseN n <$> filterM f cs
  ChooseOneAtATime cs -> ChooseOneAtATime <$> filterM f cs
  q -> pure q

-- | The choice is not one that would make a permanent card leave play.
offerable :: Game -> Choice -> Bool
offerable g = keep
 where
  keep = \case
    TargetLabel t cs | permanent t -> not (any (removes t) (concatMap runs cs))
    CardLabel _ c -> keep c
    _ -> True
  runs = \case
    Run msgs -> msgs
    Label _ cs -> concatMap runs cs
    TargetLabel _ cs -> concatMap runs cs
    CardLabel _ c -> runs c
    _ -> []
  removes t = \case
    RemoveFromPlay t' -> t' == t
    RemoveFromGame t' -> t' == t
    ReturnToHand t' -> t' == t
    UpgradeMessage uid DiscardUpgrade -> t == UpgradeRef uid
    _ -> False
  permanent = \case
    UpgradeRef uid -> maybe False isPermanent (lookup uid (gameUpgrades g))
    SupportRef sid -> maybe False (elem Permanent . cdKeywords . supportCardDef . toAttrs) (lookup sid (gameSupports g))
    AllyRef aid -> maybe False isPermanent (lookup aid (gameAllies g))
    _ -> False

-- | The campaign the game is a scenario of, if it is one.
getCampaign :: (HasGame m) => m (Maybe Campaign)
getCampaign = getsGame gameCampaign

-- | The player's identity as a campaign log knows it: its hero face's code.
getIdentityCode :: (HasGame m) => IdentityId -> m CardCode
getIdentityCode ident = getsGame \g -> maybe (error "no such player") (toHeroCardCode . toCardCode) $ lookup ident (gamePlayers g)

-- | The identity's hit points: left, the most it has, and printed.
getRemainingHitPoints :: (HasGame m) => IdentityId -> m (Natural, Natural, Natural)
getRemainingHitPoints ident = do
  p <- getIdentity ident
  hp <- getModifiedHp p
  pure (subtractNatural (identityDamage p) hp, hp, unHp (playerIdentityHP $ toAttrs p))

-- | The counters on the main schemes in play.
getCountersOnMainSchemes :: (HasGame m) => m Natural
getCountersOnMainSchemes = getsGame \g -> sum [mainSchemeCounters (toAttrs m) | m <- HashMap.elems (gameMainSchemes g)]

-- | The side schemes in play, by code, each with the player cards beneath it.
getSideSchemesInPlay :: (HasGame m) => m [(CardCode, [PlayerCard])]
getSideSchemesInPlay = getsGame $ map (\s -> (cdCardCode (getCardDef s), sideSchemeHeldCards (toAttrs s))) . HashMap.elems . gameSideSchemes

-- | The cards the player owns in their hand, deck and discard pile, and the allies they
-- own in play.
-- | Characters that can actually take indirect damage to pay a cost. Tough and
-- prevention cannot pay damage costs (RR Cost); damage cannot exceed remaining HP.
indirectCostCandidates :: (HasGame m) => IdentityId -> Source -> m [(CharacterId, Natural)]
indirectCostCandidates ident source = do
  player <- getsGame (lookup ident . gamePlayers)
  identity <- for (maybe [] pure player) \p -> (\hp -> (IdentityCharacter ident, subtractNatural (identityDamage p) hp)) <$> getModifiedHp p
  allies <- getsGame $ filter ((== ident) . getAllyController) . HashMap.elems . gameAllies
  friendly <- for allies \a -> (\hp -> (AllyCharacter $ toId a, subtractNatural (getAllyDamage a) hp)) <$> Ally.getModifiedHitPoints (toAttrs a)
  playerSource <- isPlayerCardSource source
  filterM (\(c, room) -> do
    modifiers <- getModifiers (characterRef c)
    tough <- characterMatches (CharacterWithStatus ToughStatus) c
    pure $ room > 0 && not tough && CannotTakeDamage `notElem` modifiers
      && not (playerSource && CannotTakeDamageFromPlayerCards `elem` modifiers)
      && null [n | DamageReduction n <- modifiers, n > 0]) (identity <> friendly)

characterStoredDamage :: (HasGame m) => CharacterId -> m Natural
characterStoredDamage character = getsGame $ \g -> case character of
  AttachmentCharacter aid -> maybe 0 (Attachment.attachmentDamage . toAttrs) (lookup aid $ gameAttachments g)
  IdentityCharacter ident -> maybe 0 identityDamage (lookup ident $ gamePlayers g)
  AllyCharacter aid -> maybe 0 getAllyDamage (lookup aid (gameAllies g) <|> lookup aid (entitiesAllies $ gameRemovedEntities g))
  MinionCharacter mid -> maybe 0 getMinionDamage (lookup mid (gameMinions g) <|> lookup mid (entitiesMinions $ gameRemovedEntities g))
  VillainCharacter vid -> maybe 0 villainDamage (lookup vid (gameVillains g) <|> lookup vid (entitiesVillains $ gameRemovedEntities g))

-- | Find includes every player zone and cards held under another game element.
findTouchedCard :: IdentityId -> Game -> Maybe PlayerCard
findTouchedCard you = findOwnedPlayerCardIn you "38002"

findOwnedPlayerCard :: HasGame m => IdentityId -> CardCode -> m (Maybe PlayerCard)
findOwnedPlayerCard you code = getsGame (findOwnedPlayerCardIn you code)

findOwnedPlayerCardIn :: IdentityId -> CardCode -> Game -> Maybe PlayerCard
findOwnedPlayerCardIn you code g = headMay [c | c <- cards, toCardCode c == code, pcOwner c == Just you]
 where
  cards = [c | card <- map toCard (HashMap.elems $ gameUpgrades g) <> map (toCard . toAttrs) (HashMap.elems $ gameSupports g) <> map toCard (HashMap.elems $ gameAllies g), PlayerCard c <- [card]]
    <> concat [unHand a.playerIdentityHand <> unDeck a.playerIdentityDeck <> unDiscard a.playerIdentityDiscard
       | p <- HashMap.elems (gamePlayers g), let a = toAttrs p]
    <> [c | PlayerCard c <- scenarioSetAsideCards (toAttrs $ g.gameScenario) <> gameCollection g]
    <> concatMap (Ally.allyHeldCards . toAttrs) (HashMap.elems $ gameAllies g)
    <> concatMap (Minion.minionHeldCards . toAttrs) (HashMap.elems $ gameMinions g)
    <> concatMap (supportHeldCards . toAttrs) (HashMap.elems $ gameSupports g)
    <> concatMap (upgradeHeldCards . toAttrs) (HashMap.elems $ gameUpgrades g)
    <> concatMap (Obligation.obligationHeldCards . toAttrs) (HashMap.elems $ gameObligations g)
    <> concatMap (mainSchemeHeldCards . toAttrs) (HashMap.elems $ gameMainSchemes g)
    <> concatMap (sideSchemeHeldCards . toAttrs) (HashMap.elems $ gameSideSchemes g)
    <> concatMap (MGEnvironment.environmentHeldCards . toAttrs) (HashMap.elems $ gameEnvironments g)

canFindTouched :: HasGame m => IdentityId -> m Bool
canFindTouched you = getsGame (isJust . findTouchedCard you)

touchedHost :: (HasGame m) => IdentityId -> m (Maybe CharacterId)
touchedHost you = getsGame $ \g -> headMay $ catMaybes
  [ resolve g =<< ((IdentityCharacter <$> Upgrade.upgradeAttachedIdentity a)
      <|> (AllyCharacter <$> upgradeAttachedAlly a)
      <|> ((\case EnemyMinionId mid -> MinionCharacter mid; EnemyVillainId vid -> VillainCharacter vid; EnemyAttachmentId aid -> AttachmentCharacter aid) <$> upgradeAttachedEnemy a))
  | u <- HashMap.elems (gameUpgrades g), let a = toAttrs u, toCardCode a == "38002", upgradeOwner a == you]
 where
  resolve g = \case
    IdentityCharacter p -> IdentityCharacter p <$ lookup p (gamePlayers g)
    VillainCharacter v -> VillainCharacter v <$ lookup v (gameVillains g)
    AttachmentCharacter _ -> Nothing
    AllyCharacter a -> (AllyCharacter a <$ lookup a (gameAllies g))
      <|> (MinionCharacter (MinionId $ unAllyId a) <$ lookup (MinionId $ unAllyId a) (gameMinions g))
    MinionCharacter m -> (MinionCharacter m <$ lookup m (gameMinions g))
      <|> (AllyCharacter (AllyId $ unMinionId m) <$ lookup (AllyId $ unMinionId m) (gameAllies g))

getCharacterTraits :: (HasGame m) => CharacterId -> m (HashSet Trait)
getCharacterTraits character = do
  g <- getsGame id
  case character of
    AttachmentCharacter _ -> pure mempty
    IdentityCharacter you -> maybe (pure mempty) getTraits (lookup you $ gamePlayers g)
    AllyCharacter aid -> case lookup aid (gameAllies g) of
      Just ally -> getTraits ally
      Nothing -> maybe (pure mempty) getTraits (lookup (MinionId $ unAllyId aid) $ gameMinions g)
    MinionCharacter mid -> case lookup mid (gameMinions g) of
      Just minion -> getTraits minion
      Nothing -> maybe (pure mempty) getTraits (lookup (AllyId $ unMinionId mid) $ gameAllies g)
    VillainCharacter vid -> maybe (pure mempty) getTraits (lookup vid $ gameVillains g)

getCardsOwnedBy :: (HasGame m) => IdentityId -> m [CardDef]
getCardsOwnedBy ident = getsGame \g ->
  let playersCards = concat
        [ unHand a.playerIdentityHand <> unDeck a.playerIdentityDeck <> unDiscard a.playerIdentityDiscard
        | p <- HashMap.elems (gamePlayers g), let a = toAttrs p ]
      held = concat
        [ concatMap (Ally.allyHeldCards . toAttrs) (HashMap.elems $ gameAllies g)
        , concatMap (supportHeldCards . toAttrs) (HashMap.elems $ gameSupports g)
        , concatMap (upgradeHeldCards . toAttrs) (HashMap.elems $ gameUpgrades g)
        , concatMap (Obligation.obligationHeldCards . toAttrs) (HashMap.elems $ gameObligations g)
        , concatMap (mainSchemeHeldCards . toAttrs) (HashMap.elems $ gameMainSchemes g)
        , concatMap (sideSchemeHeldCards . toAttrs) (HashMap.elems $ gameSideSchemes g)
        ]
   in [getCardDef c | c <- playersCards <> held, pcOwner c == Just ident]
      <> [getCardDef a | a <- HashMap.elems (gameAllies g), allyOwner (toAttrs a) == ident]
      <> [supportCardDef (toAttrs s) | s <- HashMap.elems (gameSupports g), supportOwner (toAttrs s) == ident]
      <> [getCardDef u | u <- HashMap.elems (gameUpgrades g), upgradeOwner (toAttrs u) == ident]

-- | A game with a seat for each player, in the order given.
newTable :: Scenario -> NonEmpty PlayerIdentity -> Game
newTable scenario (player :| others) = competitiveTable $ foldl' seat (newGame (normalizePlayer player) normalizedScenario) (map normalizePlayer others)
 where
  players = if isJust (getCompetitionSetup scenario) then (1 + length others) `div` 2 else 1 + length others
  competitiveTable table = case getCompetitionSetup scenario of
    Nothing -> table
    Just setup -> case CW.newCompetition setup (gameSeats table) of
      Nothing -> error "Competitive games require two or four players"
      Just c -> table {gameCompetition = Just c, gameAreas = [newArea (CW.teamArea team) (CW.teamPlayers c team) | team <- [CW.Registration,CW.Resistance]]}
  physical c = c {pcCardDef = resolvePlayerCountCost players $ pcCardDef c}
  normalizePlayer (PlayerIdentity attrs) = PlayerIdentity attrs
    { playerIdentityDeck = Deck $ map physical $ unDeck $ playerIdentityDeck attrs
    , playerIdentityHand = Hand $ map physical $ unHand $ playerIdentityHand attrs
    , playerIdentityDiscard = Discard $ map physical $ unDiscard $ playerIdentityDiscard attrs
    , playerIdentityWeatherDeck = fmap (map physical) $ playerIdentityWeatherDeck attrs
    }
  scenarioValue = toJSON scenario
  resolvedScenarioValue = resolveCardCostValues players scenarioValue
  normalizedScenario = if resolvedScenarioValue == scenarioValue then scenario
    else case fromJSON resolvedScenarioValue of
      Success result -> result
      Error err -> error $ "invalid normalized scenario: " <> err
  seat g p =
    g
      & (entitiesL . playersL %~ insertMap (toId p) p)
      & (playerOrderL <>~ [toId p])
      & (seatsL <>~ [toId p])
      & joinFirstArea (toId p)

-- | A player sitting down plays in the first area.
joinFirstArea :: IdentityId -> Game -> Game
joinFirstArea ident = areasL %~ \case
  a : as -> a {areaPlayers = areaPlayers a <> [ident]} : as
  [] -> [newArea defaultAreaId [ident]]

withGame :: (MonadGame env m) => (Game -> (Game, a)) -> m a
withGame f = do
  gameRef <- getGameRef
  liftIO $ atomicModifyIORef' gameRef (first refreshTraits . f)

withGame_ :: (MonadGame env m) => (Game -> Game) -> m ()
withGame_ f = withGame ((,()) . f)

withGameM :: (MonadGame env m) => (Game -> m Game) -> m ()
withGameM f = getGame >>= f >>= withGame_ . const

getsGame :: (HasGame m) => (Game -> a) -> m a
getsGame f = f <$> getGame

-- | The encounter deck, its top card first.
-- | The boost icons on the encounter card: its own, and those The Next Evolution adds.
getBoostIcons :: (HasGame m) => EncounterCard -> m Natural
getBoostIcons card = do
  extra <- sum . map (\case AdditionalBoostIcons n -> n; _ -> 0) <$> getModifiers GameRef
  pure $ boostCount (cdBoostIcons $ getCardDef card) + extra

getEncounterDeck :: (HasGame m) => m [EncounterCard]
getEncounterDeck = fmap pileDeck . getPile =<< getDeckKeyFor =<< getYou

getEncounterDiscard :: (HasGame m) => m [EncounterCard]
getEncounterDiscard = fmap pileDiscard . getPile =<< getDeckKeyFor =<< getYou

-- | The cards played this round, and by whom.
getPlayedThisWindow :: (HasGame m) => m [CardDef]
getPlayedThisWindow = getsGame (fromMaybe [] . headMay . gamePlayedInWindows)

getPlayedThisRound :: (HasGame m) => m [(IdentityId, CardDef)]
getPlayedThisRound = getsGame gamePlayedThisRound

getPlayedThisPhase :: (HasGame m) => m [(IdentityId, CardDef)]
getPlayedThisPhase = getsGame gamePlayedThisPhase

getIdentityAttackedAndThwartedThisPhase :: HasGame m => IdentityId -> m Bool
getIdentityAttackedAndThwartedThisPhase you = getsGame $ \g ->
  you `elem` gameHeroAttackedThisPhase g && you `elem` gameHeroThwartedThisPhase g

getPlayedThisTurn :: (HasGame m) => m [(IdentityId, CardDef)]
getPlayedThisTurn = getsGame gamePlayedThisTurn

-- | Enemy targets attacked by this identity (including its events), or this ally,
-- since the current player turn began. Damage need not have been dealt.
getAttacksThisTurn :: (HasGame m) => m [(Source, EnemyId)]
getAttacksThisTurn = getsGame gameAttacksThisTurn

getAttackTargetsThisTurn :: (HasGame m) => Source -> m [EnemyId]
getAttackTargetsThisTurn source = getsGame $ map snd . filter ((== source) . fst) . gameAttacksThisTurn

treacheryHasResolvedPart :: (HasGame m) => TreacheryId -> m Bool
treacheryHasResolvedPart tid = getsGame $ maybe False (Treachery.treacheryResolvedPart . toAttrs) . HashMap.lookup tid . gameTreacheries

getTreacheriesRevealedThisPhase :: (HasGame m) => m [(IdentityId, TreacheryId)]
getTreacheriesRevealedThisPhase = getsGame gameTreacheriesRevealedThisPhase

getPhase :: (HasGame m) => m Phase
getPhase = getsGame gamePhase

getUsedAbilities :: (HasGame m) => m (HashMap IdentityId [Ability])
getUsedAbilities = getsGame (HashMap.map (map fst) . view usedAbilitiesL)

-- A discard response is tied to this window occurrence, even across an immediate
-- empty-deck shuffle. A response which moves its card consumes this occurrence.
discardResponseAvailable :: HasGame m => CardId -> m Bool
discardResponseAvailable cid = do
  unused <- getsGame $ \g -> not $ any (\(a, depth) -> depth == gameWindowDepth g && abilitySource a == CardIdRef cid) $ concat $ HashMap.elems $ gameUsedAbilities g
  playerCard <- getsGame $ any (\player -> any ((== cid) . pcCardId)
    (unDeck (playerIdentityDeck $ toAttrs player) <> unDiscard (playerIdentityDiscard $ toAttrs player))) . HashMap.elems . gamePlayers
  encounterCard <- any ((== cid) . ecCardId) <$> getEncounterDiscard
  pure $ unused && (playerCard || encounterCard)

-- | Whether a player may use an ability again under its limit. An ability on an
-- encounter card is anyone's to trigger, so its limit counts every player's uses (a
-- forced ability used once is not offered to the next), unless it is a limit per player.
withinUseLimit :: (HasGame m) => IdentityId -> Ability -> m Bool
withinUseLimit ident a = getsGame \g -> do
  let currentOccurrence (_, depth) = depth == gameWindowDepth g
      relevant = case baseLimit (abilityLimit a) of
        PerWindow _ -> filter currentOccurrence
        SharedPerWindow _ _ -> filter currentOccurrence
        _ -> id
      used = HashMap.map (map fst . relevant) (gameUsedAbilities g)
      pooled = isNothing (abilityController g a) && not (perPlayer (abilityLimit a))
      perPlayer = \case
        EachPlayer _ -> True
        _ -> False
  passesUseLimit ident (if pooled then HashMap.singleton ident (concat $ HashMap.elems used) else used) a

getIdentity :: (HasCallStack, HasGame m) => IdentityId -> m PlayerIdentity
getIdentity ident = do
  g <- getGame
  case lookup ident (gamePlayers g) of
    Just a -> pure a
    Nothing -> error "missing identity"

gameAbilities :: Game -> [Ability]
gameAbilities g =
    [a | W.Window _ (W.DiscardedFromPlayerDeck owner cards) <- concat (take 1 $ gameWindows g), a <- Domino.discardResponseAbilities owner cards]
      <> concatMap getAbilities (HashMap.elems $ gamePlayers g)
      <> concatMap (filter (not . isHandAbility) . getAbilities) (HashMap.elems $ gameAllies g)
      <> [ a
         | p <- HashMap.elems (gamePlayers g)
         , c <- unHand (playerIdentityHand $ toAttrs p)
         , a <- handCardAbilities (toId p) c
         ]
      <> concatMap getAbilities (HashMap.elems $ gameSupports g)
      <> concatMap getAbilities (HashMap.elems $ gameUpgrades g)
      <> concatMap getAbilities (HashMap.elems $ gameVillains g)
      <> concatMap getAbilities (HashMap.elems $ gameAttachments g)
      <> concatMap getAbilities (HashMap.elems $ gameMinions g)
      <> [windowAbility (toAttrs minion) 97001 (W.MinionDefeated W.When $ MinionWithId $ toId minion) WhenRevealed NoCost (RunAbility (toRef minion) 97001)
         | any ((== "60142b") . toCardCode . getCardDef) (HashMap.elems $ gameMainSchemes g),minion <- HashMap.elems $ gameMinions g]
      <> concatMap getAbilities (HashMap.elems $ gameEffects g)
      <> concatMap getAbilities (HashMap.elems $ gameEnvironments g)
      <> concatMap getAbilities (HashMap.elems $ gameObligations g)
      <> concatMap getAbilities (HashMap.elems $ gameSideSchemes g)
      <> concatMap getAbilities (HashMap.elems $ gameMainSchemes g)

-- | The def of the player card an ability is on: in play, or an upgrade just discarded
-- to pay for it.
sourceAbilitySubTypes :: (HasGame m) => Source -> m [Ability.AbilitySubType]
sourceAbilitySubTypes source = do
  g <- getGame
  case headMay [labels | (active, labels, _) <- gameResolvingAbilitySubTypes g, active == source] of
    Just labels -> pure labels
    Nothing -> case toTarget source of
      EventRef eid -> case lookup eid (gameEvents g) of
        Just (Event event) -> eventAbilitySubTypes event
        Nothing -> pure []
      target -> pure $ maybe [] cdAbilitySubTypes (abilityCardDef g target)

encounterCardCanBeCanceled :: (HasGame m) => EncounterCard -> m Bool
encounterCardCanBeCanceled card | toCardCode card `elem` ["39017", "39071"] = pure False
encounterCardCanBeCanceled card = do
  you <- getYou
  case cdCardType (getCardDef card) of
    TreacheryType -> allowed (createTreachery card)
    AttachmentType -> allowed (createAttachment card)
    MinionType -> allowed (createMinion you card)
    SideSchemeType -> allowed (createSideScheme card)
    EnvironmentType -> allowed (createEnvironment card)
    ObligationType -> allowed (createObligation card)
    _ -> pure True
 where
  allowed :: (HasGame m, HasModifiersFor a, IsRef a) => a -> m Bool
  allowed prototype = do
    printed <- getModifiersFor (toSource prototype) (toTarget prototype) prototype
    modifiers <- getModifiers prototype
    pure $ CannotBeCanceled `notElem` (printed <> modifiers)

abilityCardDef :: Game -> Target -> Maybe CardDef
abilityCardDef g raw = case friendlyBoostTarget g raw of
  UpgradeRef uid ->
    getCardDef <$> (lookup uid (gameUpgrades g) <|> lookup uid (g ^. removedEntitiesL . upgradesL))
  SupportRef sid -> supportCardDef . toAttrs <$> (lookup sid (gameSupports g) <|> lookup sid (g ^. removedEntitiesL . supportsL))
  AllyRef aid -> getCardDef <$> (lookup aid (gameAllies g) <|> lookup aid (g ^. removedEntitiesL . alliesL))
  VillainRef vid -> getCardDef <$> (lookup vid (gameVillains g) <|> lookup vid (g ^. removedEntitiesL . villainsL))
  MinionRef mid -> getCardDef <$> (lookup mid (gameMinions g) <|> lookup mid (g ^. removedEntitiesL . minionsL))
  EnvironmentRef eid -> getCardDef <$> (lookup eid (gameEnvironments g) <|> lookup eid (g ^. removedEntitiesL . environmentsL))
  SideSchemeRef sid -> getCardDef <$> (lookup sid (gameSideSchemes g) <|> lookup sid (g ^. removedEntitiesL . sideSchemesL))
  MainSchemeRef sid -> getCardDef <$> (lookup sid (gameMainSchemes g) <|> lookup sid (g ^. removedEntitiesL . mainSchemesL))
  AttachmentRef aid -> (getCardDef <$> (lookup aid (gameAttachments g) <|> lookup aid (g ^. removedEntitiesL . attachmentsL)))
    <|> (getCardDef <$> attachmentUpgrade g aid)
  TreacheryRef tid -> getCardDef <$> (lookup tid (gameTreacheries g) <|> lookup tid (g ^. removedEntitiesL . treacheriesL))
  ObligationRef oid -> getCardDef <$> (lookup oid (gameObligations g) <|> lookup oid (g ^. removedEntitiesL . obligationsL))
  EventRef eid -> eventCardDef . toAttrs <$> lookup eid (gameEvents g)
  -- the identity's face up as it resolved its printed ability
  IdentityRef ident -> getCardDef <$> lookup ident (gamePlayers g)
  -- a card's ability used from its owner's hand
  CardIdRef cid -> (getCardDef . snd <$> handCard g cid) <|> (getCardDef . snd <$> pendingPaymentCard g cid) <|> (getCardDef . snd <$> discardedWindowCard g cid) <|> (getCardDef <$> headMay [c | c <- gameFocusedCards g, toCardId c == cid])
  _ -> Nothing

-- | A card in a player's hand, with the player holding it.
discardedWindowCard :: Game -> CardId -> Maybe (IdentityId, PlayerCard)
discardedWindowCard g cid = headMay [(owner, c) | W.Window _ (W.DiscardedFromPlayerDeck owner cards) <- concat (take 1 $ gameWindows g), c <- cards, pcCardId c == cid]

-- Resource cards remain sources while their damage costs resolve.
pendingPaymentCard :: Game -> CardId -> Maybe (IdentityId, PlayerCard)
pendingPaymentCard g cid = headMay
  [ (fromMaybe (activeCostIdentityId cost) (pcController card <|> pcOwner card), card)
  | cost <- gameActiveCost g
  , PendingEnergySiphonPayment card _ _ <- payments (activeCostPayment cost)
  , pcCardId card == cid
  ]

handCard :: Game -> CardId -> Maybe (IdentityId, PlayerCard)
handCard g cid =
  headMay
    [ (toId p, c)
    | p <- HashMap.elems (gamePlayers g)
    , c <- unHand (playerIdentityHand $ toAttrs p)
    , pcCardId c == cid
    ]

-- | The abilities a card has while it is in its owner's hand ("While Pip the Troll is in
-- your hand, he gains ..."): those of the card, built as it would be in play, that are
-- sourced on the card itself ('CardIdRef'). ponytail: allies and obligations, the kinds
-- so far that have one.
handCardAbilities :: IdentityId -> PlayerCard -> [Ability]
handCardAbilities ident c = filter isHandAbility $ case cdCardType (getCardDef c) of
  AllyType -> getAbilities (createAlly ident c)
  EventType -> getAbilities (createEvent ident c)
  -- System Shock: "while this card is in your hand, it gains ..."
  ObligationType -> getAbilities (handObligation c)
  _ -> []

-- | An obligation in a player's hand, built as it would be in play to give its abilities.
handObligation :: PlayerCard -> Obligation
handObligation c = lookupObligation (toCardCode c) (ObligationId $ unCardId $ pcCardId c)

-- | An ability sourced on a card id is one the card has in hand, not in play.
isHandAbility :: Ability -> Bool
isHandAbility a = case abilitySource a of
  CardIdRef _ -> True
  _ -> False

-- | Who may trigger an ability: the player controlling the card it is on. Abilities on
-- encounter cards are anyone's.
abilityController :: Game -> Ability -> Maybe IdentityId
abilityController g a
  -- "for any player": the card's controller resolves it for whoever pays
  | forAnyPlayer (abilityCriteria a) = Nothing
  | otherwise = cardController g a
-- | The player controlling the card an ability is on, if a player does.
cardController :: Game -> Ability -> Maybe IdentityId
cardController g a
  | isMissionTarget g (refocus $ abilitySource a) = Nothing
  | otherwise = case abilitySource a of
  IdentityRef ident -> Just ident
  AllyRef aid | aid `elem` g.gameUncontrolledAllies -> Nothing
  AllyRef aid -> getAllyController <$> lookup aid (gameAllies g)
  SupportRef sid -> getSupportController <$> (lookup sid (gameSupports g) <|> lookup sid (entitiesSupports $ gameRemovedEntities g))
  UpgradeRef uid -> getUpgradeController <$> (lookup uid (gameUpgrades g) <|> lookup uid (entitiesUpgrades $ gameRemovedEntities g))
  -- only the player whose identity or ally an attachment is on, or whose play area an
  -- obligation is in, may trigger it (RR, Attachment and Obligation)
  AttachmentRef aid -> lookup aid (gameAttachments g) >>= \x ->
    let at = toAttrs x
     in Attachment.attachmentIdentity at <|> (getAllyController <$> (flip lookup (gameAllies g) =<< Attachment.attachmentAlly at))
  ObligationRef oid -> Obligation.obligationOwner . toAttrs =<< lookup oid (gameObligations g)
  SideSchemeRef sid -> do
    scheme <- lookup sid (gameSideSchemes g) <|> lookup sid (entitiesSideSchemes $ gameRemovedEntities g)
    card <- SideScheme.sideSchemePlayerCard (toAttrs scheme)
    pcController card <|> pcOwner card
  -- a card in hand: the player holding it
  CardIdRef cid -> fst <$> (handCard g cid <|> pendingPaymentCard g cid)
  _ -> Nothing

-- | A labeled event is one attack even when it deals several instances of damage.
sourceHasBegunLabeledAttack :: (HasGame m) => Source -> m Bool
sourceHasBegunLabeledAttack (EventRef eid) = do
  labels <- sourceAbilitySubTypes (EventRef eid)
  attacked <- getsGame $ maybe False (\e -> eventAttackStarted (toAttrs e) || notNull (eventAttacked $ toAttrs e)) . lookup eid . gameEvents
  pure $ Ability.Attack `elem` labels && attacked
sourceHasBegunLabeledAttack _ = pure False

-- | Whether this enemy has already been chosen as a target of a labeled event.
sourceHasAttackedTarget :: HasGame m => Source -> EnemyId -> m Bool
sourceHasAttackedTarget (EventRef eid) enemy = do
  labels <- sourceAbilitySubTypes (EventRef eid)
  attacked <- getsGame $ maybe [] (eventAttacked . toAttrs) . lookup eid . gameEvents
  pure $ Ability.Attack `elem` labels && any (\(_, target, _) -> target == enemy) attacked
sourceHasAttackedTarget _ _ = pure False

-- | The effect that currently owns a controlled minion conversion.
getAllyControlSource :: HasGame m => AllyId -> m (Maybe Source)
getAllyControlSource aid = getsGame $ maybe Nothing (Ally.allyControlSource . toAttrs) . lookup aid . gameAllies

-- | Ownership persists when an ally is defeated or temporarily changes controller.
getCharacterOwner :: (HasGame m) => CharacterId -> m (Maybe IdentityId)
getCharacterOwner = \case
  IdentityCharacter ident -> pure (Just ident)
  AllyCharacter aid -> do
    ally <- getsGame $ \g -> lookup aid (gameAllies g) <|> lookup aid (entitiesAllies $ gameRemovedEntities g)
    case ally of
      Nothing -> pure Nothing
      Just a -> case toCard a of
        EncounterCard _ -> pure Nothing
        PlayerCard card -> do
          -- Story allies follow the first-player token but belong to the scenario.
          -- Campaign allies that actually enter a player's deck keep their owner.
          printed <- if isJust (cdEncounterSet $ getCardDef card)
            then getModifiersFor GameRef (toRef a) a
            else pure []
          pure $ if ControlledByFirstPlayer `elem` printed then Nothing else pcOwner card
  _ -> pure Nothing

-- | The player controlling the card a source names, if a player does.
getSourceController :: (HasGame m) => Source -> m (Maybe IdentityId)
getSourceController source = getsGame \g ->
  let controllerOf target = if isMissionTarget g target then Nothing else case target of
        CharacterRef character -> controllerOf (characterRef character)
        IdentityRef ident -> Just ident
        CardIdRef cid -> fst <$> (handCard g cid <|> pendingPaymentCard g cid)
        AllyRef aid | aid `elem` gameUncontrolledAllies g -> Nothing
        AllyRef aid -> getAllyController <$> (lookup aid (gameAllies g) <|> lookup aid (entitiesAllies $ gameRemovedEntities g))
        SupportRef sid -> getSupportController <$> (lookup sid (gameSupports g) <|> lookup sid (entitiesSupports $ gameRemovedEntities g))
        UpgradeRef uid -> getUpgradeController <$> (lookup uid (gameUpgrades g) <|> lookup uid (entitiesUpgrades $ gameRemovedEntities g))
        EventRef eid -> eventController . toAttrs <$> lookup eid (gameEvents g)
        ObligationRef oid -> obligationOwner . toAttrs =<< lookup oid (gameObligations g)
        SideSchemeRef sid -> do
          scheme <- lookup sid (gameSideSchemes g) <|> lookup sid (entitiesSideSchemes $ gameRemovedEntities g)
          card <- SideScheme.sideSchemePlayerCard (toAttrs scheme)
          pcController card <|> pcOwner card
        _ -> Nothing
  in controllerOf (refocus source)

-- | The character acting through this source. Supports and encounter-card effects
-- act independently; an upgrade attached to an ally acts through that ally.
threatSourceCharacter :: HasGame m => Source -> m (Maybe CharacterId)
threatSourceCharacter = \case
  CharacterRef character -> pure $ Just character
  EnemyRef enemy -> pure $ Just $ toCharacter enemy
  IdentityRef you -> pure $ Just $ IdentityCharacter you
  AllyRef ally -> pure $ Just $ AllyCharacter ally
  MinionRef minion -> getsGame $ \g -> Just $ maybe (MinionCharacter minion) AllyCharacter (friendlyProxyAlly g minion)
  VillainRef villain -> pure $ Just $ VillainCharacter villain
  source@(EventRef _) -> fmap IdentityCharacter <$> getSourceController source
  source@(UpgradeRef uid) -> do
    ally <- getsGame (join . fmap (upgradeAttachedAlly . toAttrs) . lookup uid . gameUpgrades)
    case ally of
      Just aid -> pure $ Just $ AllyCharacter aid
      Nothing -> fmap IdentityCharacter <$> getHeroSourceController source
  EffectRef eid -> getsGame (fmap (Effect.effectSource . toAttrs) . lookup eid . gameEffects) >>= maybe (pure Nothing) threatSourceCharacter
  _ -> pure Nothing

-- | RR You/Your: allies, supports and encounter cards do not extend the identity.
getHeroSourceController :: (HasGame m) => Source -> m (Maybe IdentityId)
getHeroSourceController source = case source of
  IdentityRef _ -> getSourceController source
  EventRef _ -> getSourceController source
  UpgradeRef uid -> do
    attached <- getsGame (fmap (upgradeAttachedAlly . toAttrs) . lookup uid . gameUpgrades)
    if maybe False isJust attached then pure Nothing else getSourceController source
  EffectRef eid -> getsGame (fmap (Effect.effectSource . toAttrs) . lookup eid . gameEffects) >>= maybe (pure Nothing) getHeroSourceController
  _ -> pure Nothing

-- | Whether the source is a player card: a hero or alter-ego, an ally, event, upgrade or
-- support, or an effect one of them created. Obligations and encounter cards are not.
isPlayerCardSource :: (HasGame m) => Source -> m Bool
isPlayerCardSource = \case
  source@(CardIdRef cid) -> getsGame $ \g ->
    any (\cost -> case activeCostTarget cost of ForCard card -> pcCardId card == cid; _ -> False) (gameActiveCost g)
      || maybe False ((`elem` [AllyType, EventType, ResourceType, SupportType, UpgradeType, PlayerSideSchemeType, HeroType, AlterEgoType]) . cdCardType)
        (abilityCardDef g (refocus source) <|> (getCardDef <$> headMay (mapMaybe (cardInPlay g) [AllyRef $ AllyId $ unCardId cid, SupportRef $ SupportId $ unCardId cid, UpgradeRef $ UpgradeId $ unCardId cid])))
  IdentityRef _ -> pure True
  AllyRef _ -> pure True
  EventRef _ -> pure True
  UpgradeRef _ -> pure True
  SupportRef _ -> pure True
  EffectRef eid -> getsGame (fmap (Effect.effectSource . toAttrs) . lookup eid . gameEffects) >>= maybe (pure False) isPlayerCardSource
  _ -> pure False

-- | Whether the character takes none of the damage from the source: it cannot take
-- damage (The Red House), or not from a player card (Sanctuary).
cannotTakeDamageFrom :: (HasGame m) => Target -> Source -> m Bool
cannotTakeDamageFrom target source = do
  missionRestricted <- getsGame (\g -> isMissionTarget g target && not (missionSourceAllowed g source))
  restricted <- sourceRestrictedFromEnemy source target
  modifiers <- getModifiers target
  attackingCharacter <- sourceAttackCharacter source
  identitySource <- getHeroSourceController source
  if
    | any (\case CannotBeDamagedByIdentity you -> identitySource == Just you; _ -> False) modifiers -> pure True
    | any (\case DamageOnlyFromCharacterAttacks allowed -> attackingCharacter /= Just allowed; _ -> False) modifiers -> pure True
    | missionRestricted || restricted -> pure True
    | CannotTakeDamage `elem` modifiers -> pure True
    | CannotTakeDamageFromPlayerCards `elem` modifiers -> isPlayerCardSource source
    | otherwise -> pure False

-- | The abilities in play a player may trigger.
getAbilitiesFor :: (HasGame m) => IdentityId -> m [Ability]
getAbilitiesFor ident = getAbilitiesForMatching ident (const $ pure True)

-- Preview the complete basic-power rules in an isolated game, including costs.
-- Cell Phone uses the same candidates for initiation and its subsequent choices.
getUsableBasicAttackOrThwartAbilities :: (HasGame m, HasQueue m) => IdentityId -> m [Ability]
getUsableBasicAttackOrThwartAbilities you = do
  snapshot <- getGame
  app <- newApp snapshot []
  runGameApp app $ asYou you do
    abilities <- getAbilitiesForMatching you $ \ab -> pure $ abilityType ab == Basic && abilityIndex ab `elem` [300, 301]
    filterM (\ab -> (&&) <$> passesCriteria you ab <*> passesCanAffordCost you ab) abilities

-- Filter large candidate sets with pure or cheap checks first. Blanking scans
-- modifiers, and affordability may search payment combinations, so callers should
-- reject irrelevant ability types/windows before asking for that expensive work.
getAbilitiesForMatching :: (HasGame m) => IdentityId -> (Ability -> m Bool) -> m [Ability]
getAbilitiesForMatching ident matches = do
  keywordAbilities <- (<>) <$> retaliationAbilities <*> delayedAttackDamageAbilities
  candidates <- getsGame \g -> filter (\a -> not (isMissionTarget g $ refocus $ abilitySource a) && maybe True (== ident) (abilityController g a) && unhelped g a && competitiveAbilityAllowed g ident a
    && (abilitySubType a /= Just Ability.Defense || not (isDefendingOtherSimultaneousAttack ident g))) (getAbilities g <> keywordAbilities)
  permitted <- canResolvePlayerCardAbilities ident
  snapshot <- getGame
  let playerAbility a = not (engineAttackResponse a) && not (isIdentity (abilitySource a) && abilityIndex a == 100) &&
        maybe False ((`elem` [HeroType, AlterEgoType, AllyType, SupportType, UpgradeType, EventType, ResourceType, PlayerSideSchemeType]) . cdCardType) (abilityCardDef snapshot (refocus $ abilitySource a))
  abilities <- filterM matches $ if permitted then candidates else filter (not . playerAbility) candidates
  filterM (\a -> if printed a then do
    blanked <- isBlanked (abilitySource a)
    blocked <- case abilitySource a of
      IdentityRef you -> do
        hero <- isHero <$> getIdentity you
        (hero &&) . elem CannotTriggerPrintedHeroAbilities <$> getModifiers you
      _ -> pure False
    pure $ not blanked && not blocked
    else pure True) abilities
 where
  -- a blank text box leaves the game's own abilities: basic powers and changing form
  printed a = abilityIndex a /= 97001 && not (engineAttackResponse a) && abilityType a /= Basic && not (isIdentity (abilitySource a) && abilityIndex a == 100)
  isIdentity = \case
    IdentityRef _ -> True
    _ -> False
  -- while a player resolves a card with peril, the others trigger nothing but what is
  -- forced, and no card another player controls helps them (RR Peril)
  unhelped g a = case gamePeril g of
    Just p | p /= ident -> isForcedAbility a
    Just _ -> maybe True (== ident) (cardController g a)
    Nothing -> True

competitiveAbilityAllowed :: Game -> IdentityId -> Ability -> Bool
competitiveAbilityAllowed g you ability
  | Nothing <- gameCompetition g = True
  | Just c <- gameCompetition g
  , any phaseWindow (concat $ take 1 $ gameWindows g)
  , CW.playerTeam c you /= Just (CW.phaseTeam $ CW.competitivePhase c)
      || isJust (CW.teamOutcome c $ CW.phaseTeam $ CW.competitivePhase c) = False
  | otherwise = local source || maybe False (local . IdentityRef) (cardController g ability) || crossLeaderResponse
 where
  source = refocus $ abilitySource ability
  crossLeaderResponse = isForcedAbility ability
    && maybe False ((`elem` [VillainType,MinionType,AttachmentType,SideSchemeType,MainSchemeType]) . cdCardType) (abilityCardDef g source)
    && maybe False (\target -> target `elem` crossedLeaders && target `elem` currentLeaders) host
  host = case source of
    AttachmentRef aid -> enemyRef <$> (Attachment.attachmentEnemy . toAttrs =<< lookup aid (gameAttachments g))
    SideSchemeRef _ -> case abilityWindow ability of
      Just (W.VillainWouldTakeDamageFrom (VillainWithId vid) _) -> Just $ VillainRef vid
      _ -> Nothing
    MainSchemeRef _ -> case abilityWindow ability of
      Just (W.VillainStatusGained _ (VillainWithId vid)) -> Just $ VillainRef vid
      _ -> Nothing
    _ -> Just source
  crossedLeaders = concatMap crossed $ concat $ gameWindows g
  crossed (W.Window _ event) = case event of
    W.EnemySchemeBegins enemy _ -> [enemyRef enemy]
    W.Schemed vid -> [VillainRef vid]
    W.VillainSchemeResolved vid _ -> [VillainRef vid]
    W.VillainSchemeResolvedOnMainScheme vid _ -> [VillainRef vid]
    W.EnemyAttackOnEnemy attacker defender | areaOf attacker /= areaOf defender -> [enemyRef attacker,enemyRef defender]
    W.VillainDamageFrom defender from damage | Just attacker <- sourceEnemy from,damageSource damage `elem` [FromAttack,FromOverkill],areaOf attacker /= areaOf (EnemyVillainId defender) -> [enemyRef attacker,VillainRef defender]
    _ -> []
  currentLeaders = concatMap namedLeaders $ concat $ take 1 $ gameWindows g
  namedLeaders (W.Window _ event) = case event of
    W.EnemySchemeBegins enemy _ -> [enemyRef enemy]
    W.Schemed vid -> [VillainRef vid]
    W.VillainSchemeResolved vid _ -> [VillainRef vid]
    W.VillainSchemeResolvedOnMainScheme vid _ -> [VillainRef vid]
    W.EnemyAttackOnEnemy attacker defender -> [enemyRef attacker,enemyRef defender]
    W.VillainDamageFrom vid _ _ -> [VillainRef vid]
    W.DamagedVillain vid _ -> [VillainRef vid]
    W.VillainGainingStatus vid _ -> [VillainRef vid]
    W.VillainGainedStatus vid _ -> [VillainRef vid]
    W.EnemyWouldBeDamaged enemy _ _ -> [enemyRef enemy]
    W.EnemyAttackDefeatedCharacter enemy character -> enemyRef enemy : [VillainRef vid | VillainCharacter vid <- [character]]
    W.EnemyAttacksAndDamages enemy character _ -> enemyRef enemy : [VillainRef vid | VillainCharacter vid <- [character]]
    _ -> []
  areaOf enemy = areaId <$> areaOfTarget g (enemyRef enemy)
  here = areaId <$> areaOfTarget g (IdentityRef you)
  local target = (areaId <$> areaOfTarget g target) == here
  phaseWindow (W.Window _ event) = case event of
    W.VillainThreatStepFinished -> True
    W.PhaseBegan _ -> True
    W.PhaseEnded _ -> True
    _ -> False

-- | Whether the card's printed text box is treated as blank (Tech Theft). Only the cards
-- that can blank another are asked, so a blanked card's own modifiers are not needed.
isBlanked :: (HasGame m, IsRef a) => a -> m Bool
isBlanked a = do
  g <- getGame
  let target = canonicalAttachmentTarget g (toTarget a)
  inElementArea target $ isBlankedInContext target

isBlankedInContext :: (HasGame m, IsRef a) => a -> m Bool
isBlankedInContext a = do
  g <- getGame
  -- a facedown upgrade has no text in play (Spectrum's energy forms)
  let facedown = case toTarget a of
        UpgradeRef uid -> maybe False (upgradeFaceDown . toAttrs) (lookup uid (gameUpgrades g))
        _ -> False
  let protected = case toTarget a of
        SupportRef sid -> maybe False ((== "31001b") . toCardCode . Support.supportCardDef . toAttrs) (lookup sid (gameSupports g))
        UpgradeRef uid -> maybe False ((== "31002b") . toCardCode . getCardDef) (lookup uid (gameUpgrades g))
        _ -> False
  let targetDef = abilityCardDef g (toTarget a)
      permanentSet = targetDef >>= \def -> if isPermanent def then cdEncounterSet def else Nothing
      allowed provider = modifierProviderAllowed g (toTarget a) provider && case permanentSet of
        Nothing -> True
        Just ownSet ->
          let source = case provider of
                EffectRef eid -> maybe provider (refocus . Effect.effectSource . toAttrs) (lookup eid $ gameEffects g)
                _ -> provider
           in (cdEncounterSet =<< abilityCardDef g source) == Just ownSet
  let uncontrolled = case toTarget a of { AllyRef aid -> aid `elem` g.gameUncontrolledAllies; _ -> False }
  let treatedAlly = case toTarget a of
        MinionRef mid -> maybe False (isJust . Minion.minionOriginalAlly . toAttrs) (lookup mid $ gameMinions g)
        _ -> False
  if protected then pure False else (treatedAlly ||) . (missionAlly g (toTarget a) ||) . (attackBlanksTarget g (toTarget a) ||) . (uncontrolled ||) . (facedown ||) . elem Blank . mconcat
    <$> sequence
      [ concatMapM (getBlankingModifiersFor (toSource a) (toTarget a)) (filter (allowed . toTarget) $ HashMap.elems $ gameSideSchemes g)
      -- Effect modifiers are stored explicitly; match only effects that can blank.
      , concatMapM (getBlankingModifiersFor (toSource a) (toTarget a))
          (filter (elem Blank . Effect.effectModifiers . toAttrs) $ filter (allowed . toTarget . toAttrs) $ HashMap.elems $ gameEffects g)
      , concatMapM (getBlankingModifiersFor (toSource a) (toTarget a)) (filter (allowed . toTarget) $ HashMap.elems $ gameAttachments g)
      , concatMapM (getBlankingModifiersFor (toSource a) (toTarget a)) (filter (allowed . toTarget) $ HashMap.elems $ gameUpgrades g)
      , concatMapM (getBlankingModifiersFor (toSource a) (toTarget a)) (filter (allowed . toTarget) $ HashMap.elems $ gameEnvironments g)
      , concatMapM (getBlankingModifiersFor (toSource a) (toTarget a)) (filter (allowed . toTarget . toAttrs) $ HashMap.elems $ gameVillains g)
      , concatMapM (getBlankingModifiersFor (toSource a) (toTarget a)) (filter (allowed . toTarget) $ HashMap.elems $ gameObligations g)
      ]

messageLimit :: Int
messageLimit = 10000

-- | Whether all the question offers is one player.
onlyPlayer :: Question -> Bool
onlyPlayer = \case
  QuestionLabel _ _ q -> onlyPlayer q
  Read _ _ -> False
  Searched _ q -> onlyPlayer q
  ChooseOne [TargetLabel (IdentityRef _) _] -> True
  ChoosePlayerOrder (Unsorted [_]) _ -> True
  _ -> False

-- TODO: implement this for api
replayChoices :: (MonadGame env m) => [Diff.Patch] -> m ()
replayChoices _ = pure ()

-- | How much damage the identity is about to take, as interrupts have left it: "discard
-- that many cards" (Shield Spell).
pendingDamageAmount :: (HasQueue m) => IdentityId -> m Natural
pendingDamageAmount = fmap (maybe 0 damageAmount) . pendingDamage

-- | The cards a player may discard to pay: those in their own hand. A resource card's
-- criteria say when it may be spent: "spend this card only in hero form".
getAvailablePaymentSources ::
  (HasQueue m, MonadThrow m, HasGame m) => IdentityId -> m [PlayerCard]
getAvailablePaymentSources ident = asYou ident do
  hand <- getsGame $ maybe [] (unHand . playerIdentityHand . toAttrs) . lookup ident . gamePlayers
  flip filterM hand \c ->
    if cdCardType (getCardDef c) == ResourceType
      then passesCardCriteria ident c (cdCriteria $ getCardDef c)
      else pure True

-- | Ordinary helpers contribute only cards whose text permits another payer.
resourceCostPayers :: (HasGame m) => IdentityId -> Maybe PlayerCard -> m [IdentityId]
resourceCostPayers ident _card = do
  peril <- getsGame gamePeril
  if isJust peril then pure [ident] else asYou ident $ selectList AnyIdentity

getPaymentSourcesFor ::
  (HasQueue m, MonadThrow m, HasGame m) =>
  IdentityId -> IdentityId -> Maybe PlayerCard -> m [PlayerCard]
getPaymentSourcesFor payer contributor card = do
  ordinary <- allianceCostPayers payer card
  sources <- getAvailablePaymentSources contributor
  if contributor `elem` ordinary then pure sources else do
    allowed <- resourceCostPayers payer card
    if contributor `notElem` allowed then pure [] else filterM shared sources
 where
  shared c = case cdCanPayForOthersWithTrait (getCardDef c) of
    Nothing -> pure False
    Just trait -> identityMatches (IdentityWithId contributor <> IdentityWithTrait trait) contributor

getPaymentResourceAbilities ::
  (MonadThrow m, MonadRandom m, HasQueue m, HasGame m) =>
  IdentityId -> IdentityId -> Maybe PlayerCard -> m [Ability]
getPaymentResourceAbilities payer contributor card = do
  ordinary <- allianceCostPayers payer card
  if contributor `elem` ordinary then getResourceAbilities contributor card else pure []

getAvailableResourcesFor ::
  ( HasQueue m
  , MonadRandom m
  , MonadThrow m
  , HasGame m
  ) =>
  IdentityId ->
  Maybe PlayerCard ->
  m [Resource]
getAvailableResourcesFor ident mc = do
  payers <- resourceCostPayers ident mc
  generated <- concatMapM (\payer -> do
    let reserved = if payer == ident then maybe 0 (sum . map (\case IdentityCountersCost n -> n; _ -> 0) . flattenCosts . cdAbilityCost . getCardDef) mc else 0
    abilitiesResources <- concatMapM (abilityResourcesForCardWithReservedCounters mc reserved) =<< getPaymentResourceAbilities ident payer mc
    playerResources <- concatMapM (`resourcesFor` mc) =<< getPaymentSourcesFor ident payer mc
    pure $ playerResources <> abilitiesResources) payers
  pure $ doubleWildResources (getCardDef <$> mc) generated

getResourceAbilities ::
  ( MonadThrow m
  , MonadRandom m
  , HasQueue m
  , HasGame m
  ) =>
  IdentityId ->
  -- | the card being paid for, if a card is
  Maybe PlayerCard ->
  m [Ability]
getResourceAbilities ident mc = do
  present <- getsGame (HashMap.member ident . gamePlayers)
  permitted <- canResolvePlayerCardAbilities ident
  if present && permitted then resourceAbilitiesForLivingIdentity ident mc else pure []

resourceAbilitiesForLivingIdentity :: (HasGame m, HasQueue m, MonadThrow m, MonadRandom m) => IdentityId -> Maybe PlayerCard -> m [Ability]
resourceAbilitiesForLivingIdentity ident mc = asYou ident do
  printedAbilities <- getAbilitiesForMatching ident (pure . (`elem` [Resource, HeroResource, AlterEgoResource]) . abilityType)
  counters <- project PlayerIdentityCounters ident
  -- With no counters this payment cannot be made. Avoid a full modifier pass for
  -- every affordability check of heroes who have no counter resource to spend.
  counterAbilities <- if counters == 0 then pure [] else do
    modifiers <- getModifiers ident
    blanked <- isBlanked ident
    pure [ label "$label.spendIdentityCounter" $ ability ident 990 Resource IsSelf
             (IdentityCountersCost 1) (Pay $ ResourcePayment Wild)
         | IdentityCountersAsResources `elem` modifiers, not blanked ]
  identity <- getIdentity ident
  syncAbilities <- if toCardCode identity /= "31001a" then pure [] else do
    blanked <- isBlanked ident
    upgrades <- getsGame gameUpgrades
    interfaces <- selectList (UpgradeControlledBy (IdentityWithId ident) <> UpgradeWithTrait Interface <> UnexhaustedUpgrade)
    pure [ label (ikeyWith "label.syncRatioUpgrade" [textVar "upgrade" $ title $ cdName $ getCardDef upgrade]) $ ability ident index Resource IsSelf
             (ExhaustUpgradeCost $ UpgradeWithId uid)
             (Pay $ SyncRatioPayment (fromIntegral $ length icons) <> mconcat (map ResourcePayment icons))
         | not blanked
         , (index, uid) <- zip [991 ..] (Data.List.sort interfaces)
         , Just upgrade <- [HashMap.lookup uid upgrades]
         , let icons = printedResources (getCardDef upgrade)
         ]
  let abilities = printedAbilities <> counterAbilities <> syncAbilities
  filterM
    ( andM
        . sequence
          [ pure . (`elem` [Resource, HeroResource, AlterEgoResource]) . abilityType
          , pure . paysFor
          , paysForAbility
          , withinUseLimit ident
          , preservesCounters
          , passesCriteria ident
          , passesCanAffordCost ident
          ]
    )
    abilities
 where
  paysFor a = all (\m -> maybe False (cardMatch m) mc) (payingForMatchers $ abilityCriteria a)
  paysForAbility a = do
    g <- getGame
    let active = case gameActiveCost g of
          cost : _ -> case activeCostTarget cost of
            ForAbility ab -> (, abilityIndex ab) . toCardCode <$> abilityCardDef g (refocus $ abilitySource ab)
            _ -> Nothing
          _ -> Nothing
        context = gamePayingForAbility g <|> active
        matches = \case
          PayingForAbility code index -> context == Just (code, index) && maybe True ((== code) . toCardCode) mc
          Criteria cs -> all matches cs
          AnyCriteria cs -> any matches cs
          _ -> True
    pure $ matches (abilityCriteria a)
  preservesCounters a = case abilityCost a of
    IdentityCountersCost n -> do
      current <- project PlayerIdentityCounters ident
      let reserved = maybe 0 (sum . map (\case IdentityCountersCost k -> k; _ -> 0) . flattenCosts . cdAbilityCost . getCardDef) mc
      pure $ current >= n + reserved
    _ -> pure True

inCompetitiveAreaOnly :: HasGame m => (a -> Target) -> [a] -> m [a]
inCompetitiveAreaOnly target values = do
  competitive <- getCompetition
  if isNothing competitive then pure values else inYourAreaOnly target values

gameSelectSupportUnscoped :: HasGame m => SupportMatcher -> m (HashSet SupportId)
gameSelectSupportUnscoped = \case
  SupportCanBeReadied -> do
    supports <- HashMap.elems <$> getsGame gameSupports
    readyable <- filterM (\support -> do
      modifiers <- getModifiers support
      pure $ isExhausted support && CannotReady `notElem` modifiers && CannotBeReadiedByPlayerCards `notElem` modifiers) supports
    pure $ HashSet.fromList $ map toId readyable
  UnexhaustedSupport -> do
    supports <- HashMap.elems <$> getsGame gameSupports
    pure $ HashSet.fromList $ map toId $ filter (not . isExhausted) supports
  SupportWithExhaustion exhausted -> do
    supports <- HashMap.elems <$> getsGame gameSupports
    pure $ HashSet.fromList $ map toId $ filter ((== exhausted) . isExhausted) supports
  SupportControlledBy identityMatcher -> do
    supports <- HashMap.elems <$> getsGame gameSupports
    identities <- select identityMatcher
    pure $
      HashSet.fromList $
        map toId $
          filter
            ((`member` identities) . getSupportController)
            supports
  SupportWithUses gameValueMatcher -> do
    upgrades <- HashMap.elems <$> getsGame gameSupports
    HashSet.fromList
      . map toId
      <$> filterM (gameValueMatches gameValueMatcher . getSupportUses) upgrades
  SupportNamed name -> do
    supports <- HashMap.elems <$> getsGame gameSupports
    pure $ HashSet.fromList $ map toId $ filter ((== name) . cdName . supportCardDef . toAttrs) supports
  SupportMatches xs -> do
    all' <- HashSet.fromList . HashMap.keys <$> getsGame gameSupports
    foldr HashSet.intersection all' <$> traverse gameSelectSupport xs
  SupportWithTrait t -> do
    supports <- HashMap.elems <$> getsGame gameSupports
    pure $ HashSet.fromList $ map toId $ filter (member t . cdTraits . supportCardDef . toAttrs) supports

-- | The schemes matching, in any area.
gameSelectAnyScheme :: (HasGame m) => SchemeMatcher -> m (HashSet SchemeId)
gameSelectAnyScheme = \case
  PlayerSideScheme -> getsGame $ HashSet.fromList . map (SchemeSideSchemeId . toId) . filter ((== PlayerSideSchemeType) . cdCardType . getCardDef) . HashMap.elems . gameSideSchemes
  NotScheme matcher -> HashSet.difference <$> gameSelectAnyScheme AnyScheme <*> gameSelectAnyScheme matcher
  SchemeMatches matchers -> do
    initial <- gameSelectAnyScheme AnyScheme
    foldlM (\found matcher -> HashSet.intersection found <$> gameSelectAnyScheme matcher) initial matchers
  AnyScheme -> do
    mainSchemes <- HashMap.elems <$> getsGame gameMainSchemes
    sideSchemes <- HashMap.elems <$> getsGame gameSideSchemes
    pure $
      HashSet.fromList $
        map (SchemeMainSchemeId . toId) mainSchemes
          <> map (SchemeSideSchemeId . toId) sideSchemes
  Matchers.MainScheme -> do
    mainSchemeIds <-
      map (SchemeMainSchemeId . toId) . HashMap.elems <$> getsGame gameMainSchemes
    pure $ HashSet.fromList mainSchemeIds
  MainSchemeOwnedBy you -> getsGame $ \g -> HashSet.fromList [SchemeMainSchemeId sid | (sid,scheme) <- HashMap.toList (gameMainSchemes g),mainSchemeOwner (toAttrs scheme) == Just you]
  EncounterMainScheme -> do
    gliders <- getGliderSchemes
    if null gliders then gameSelectAnyScheme Matchers.MainScheme
      else pure $ fromList $ map SchemeMainSchemeId gliders
  SchemeWithId enemyId -> case enemyId of
    SchemeMainSchemeId mainSchemeId -> do
      mainSchemes <- HashMap.elems <$> getsGame gameMainSchemes
      pure . fromList $
        map (SchemeMainSchemeId . toId) $
          filter
            ((== mainSchemeId) . toId)
            mainSchemes
    SchemeSideSchemeId sideSchemeId ->
      HashSet.map SchemeSideSchemeId
        <$> gameSelectSideScheme (SideSchemeWithId sideSchemeId)
  SchemeWithThreat matcher -> do
    schemes <- HashSet.toList <$> gameSelectScheme AnyScheme
    HashSet.fromList <$> filterM (\scheme -> gameValueMatches matcher =<< gameSelectCountScheme SchemeThreat (SchemeWithId scheme)) schemes
  SchemeWithLeastThreat -> do
    schemes <- HashSet.toList <$> gameSelectScheme AnyScheme
    scored <- for schemes \scheme -> (scheme,) <$> gameSelectCountScheme SchemeThreat (SchemeWithId scheme)
    pure $ HashSet.fromList $ case scored of
      [] -> []
      _ -> let least = minimum (map snd scored) in [scheme | (scheme, threat) <- scored, threat == least]
  SchemeWithoutThwartPrevention -> do
    schemes <- gameSelectAnyScheme AnyScheme
    you <- getYou
    HashSet.fromList <$> filterM (\scheme -> (&&) <$> (notElem ThreatCannotBeRemovedByThwarting <$> getModifiers (schemeRef scheme)) <*> canPayAdditionalThwartCost you scheme) (HashSet.toList schemes)
  ThwartableScheme -> do
    ignores <- elem IgnoresCrisis <$> (getModifiers . IdentityRef =<< getYou)
    crisis <- hasCrisisInYourArea
    thwartable (crisis && not ignores)
  ThwartableSchemeIgnoringCrisis -> thwartable False
  SchemeAHeroCanBasicThwart who -> basicThwartSchemes who
  SchemeYouCanThwart -> do
    schemes <- gameSelectAnyScheme (ThwartableScheme <> SchemeWithoutThwartPrevention)
    who <- getYou
    forbidden <- elem CannotThwartSideSchemes <$> getModifiers (IdentityRef who)
    let allowed = if forbidden then HashSet.filter (\case SchemeSideSchemeId _ -> False; _ -> True) schemes else schemes
    patrolled <- isPatrolled who
    let schemes = allowed
    if patrolled then fromList <$> filterM (\case SchemeMainSchemeId sid -> not <$> isRestrictedMainScheme sid; _ -> pure True) (HashSet.toList schemes) else pure schemes
  SchemeYouCanThwartIgnoringCrisis -> do
    schemes <- gameSelectAnyScheme (ThwartableSchemeIgnoringCrisis <> SchemeWithoutThwartPrevention)
    who <- getYou
    forbidden <- elem CannotThwartSideSchemes <$> getModifiers (IdentityRef who)
    let allowed = if forbidden then HashSet.filter (\case SchemeSideSchemeId _ -> False; _ -> True) schemes else schemes
    patrolled <- isPatrolled who
    let schemes = allowed
    if patrolled then fromList <$> filterM (\case SchemeMainSchemeId sid -> not <$> isRestrictedMainScheme sid; _ -> pure True) (HashSet.toList schemes) else pure schemes
  SchemeAnAllyCanThwart aid ignoresCrisis -> do
    allyModifiers <- getModifiers (AllyRef aid)
    let ownException = BasicThwartIgnoresCrisis `elem` allyModifiers || IgnoresCrisis `elem` allyModifiers
        ignoresPatrol = IgnoresPatrol `elem` allyModifiers
    crisis <- hasCrisisInYourArea
    schemes <- HashSet.fromList <$> (filterM (\scheme -> (&&) <$> canRemoveThreatFrom (AllyRef aid) scheme <*> (notElem ThreatCannotBeRemovedByThwarting <$> getModifiers (schemeRef scheme))) . HashSet.toList =<< thwartable (crisis && not (ignoresCrisis || ownException)))
    controller <- getsGame (fmap getAllyController . lookup aid . gameAllies)
    forbidden <- maybe (pure False) (fmap (elem CannotThwartSideSchemes) . getModifiers . IdentityRef) controller
    numeric <- filterM (\scheme -> do
      assault <- schemeHasKeyword scheme Assault
      getsGame $ \g -> case lookup aid (gameAllies g) of
        Just (Ally.Ally actor) -> if assault then Ally.allyHasAttack actor else Ally.allyHasThwart actor
        Nothing -> False) (HashSet.toList schemes)
    let allowed = if forbidden then HashSet.filter (\case SchemeSideSchemeId _ -> False; _ -> True) (HashSet.fromList numeric) else HashSet.fromList numeric
    patrolled <- maybe (pure False) (\ident -> selectAny (MinionEngagedWith (IdentityWithId ident) <> MinionWithKeyword Patrol)) controller
    if patrolled && not ignoresPatrol then fromList <$> filterM (\case SchemeMainSchemeId sid -> not <$> isRestrictedMainScheme sid; _ -> pure True) (HashSet.toList allowed) else pure allowed
  UnpatrolledMainScheme -> do
    patrolled <- isPatrolled =<< getYou
    schemes <- gameSelectAnyScheme Matchers.MainScheme
    if patrolled then fromList <$> filterM (\case SchemeMainSchemeId sid -> not <$> isRestrictedMainScheme sid; _ -> pure True) (HashSet.toList schemes) else pure schemes
 where
  thwartable crisisSideSchemes = do
    you <- getYou
    let removable = \case
          ThreatCannotBeRemoved -> False
          ThreatRemovableOnlyBy p -> p == you
          _ -> True
    sideSchemes <- filterM (\scheme -> (&&) <$> canPlayerRemoveThreatFrom you (SchemeSideSchemeId $ toId scheme) <*> (all removable <$> getModifiers scheme)) . HashMap.elems =<< getsGame gameSideSchemes
    unrestricted <- filterM (\scheme -> (&&) <$> canPlayerRemoveThreatFrom you (SchemeMainSchemeId $ toId scheme) <*> (not <$> isRestrictedMainScheme (toId scheme))) . filter ((> 0) . getMainSchemeThreat) =<< (filterM (fmap (all removable) . getModifiers) . HashMap.elems =<< getsGame gameMainSchemes)
    if crisisSideSchemes
      then
        pure $
          HashSet.fromList $
            map (SchemeSideSchemeId . toId) (filter ((> 0) . getSideSchemeThreat) sideSchemes)
              <> map (SchemeMainSchemeId . toId) unrestricted
      else do
        mainSchemes <- filterM (fmap (all removable) . getModifiers) . HashMap.elems =<< getsGame gameMainSchemes
        pure $
          HashSet.fromList $
            map
              (SchemeMainSchemeId . toId)
              (filter ((> 0) . getMainSchemeThreat) mainSchemes)
              <> map
                (SchemeSideSchemeId . toId)
                (filter ((> 0) . getSideSchemeThreat) sideSchemes)

withDepthGuard :: (HasGame m, Monoid a) => Int -> ReaderT Game m a -> m a
withDepthGuard depthGuard body = do
  depth <- getsGame gameDepth
  if depth > depthGuard
    then pure mempty
    else getGame >>= runReaderT (local delve body)

delve :: Game -> Game
delve g = g {gameDepth = gameDepth g + 1}

-- | The cards held by the upgrades the player controls, played as if in hand.
-- | Facedown events an ally permits its controller to play. Blanking suppresses
-- that permission without discarding the attached cards.
-- | The effect is serializable but has no card definition: it is engine plumbing
-- for a reduction on one exact card, not a next-card discount.
cardPlayReduction :: EffectId -> IdentityId -> PlayerCard -> Natural -> Effect
cardPlayReduction eid ident card amount = Effect.Effect $ CardPlayReduction Effect.EffectAttrs
  { Effect.effectId = eid
  , Effect.effectCardCode = "engine-card-play-reduction"
  , Effect.effectSource = CardIdRef (pcCardId card)
  , Effect.effectMatcher = IdentityEntity (IdentityWithId ident)
  , Effect.effectModifiers = [ResourceCostReductionForCard (pcCardId card) amount]
  , Effect.effectEnds = Nothing
  }

cardPlayIgnoringCost :: EffectId -> IdentityId -> PlayerCard -> Effect
cardPlayIgnoringCost eid ident card = Effect.Effect $ CardPlayReduction Effect.EffectAttrs
  { Effect.effectId = eid
  , Effect.effectCardCode = "engine-card-play-reduction"
  , Effect.effectSource = CardIdRef (pcCardId card)
  , Effect.effectMatcher = IdentityEntity (IdentityWithId ident)
  , Effect.effectModifiers = [IgnoresResourceCostForCard (pcCardId card)]
  , Effect.effectEnds = Nothing
  }

-- | Validate a nested play with only this card's resource cost ignored.
canPlayCardIgnoringResourceCost :: (HasGame m, HasQueue m) => IdentityId -> PlayerCard -> m Bool
canPlayCardIgnoringResourceCost ident card = do
  g <- getGame
  case lookup ident (gamePlayers g) of
    Nothing -> pure False
    Just player -> do
      available <- getPlayableHandCards ident
      if card `notElem` available then pure False else do
        let eid = EffectId (unCardId $ pcCardId card)
            ignored = g & entitiesL . effectsL %~ insertMap eid (cardPlayIgnoringCost eid ident card)
        app <- newApp ignored []
        runGameApp app (isPlayableFromHand player card)

-- | Med Lab may follow a consequential defeat into any of these out-of-play
-- destinations, but cannot exhaust to recapture a card now in hand or in play.
canCaptureDefeatedAlly :: HasGame m => PlayerCard -> m Bool
canCaptureDefeatedAlly card = do
  g <- getGame
  let cid = pcCardId card
      discarded = [pcCardId c | p <- HashMap.elems (gamePlayers g), c <- unDiscard (playerIdentityDiscard $ toAttrs p)]
      encounters = map ecCardId (scenarioDiscard (toAttrs $ g.gameScenario) <> concatMap pileDiscard (HashMap.elems $ scenarioDecks $ toAttrs $ g.gameScenario))
      victory = map ecCardId (gameVictoryDisplay g)
      collection = map toCardId (g.gameCollection)
  pure $ cid `elem` (discarded <> encounters <> victory <> collection)

-- | A held ally is playable as if in hand, retaining its physical owner.
canPlayHeldAlly :: (HasGame m, HasQueue m) => IdentityId -> PlayerCard -> m Bool
canPlayHeldAlly _ card | not (cardMatch (CardWithType AllyType) card) = pure False
canPlayHeldAlly you card = do
  g <- getGame
  let withCard = g & entitiesL . playersL . ix you %~ \(PlayerIdentity a) -> PlayerIdentity a
        {playerIdentityHand = Hand $ card : unHand (playerIdentityHand a)}
  canPlayCardWithReductionIn withCard you 0 card

heldAllyPlayable :: (HasGame m, HasQueue m) => IdentityId -> SupportId -> m Bool
heldAllyPlayable you sid = do
  cards <- getsGame $ maybe [] (supportHeldCards . toAttrs) . lookup sid . gameSupports
  anyM (canPlayHeldAlly you) cards

-- | Check the complete ordinary play rules under the same exact-card reduction
-- that the nested play will receive. Feasibility uses an isolated copy of the game.
canPlayCardWithReduction :: (HasGame m, HasQueue m) => IdentityId -> Natural -> PlayerCard -> m Bool
canPlayCardWithReduction ident amount card = do
  g <- getGame
  canPlayCardWithReductionIn g ident amount card

-- Preview the explicit permission to play a searched card without granting it
-- ordinary hand permissions in the real game.
canPlaySearchedCardWithReduction :: (HasGame m, HasQueue m) => IdentityId -> Natural -> PlayerCard -> m Bool
canPlaySearchedCardWithReduction you amount card = do
  g <- getGame
  let staged = g & entitiesL . playersL . ix you %~ \(PlayerIdentity attrs) -> PlayerIdentity attrs
        {playerIdentityHand = Hand (card : filter ((/= pcCardId card) . pcCardId) (unHand $ playerIdentityHand attrs))}
  canPlayCardWithReductionIn staged you amount card

canPlayCardWithReductionIn :: (HasGame m, HasQueue m) => Game -> IdentityId -> Natural -> PlayerCard -> m Bool
canPlayCardWithReductionIn g ident amount card = case lookup ident (gamePlayers g) of
  Nothing -> pure False
  Just player -> do
    -- Physical card IDs make feasibility checks independent of randomness.
    let eid = EffectId (unCardId $ pcCardId card)
        reduced = g & entitiesL . effectsL %~ insertMap eid (cardPlayReduction eid ident card amount)
    app <- newApp reduced []
    runGameApp app do
      available <- getPlayableHandCards ident
      if card `elem` available then isPlayableFromHand player card else pure False

-- | Meditation's initiation check excludes itself as a target and resource, and
-- tests every candidate after the identity has been exhausted to pay its cost.
hasCardToPlayWithReductionAfterExhausting :: (HasGame m, HasQueue m) => IdentityId -> PlayerCard -> Natural -> m Bool
hasCardToPlayWithReductionAfterExhausting ident source amount = do
  g <- getGame
  let exhausted = g & entitiesL . playersL . ix ident %~ \(PlayerIdentity a) -> PlayerIdentity a
        { playerIdentityExhausted = True
        , playerIdentityHand = Hand (filter ((/= pcCardId source) . pcCardId) (unHand $ playerIdentityHand a)) }
  app <- newApp exhausted []
  cards <- runGameApp app (getPlayableHandCards ident)
  anyM (canPlayCardWithReductionIn exhausted ident amount) cards

-- | Only supports granting permission expose their attached cards as playable.
-- Blanking suppresses that permission, while preserving their facedown cards.
supportIsInPlay :: (HasGame m) => SupportId -> m Bool
supportIsInPlay sid = getsGame (HashMap.member sid . gameSupports)

getSupportHeldCardsOn :: (HasGame m) => SupportId -> m [PlayerCard]
getSupportHeldCardsOn sid = getsGame $ maybe [] (supportHeldCards . toAttrs) . lookup sid . gameSupports

isCurrentBoostCard :: (HasGame m) => EncounterCard -> m Bool
isCurrentBoostCard card = getsGame $ any ((== ecCardId card) . ecCardId) . HashMap.elems . gameBoostCards

getBoostCard :: (HasGame m) => Target -> m (Maybe EncounterCard)
getBoostCard target = getsGame $ \g -> lookup target (gameBoostCards g) <|> inPlayBoostCard g target

countControlledCardsWithTrait :: (HasGame m) => IdentityId -> Trait -> m Natural
countControlledCardsWithTrait you trait = do
  allies <- selectList (AllyControlledBy $ IdentityWithId you)
  supports <- selectList (SupportControlledBy $ IdentityWithId you)
  upgrades <- selectList (UpgradeControlledBy $ IdentityWithId you)
  let refs = IdentityRef you : map AllyRef allies <> map SupportRef supports <> map UpgradeRef upgrades
  fromIntegral . length <$> filterM (\target -> do
    printed <- getsGame $ maybe mempty cdTraits . (`abilityCardDef` target)
    gained <- getGainedTraits target
    pure $ trait `member` (printed <> gained)) refs

characterAttackValue :: (HasGame m) => CharacterId -> m Natural
characterAttackValue = \case
  IdentityCharacter ident -> currentIdentity <$> getIdentity ident >>= \case
    HeroSide hero -> Hero.getModifiedAttack (toAttrs hero)
    AlterEgoSide _ -> pure 0
  AllyCharacter aid -> getsGame (lookup aid . gameAllies) >>= maybe (pure 0) (Ally.getModifiedAttack . toAttrs)
  VillainCharacter vid -> getVillainAttackValue vid
  MinionCharacter mid -> getsGame (lookup mid . gameMinions) >>= maybe (pure 0) (Minion.getModifiedAttack . toAttrs)
  AttachmentCharacter _ -> pure 0

getSupportHoldingCard :: (HasGame m) => PlayerCard -> m (Maybe SupportId)
getSupportHoldingCard card = getsGame $ headMay . map fst . filter (elem card . supportHeldCards . toAttrs . snd) . HashMap.toList . gameSupports

getSupportHeldCards :: (HasGame m) => IdentityId -> m [PlayerCard]
getSupportHeldCards ident = do
  supports <- getsGame $ filter (\s -> getSupportController s == ident && supportCanPlayHeldEvents (toAttrs s) && notNull (supportHeldCards $ toAttrs s)) . HashMap.elems . gameSupports
  active <- filterM (fmap not . isBlanked) supports
  pure $ concatMap (supportHeldCards . toAttrs) active

getHeldPlayableCards :: (HasGame m) => IdentityId -> m [PlayerCard]
getHeldPlayableCards ident = concat <$> sequence [getUpgradeHeldCards ident, getAllyHeldCards ident, getSupportHeldCards ident]

getAllyHoldingCard :: (HasGame m) => PlayerCard -> m (Maybe AllyId)
getAllyHoldingCard card = getsGame $ headMay . map fst . filter (elem card . Ally.allyHeldCards . toAttrs . snd) . HashMap.toList . gameAllies

getAllyHeldCards :: (HasGame m) => IdentityId -> m [PlayerCard]
getAllyHeldCards ident = do
  allies <- getsGame $ filter (\a -> getAllyController a == ident && notNull (Ally.allyHeldCards $ toAttrs a)) . HashMap.elems . gameAllies
  active <- filterM (fmap not . isBlanked) allies
  pure $ concatMap (Ally.allyHeldCards . toAttrs) active

-- | Players who may contribute to this card. Peril and separate game areas still
-- prevent help from players who cannot participate in the resolving player's game.
allianceCostPayers :: (HasGame m) => IdentityId -> Maybe PlayerCard -> m [IdentityId]
allianceCostPayers ident card = do
  peril <- getsGame gamePeril
  shared <- activeEncounterAllowsContributions
  if (maybe False (member Alliance . cdKeywords . getCardDef) card || shared) && isNothing peril
    then asYou ident $ selectList AnyIdentity
    else pure [ident]

getUpgradeHeldCards :: (HasGame m) => IdentityId -> m [PlayerCard]
getUpgradeHeldCards ident = do
  hosts <- getsGame $ filter (\u -> getUpgradeController u == ident && not (upgradeHeldCardsFacedown $ toAttrs u) && Upgrade.upgradeCanPlayHeldCards (toAttrs u)) . HashMap.elems . gameUpgrades
  active <- filterM (fmap not . isBlanked) hosts
  pure $ concatMap (upgradeHeldCards . toAttrs) active

-- | The traits the character gains (a card can gain a trait, RR Traits): from those
-- worked out for the game, or, for a character they were not worked out for (an ally
-- looked at in hand), from its modifiers.
getGainedTraits :: (HasGame m, IsRef a) => a -> m (HashSet Trait)
getGainedTraits a =
  getsGame gameGainedTraits >>= \case
    GainedTraits (Just layer) | Just gained <- HashMap.lookup (toTarget a) layer -> pure gained
    _ -> gainedTraitsFromModifiers =<< getModifiers a

traitsGainedFrom :: [Modifier] -> HashSet Trait
traitsGainedFrom ms = HashSet.fromList [t | TraitModifier t <- ms]

-- Copied traits participate in the same monotone trait layer as printed grants.
gainedTraitsFromModifiers :: HasGame m => [Modifier] -> m (HashSet Trait)
gainedTraitsFromModifiers ms = do
  copied <- traverse getCharacterTraits [c | CopyTraitsFrom c <- ms]
  pure $ traitsGainedFrom ms <> mconcat copied

-- | Work out the traits each character gains, once for the game as it stands, so that
-- asking a trait never runs every modifier again: Blaze of Glory's "each Guardian
-- character" asked each character's traits, each of which ran Blaze of Glory, and so
-- on. Lazy, so a game state no trait is asked of costs nothing.
--
-- A card that grants a trait can ask a trait itself (Ant-Man's Scott Lang gains Giant
-- while you are Giant), so this is a fixpoint: start with nothing gained, grant each
-- trait against what is known, until nothing more is gained. ponytail: modifiers only
-- ever add a trait, so it settles; a "loses a trait" modifier would need layers.
refreshTraits :: Game -> Game
refreshTraits g = blank {gameGainedTraits = GainedTraits (Just $ go (0 :: Int) none)}
 where
  -- the old traits dropped first, so this game does not hold on to the one before
  !blank = g {gameGainedTraits = GainedTraits Nothing}
  characters =
    map IdentityRef (HashMap.keys $ gamePlayers g)
      <> map AllyRef (HashMap.keys $ gameAllies g)
      <> map MinionRef (HashMap.keys $ gameMinions g)
      <> map VillainRef (HashMap.keys $ gameVillains g)
  none = HashMap.fromList [(c, mempty) | c <- characters]
  -- wherever the character is, whoever looks
  grant known =
    runIdentity . runReaderT (HashMap.fromList <$> for characters \c -> (c,) <$> (gainedTraitsFromModifiers =<< getModifiers c)) $
      blank {gameGainedTraits = GainedTraits (Just known), gameAllAreas = True}
  go n known
    | n > 20 = error "the traits characters gain do not settle"
    | otherwise = let next = grant known in if next == known then known else go (n + 1) next

inElementArea :: (HasGame m, IsRef a) => a -> ReaderT Game m b -> m b
inElementArea target body = do
  g <- getGame
  let current = fromMaybe (g.gameActivePlayer) (gameYou g)
      here = areaId <$> areaOfTarget g (if toTarget target == GameRef then IdentityRef current else toTarget target)
      same you = (areaId <$> areaOfTarget g (IdentityRef you)) == here
      context = if isNothing (gameCompetition g) || same current then current
        else fromMaybe current $ find same (gamePlayerOrder g <> gameSeats g)
  runReaderT body g {gameYou = Just context}

getModifiers :: (HasGame m, IsRef a) => a -> m [Modifier]
getModifiers a = do
  g <- getGame
  let target = friendlyBoostTarget g $ canonicalAttachmentTarget g (toTarget a)
      bonuses = case target of
        AllyRef aid -> [modifier | context <- g.gameFriendlyBoosts,friendlyBoostAlly context == aid,modifier <- [AttackModifier $ friendlyBoostIcons context,ThwartModifier $ friendlyBoostIcons context] <> map KeywordModifier (friendlyBoostKeywords context)]
        _ -> []
  (bonuses <>) <$> inElementArea target (getModifiersInContext target)

getModifiersInContext :: (HasGame m, IsRef a) => a -> m [Modifier]
getModifiersInContext a = withDepthGuard 2 $ do
  snapshot <- getGame
  let allowed :: IsRef provider => provider -> Bool
      allowed provider = modifierProviderAllowed snapshot (toTarget a) (toTarget provider)
  effects <- filter (allowed . toAttrs) . HashMap.elems <$> getsGame gameEffects
  -- a card whose text box is blank gives no modifiers
  upgrades <- filterM (fmap not . isBlanked) . filter allowed . HashMap.elems =<< getsGame gameUpgrades
  supports <- filterM (fmap not . isBlanked) . filter allowed . HashMap.elems =<< getsGame gameSupports
  attachments <- filter allowed . HashMap.elems <$> getsGame gameAttachments
  allies <- filterM (fmap not . isBlanked) . filter allowed . HashMap.elems =<< getsGame gameAllies
  minions <- filterM (fmap not . isBlanked) . filter allowed . HashMap.elems =<< getsGame gameMinions
  events <- filter (allowed . toAttrs) . HashMap.elems <$> getsGame gameEvents
  players <- filter allowed . HashMap.elems <$> getsGame gamePlayers
  sideSchemes <- filterM (fmap not . isBlanked) . filter allowed . HashMap.elems =<< getsGame gameSideSchemes
  mainSchemes <- filter allowed . HashMap.elems <$> getsGame gameMainSchemes
  villains <- filter (allowed . toAttrs) . HashMap.elems <$> getsGame gameVillains
  environments <- filter allowed . HashMap.elems <$> getsGame gameEnvironments
  treacheries <- filter allowed . HashMap.elems <$> getsGame gameTreacheries
  obligations <- filter allowed . HashMap.elems <$> getsGame gameObligations
  -- A card answers its own play-cost text from hand and while reserved for an
  -- active payment. Reserving it must not turn off a continuous cost ability.
  let inHandOf :: (HasGame m) => CardType -> (IdentityId -> PlayerCard -> x) -> m [x]
      inHandOf cardType create = case toTarget a of
        CardIdRef cid -> getsGame \g ->
          [ create owner card
          | (owner, card) <- Data.List.nubBy (\(_, left) (_, right) -> pcCardId left == pcCardId right) $
              [(toId p, c) | p <- HashMap.elems (gamePlayers g), c <- unHand (playerIdentityHand $ toAttrs p)
                  <> [top | toCardCode p == "45030a", top <- take 1 (unDeck $ playerIdentityDeck $ toAttrs p)]
                  <> maybeToList (playerIdentityPendingTopDeckPlay $ toAttrs p)]
                <> [(activeCostIdentityId cost, c) | cost <- gameActiveCost g, ForCard c <- [activeCostTarget cost]]
          , pcCardId card == cid
          , cdCardType (getCardDef card) == cardType
          ]
        _ -> pure []
  inHand <- inHandOf AllyType createAlly
  eventsInHand <- inHandOf EventType createEvent
  supportsInHand <- inHandOf SupportType createSupport
  upgradesInHand <- inHandOf UpgradeType createUpgrade
  attackBlank <- getsGame $ \g -> [Blank | attackBlanksTarget g (toTarget a)]
  groupedDefense <- getsGame $ \g -> [CannotDefend | IdentityRef who <- [toTarget a], isDefendingOtherSimultaneousAttack who g]
  mconcat
    <$> sequence
      [ pure attackBlank
      , pure groupedDefense
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) effects
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) inHand
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) eventsInHand
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) supportsInHand
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) upgradesInHand
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) upgrades
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) supports
      , concatMapM (\attachment -> do
          blanked <- isBlanked attachment
          outside <- getPrintedModifiersFor (toSource a) (toTarget a) attachment
          inside <- if blanked then pure [] else getModifiersFor (toSource a) (toTarget a) attachment
          pure $ outside <> inside) attachments
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) allies
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) minions
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) events
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) players
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) sideSchemes
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) mainSchemes
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) villains
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) environments
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) treacheries
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) obligations
      ]

getCurrentWindows :: (HasGame m) => m [Window]
getCurrentWindows = do
  windows <- getsGame gameWindows
  pure $ case windows of
    [] -> []
    x : _ -> x

getActiveCost :: (HasGame m) => m (Maybe ActiveCostId)
getActiveCost = do
  costStack <- getsGame gameActiveCost
  pure $ case costStack of
    [] -> Nothing
    cost : _ -> Just $ activeCostId cost

-- | The original payer, even while a contributor pays one of this cost's parts.
getActiveCostCard :: (HasGame m) => ActiveCostId -> m (Maybe PlayerCard)
getActiveCostCard acid = do
  cost <- getsGame $ headMay . filter ((== acid) . activeCostId) . gameActiveCost
  maybe (pure Nothing) activeCostPaymentCard cost

getActiveCostPayer :: (HasGame m) => ActiveCostId -> m (Maybe IdentityId)
getActiveCostPayer acid = getsGame $ fmap activeCostIdentityId . find ((== acid) . activeCostId) . gameActiveCost

-- | A card-owned predicate for a complete exhaust-cost assignment. Non-events
-- have no event predicate; event code determines its own meaningful effect.
eventExhaustionsValid :: (HasGame m) => IdentityId -> PlayerCard -> [CharacterId] -> m Bool
eventExhaustionsValid payer card characters = case cdCardType (getCardDef card) of
  EventType -> case createEvent payer card of Event e -> eventExhaustionCostValid e characters
  _ -> pure True

-- | The resources actually spent on an event, after declarations and wasted surplus.
getEventPlayerCard :: HasGame m => EventId -> m (Maybe PlayerCard)
getEventPlayerCard eid = getsGame $ \g -> do
  event <- lookup eid (gameEvents g)
  let you = eventController (toAttrs event)
  pure $ MkPlayerCard (CardId $ unEventId eid) (eventCardDef $ toAttrs event) (Just $ eventOwner $ toAttrs event) (Just you)

getEventPaidResources :: (HasGame m) => EventId -> m [Resource]
getEventPaidResources eid = do
  receipt <- getsGame (fmap (eventPaidWith . toAttrs) . lookup eid . gameEvents)
  pure $ maybe [] resources receipt
 where
  resources p = fromMaybe (mapMaybe one $ payments p) (paymentActualResources p)
  one (ResourcePayment r) = Just r
  one (WildPayment r) = Just r
  one (ContributedWildPayment _ r) = Just r
  one _ = Nothing

getCurrentPayment :: (HasGame m) => m Payment
getCurrentPayment = do
  costStack <- getsGame gameActiveCost
  pure $ case costStack of
    [] -> NoPayment
    cost : _ -> activeCostPayment cost

getDifficulty :: (HasGame m) => m Difficulty
getDifficulty = getsGame $ getScenarioDifficulty . gameScenario

abilityResources ::
  (HasQueue m, MonadThrow m, HasGame m) => Ability -> m [Resource]
abilityResources = abilityResourcesWithReservedCounters 0

-- | Repeated counter spending has one activation choice, but every unreserved
-- counter contributes to affordability. Counters owed by the card itself remain due.
abilityResourcesWithReservedCounters ::
  (HasQueue m, MonadThrow m, HasGame m) => Natural -> Ability -> m [Resource]
abilityResourcesWithReservedCounters = abilityResourcesForCardWithReservedCounters Nothing

abilityResourcesForCardWithReservedCounters ::
  (HasQueue m, MonadThrow m, HasGame m) => Maybe PlayerCard -> Natural -> Ability -> m [Resource]
abilityResourcesForCardWithReservedCounters context reserved a = do
  def <- getsGame (`abilityCardDef` refocus (abilitySource a))
  controller <- getSourceController (abilitySource a)
  generated <- case (toCardCode <$> def, controller) of
    (Just "56004", Just you) -> do
      count <- selectListCount (MinionEngagedWith $ IdentityWithId you)
      pure $ replicate (fromIntegral $ min 3 count) Physical
    (Just "56036", _) -> do
      let sameSet = maybe False (\card -> canonicalCardCode (getCardDef card) `elem` ["56030","56031","56032","56033","56034","56035","56036","56037","56038","56039","56040"]) context
      pure $ replicate (if sameSet then 2 else 1) Wild
    (Just "59012", Just you) -> do
      count <- HCHelpers.giftCount you
      pure $ replicate (fromIntegral count) Wild
    (Just "60043", _) -> pure [Wild]
    _ -> go (abilityChoices a)
  repeats <- case (abilitySource a, baseLimit $ abilityLimit a, abilityCost a) of
    (IdentityRef ident, NoLimit, IdentityCountersCost n) | n > 0 -> do
      counters <- project PlayerIdentityCounters ident
      pure $ subtractNatural reserved counters `div` n
    _ -> pure 1
  pure $ concat $ replicate (fromIntegral repeats) generated
 where
  go [] = pure []
  go (Pay receipt : xs) = do
    icons <- concatMapM receiptResources (payments receipt)
    (icons <>) <$> go xs
  go (_ : xs) = go xs
  receiptResources = \case
    PhysicalResourcesForTough ident -> do
      tough <- project PlayerIdentityTough ident
      extra <- project PlayerIdentityAdditionalTough ident
      pure $ replicate (fromIntegral $ (if tough then 1 else 0) + extra) Physical
    OverpaymentOnlyPayment r -> pure [r]
    ResourcePayment r -> pure [r]
    ResourcePaymentFromPlayerCard card -> resourcesFor card context
    ResourcePaymentFromCard matcher -> do
      cards <- selectList matcher
      case cards of
        [] -> pure []
        [c] -> pure $ printedResources $ getCardDef c
        _ -> error "invalid matcher"
    _ -> pure []

-- | The hazard icons in "your" area.
hasCrisisInYourArea :: (HasGame m) => m Bool
hasCrisisInYourArea = do
  (crisis, _, _, _) <- getEncounterIconCounts
  pure $ crisis > 0

-- | The target's extra damage cost is a cost of thwarting, not of merely moving
-- threat. Selection must not offer a scheme whose damage cannot be paid.
canPayAdditionalThwartCost :: (HasGame m) => IdentityId -> SchemeId -> m Bool
canPayAdditionalThwartCost who scheme = do
  modifiers <- getModifiers (schemeRef scheme)
  let indirect = sum [n | AdditionalThwartCost c <- modifiers, IndirectDamageCost n <- flattenCosts c]
  if indirect == 0 then pure True else (>= indirect) . sum . map snd <$> indirectCostCandidates who (refocus $ schemeRef scheme)

-- | Printed keywords are blankable; external gains and losses still apply.
schemeHasKeyword :: HasGame m => SchemeId -> Keyword -> m Bool
schemeHasKeyword scheme keyword = do
  let target = schemeRef scheme
  blanked <- isBlanked target
  modifiers <- getModifiers target
  printed <- getsGame $ maybe [] (toList . cdKeywords) . (`abilityCardDef` target)
  pure $ LoseKeyword keyword `notElem` modifiers
    && ((not blanked && keyword `elem` printed) || KeywordModifier keyword `elem` modifiers)

basicThwartSchemes :: (HasGame m) => IdentityId -> m (HashSet SchemeId)
basicThwartSchemes who = do
  modifiers <- getModifiers (IdentityRef who)
  crisis <- hasCrisisInYourArea
  let ignoringCrisis = IgnoresCrisis `elem` modifiers || BasicThwartIgnoresCrisis `elem` modifiers
  allSchemes <- selectList (ThwartableSchemeIgnoringCrisis <> SchemeWithoutThwartPrevention)
  patrolled <- isPatrolled who
  legal <- filterM (\case SchemeMainSchemeId sid -> do
                            restricted <- isRestrictedMainScheme sid
                            pure $ not restricted || ((not crisis || ignoringCrisis) && (not patrolled || BasicThwartIgnoresPatrol `elem` modifiers))
                          _ -> pure True) allSchemes
  let allowed = if CannotThwartSideSchemes `elem` modifiers then filter (\case SchemeSideSchemeId _ -> False; _ -> True) legal else legal
  candidates <- filterM (\scheme -> (&&) <$> canRemoveThreatFrom (IdentityRef who) scheme <*> canPayAdditionalThwartCost who scheme) allowed
  if BasicThwartOnlyMostThreat `notElem` modifiers then pure (fromList candidates) else do
    boardSchemes <- selectList AnyScheme
    amounts <- for boardSchemes (\s -> (s,) <$> gameSelectCountScheme SchemeThreat (SchemeWithId s))
    let largest = maximum (0 : map snd amounts)
    pure $ fromList [s | s <- candidates, Just largest == Data.List.lookup s amounts]

getHazardCount :: (HasGame m) => m Natural
getHazardCount = getHazardCountIn . areaId =<< getYourArea

-- | Crisis, acceleration, amplify and hazard icons on cards in the current
-- game area. Physical acceleration tokens are deliberately excluded.
getEncounterIconCounts :: (HasGame m) => m (Natural, Natural, Natural, Natural)
getEncounterIconCounts = do
  aid <- areaId <$> getYourArea
  getsGame $ \g -> encounterIconCountsIn g aid

encounterIconCountsIn :: Game -> AreaId -> (Natural, Natural, Natural, Natural)
encounterIconCountsIn g aid =
  let printed :: (HasCardDef a) => (k -> Target) -> HashMap k a -> [CardDef]
      printed ref cards =
        [ getCardDef card
        | (key, card) <- HashMap.toList cards
        , (areaId <$> areaOfTarget g (ref key)) == Just aid
        , not (blankIn g $ ref key)
        , not (iconsSuppressedIn g $ ref key)
        ]
      defs = printed IdentityRef (gamePlayers g)
        <> printed VillainRef (gameVillains g)
        <> printed MinionRef (gameMinions g)
        <> printed AllyRef (gameAllies g)
        <> printed SideSchemeRef (gameSideSchemes g)
        <> printed MainSchemeRef (gameMainSchemes g)
        <> printed AttachmentRef (gameAttachments g)
        <> printed EnvironmentRef (gameEnvironments g)
        <> [supportCardDef (toAttrs card)
           | (key, card) <- HashMap.toList (gameSupports g)
           , (areaId <$> areaOfTarget g (SupportRef key)) == Just aid
           , not (blankIn g $ SupportRef key)]
        <> printed UpgradeRef (gameUpgrades g)
        <> printed ObligationRef (gameObligations g)
      modifiers = concat (iconModifiersIn g aid)
      crisis = fromIntegral (length
        [ () | (sid, scheme) <- HashMap.toList (gameSideSchemes g)
        , isCrisis scheme
        , (areaId <$> areaOfTarget g (SideSchemeRef sid)) == Just aid
        , not (blankIn g $ SideSchemeRef sid)
        , not (iconsSuppressedIn g $ SideSchemeRef sid)
        ]) + fromIntegral (length [() | CrisisIcon <- modifiers])
   in ( crisis
      , sum (map cdAcceleration defs) + sum [n | AccelerationIconModifier n <- modifiers]
      , sum (map cdAmplify defs) + sum [n | AmplifyIconModifier n <- modifiers]
      , sum (map cdHazards defs) + sum [n | HazardIconModifier n <- modifiers]
      )

getEncounterIconCount :: (HasGame m) => m Natural
getEncounterIconCount = do
  (crisis, acceleration, amplify, hazard) <- getEncounterIconCounts
  pure $ crisis + acceleration + amplify + hazard

getHazardCountIn :: (HasGame m) => AreaId -> m Natural
getHazardCountIn aid = getsGame $ \g ->
  let (_, _, _, hazard) = encounterIconCountsIn g aid
   in hazard

-- | The cards in the victory display, the first added first (RR Victory Display).
getVictoryDisplay :: (HasGame m) => m [EncounterCard]
getVictoryDisplay = getsGame gameVictoryDisplay

getPlayerVictoryDisplay :: (HasGame m) => m [PlayerCard]
getPlayerVictoryDisplay = getsGame gamePlayerVictoryDisplay

-- Hela

-- | The side scheme cards in the victory display (Hela, Hela's Domain).
sideSchemesInVictoryDisplay :: (HasGame m) => m Natural
sideSchemesInVictoryDisplay = do
  encounter <- getVictoryDisplay
  player <- getPlayerVictoryDisplay
  pure $ fromIntegral $ length [() | c <- map EncounterCard encounter <> map PlayerCard player, cdCardType (getCardDef c) `elem` [SideSchemeType, PlayerSideSchemeType]]

-- | The cards a main scheme holds faceup, attached to it (Odin on Odin's Torment).
getMainSchemeHeldCards :: (HasGame m) => MainSchemeId -> m [PlayerCard]
getMainSchemeHeldCards ms = getsGame (maybe [] (mainSchemeHeldCards . toAttrs) . lookup ms . gameMainSchemes)

-- | The ally card as a minion with a blank text box (Fallen Warrior, Beguiled): its SCH
-- its printed THW, its ATK and hit points its printed ones, its damage the ally's; its
-- printed traits kept (RR Traits), its keywords gone with its text box. It is the
-- player's card, for its owner's discard pile as it leaves play; the board shows it as
-- the ally's card.
allyAsMinion :: IdentityId -> PlayerCard -> Attrs Ally -> Minion
allyAsMinion ident card a = overMinion become base
 where
  mid = MinionId $ unCardId $ pcCardId card
  original = Ally.allyOriginalMinion a >>= \value -> case fromJSON value of
    Success minion -> Just minion
    Error _ -> Nothing
  base = fromMaybe (lookupMinion "xallyminion" ident mid) original
  become m =
    m
      { Minion.minionCardDef = maybe ((pcCardDef card) {cdCardType = MinionType, cdKeywords = mempty}) (Minion.minionCardDef . toAttrs) original
      , Minion.minionScheme = maybe (Sch $ unThw $ Ally.allyThwart a) (Minion.minionScheme . toAttrs) original
      , Minion.minionAttack = maybe (Ally.allyAttack a) (Minion.minionAttack . toAttrs) original
      , Minion.minionHitPoints = maybe (Ally.allyHitPoints a) (Minion.minionHitPoints . toAttrs) original
      , Minion.minionExhausted = Ally.allyExhausted a
      , Minion.minionDamage = Ally.allyDamage a
      , Minion.minionCounters = Ally.allyCounters a
      , Minion.minionHeldCards = Ally.allyHeldCards a
      , Minion.minionFacedownCard = Just card
      , Minion.minionUpgrades = allyUpgrades a
      , Minion.minionHeldTreacheries = Ally.allyHeldTreacheries a
      }

-- | The cards in The Collection (Infiltrate the Museum), the first added first: a player
-- card's owner is its 'pcOwner'.
getCollection :: (HasGame m) => m [Card]
getCollection = getsGame gameCollection

-- | The victory points in the victory display: each card's X (RR Victory X).
getVictoryPoints :: (HasGame m) => m Int
getVictoryPoints = sum . map victoryValue <$> getVictoryDisplay

-- | A card's X, for victory X, if it has the keyword.
victoryValue :: (HasCardDef a) => a -> Int
victoryValue a = sum [n | Victory n <- toList (cdKeywords $ getCardDef a)]

-- | Indirect damage (RR): assigned a point at a time, by the player given, among the
-- characters the players given control that can take damage, none past what would defeat
-- it (a tough one too, its card preventing what it is given), then dealt at once; what
-- cannot be assigned is ignored. The step given goes on with what is left to assign.
assignIndirectDamage ::
  (ReverseQueue m) =>
  IdentityId ->
  [IdentityId] ->
  Source ->
  Damage ->
  [(CharacterId, Natural)] ->
  (Damage -> [(CharacterId, Natural)] -> m ()) ->
  Bool ->
  m ()
assignIndirectDamage chooser idents source damage assigned next exhaustDamaged = do
  g <- getGame
  let n = damageAmount damage
      given c = sum [k | (c', k) <- assigned, c' == c]
  identity <- for [(i, p) | i <- idents, Just p <- [lookup i (gamePlayers g)]] \(i, p) -> (IdentityCharacter i,,) <$> getModifiedHp p <*> pure (identityDamage p)
  allies <- for [a | a <- HashMap.elems (gameAllies g), getAllyController a `elem` idents] \a ->
    (AllyCharacter (toId a),,) <$> Ally.getModifiedHitPoints (toAttrs a) <*> pure (getAllyDamage a)
  open <-
    filterM (fmap (notElem CannotTakeDamage) . getModifiers . characterRef . fst)
      [(c, room) | (c, hp, damage) <- identity <> allies, let room = subtractNatural (damage + given c) hp, room > 0]
  let assign c k = next damage {damageAmount = n - k} ((c, k) : assigned)
  case open of
    _ | n == 0 || null open ->
      pushAll . simultaneousDamage =<< traverse (\(c, k) -> capture do
        before <- characterDamageNow c
        damageCharacter c source damage {damageAmount = k, damageIndirect = True}
        when exhaustDamaged $ push $ ExhaustIfDamaged c before) (HashMap.toList $ HashMap.fromListWith (+) assigned)
    [(c, room)] -> assign c (min n room)
    _ -> chooseOnePrompted chooser "$label.assignIndirectDamage" [targeting (characterRef c) (assign c 1) | (c, _) <- open]

-- * The Milano and the Power Stone (The Galaxy's Most Wanted)

-- | The Milano support (Ship Command), while it is in play.
getMilano :: (HasGame m) => m (Maybe SupportId)
getMilano = getsGame \g -> headMay [toId s | s <- HashMap.elems (gameSupports g), isMilano s]

isMilano :: Support -> Bool
isMilano = (== "Milano") . title . cdName . supportCardDef . toAttrs

-- | Whether the Milano is in play and ready, so it can be exhausted: "exhaust the Milano"
-- is a cost and an option only then.
milanoIsReady :: (HasGame m) => m Bool
milanoIsReady = getsGame \g -> any (\s -> isMilano s && not (isExhausted s)) (HashMap.elems $ gameSupports g)

-- | Whether the player controls the Milano: the first player does.
controlsMilano :: (HasGame m) => IdentityId -> m Bool
controlsMilano ident = getsGame \g -> any (\s -> isMilano s && getSupportController s == ident) (HashMap.elems $ gameSupports g)

-- | The Power Stone attachment (Power Stone), while it is in play.
getPowerStone :: (HasGame m) => m (Maybe AttachmentId)
getPowerStone = getsGame \g -> headMay [toId a | a <- HashMap.elems (gameAttachments g), title (cdName $ getCardDef a) == "Power Stone"]

-- | The character an attachment is attached to: an enemy, an identity or an ally.
getAttachedCharacter :: (HasGame m) => AttachmentId -> m (Maybe CharacterId)
getAttachedCharacter aid = getsGame $ \g ->
  (do
    at <- toAttrs <$> lookup aid (gameAttachments g)
    (enemyCharacter <$> Attachment.attachmentEnemy at)
      <|> (IdentityCharacter <$> Attachment.attachmentIdentity at)
      <|> (AllyCharacter <$> Attachment.attachmentAlly at))
  <|> (do
    at <- toAttrs <$> attachmentUpgrade g aid
    (enemyCharacter <$> upgradeAttachedEnemy at)
      <|> (IdentityCharacter <$> Upgrade.upgradeAttachedIdentity at)
      <|> (AllyCharacter <$> upgradeAttachedAlly at))
 where
  enemyCharacter = \case
    EnemyVillainId v -> VillainCharacter v
    EnemyMinionId m -> MinionCharacter m
    EnemyAttachmentId aid -> AttachmentCharacter aid

-- | The hero or villain the Power Stone is attached to.
getPowerStoneHolder :: (HasGame m) => m (Maybe CharacterId)
getPowerStoneHolder = maybe (pure Nothing) getAttachedCharacter =<< getPowerStone

-- | Whether the player controls the Power Stone: it is attached to their identity.
controlsPowerStone :: (HasGame m) => IdentityId -> m Bool
controlsPowerStone ident = (== Just (IdentityCharacter ident)) <$> getPowerStoneHolder

-- | The amplify icons in play in "your" area (RR Amplify Icon): on its schemes, villains,
-- minions, attachments and environments.
getAmplifyCount :: (HasGame m) => m Natural
getAmplifyCount = do
  (_, _, amplify, _) <- getEncounterIconCounts
  pure amplify

-- | The cards of the map in the area.
inArea :: Game -> AreaId -> (k -> Target) -> HashMap k a -> [a]
inArea g aid ref = map snd . filter (\(k, _) -> (areaId <$> areaOfTarget g (ref k)) == Just aid) . HashMap.toList

-- | The acceleration tokens and the acceleration icons in "your" area.
getAccelerationCount :: (HasGame m) => m Natural
getAccelerationCount = getAccelerationCountIn . areaId =<< getYourArea

-- | The acceleration tokens next to the area's main scheme, which stay as it leaves play
-- (RR, Acceleration Token), and the area's acceleration icons.
-- | Physical acceleration tokens, excluding printed and gained icons.
getAccelerationTokenCount :: (HasGame m) => m Natural
getAccelerationTokenCount = getsGame \g ->
  sum (map areaAccelerationTokens (gameAreas g))
    + sum [mainSchemeAccelerationTokens (toAttrs scheme) | scheme <- HashMap.elems (gameMainSchemes g)]
    + sum [sideSchemeAccelerationTokens (toAttrs scheme) | scheme <- HashMap.elems (gameSideSchemes g)]
    + sum (map Minion.getMinionAccelerationTokens $ HashMap.elems $ gameMinions g)

getAccelerationCountIn :: (HasGame m) => AreaId -> m Natural
getAccelerationCountIn aid = getsGame \g ->
  sum [areaAccelerationTokens a | a <- gameAreas g, areaId a == aid] + accelerationIconsIn g aid
    + sum (map (mainSchemeAccelerationTokens . toAttrs) (inArea g aid MainSchemeRef (gameMainSchemes g)))
    + sum (map Minion.getMinionAccelerationTokens (inArea g aid MinionRef (gameMinions g)))

-- | The acceleration icons in the area, on its schemes, villains and obligations.
accelerationIconsIn :: Game -> AreaId -> Natural
accelerationIconsIn g aid =
  let (_, acceleration, _, _) = encounterIconCountsIn g aid
   in acceleration + sum (map (sideSchemeAccelerationTokens . toAttrs) (inArea g aid SideSchemeRef (gameSideSchemes g)))

getIdentityAttackAgainst :: (HasGame m) => IdentityId -> EnemyId -> m Natural
getIdentityAttackAgainst ident enemy = do
  player <- getIdentity ident
  case currentIdentity player of
    HeroSide hero -> Hero.getModifiedAttackAgainst (toAttrs hero) enemy
    AlterEgoSide _ -> pure 0

-- | Whether an attachment still has a friendly character as its live host. A host
-- leaving play ceases to be protected before its attached cards are discarded.
attachmentHasFriendlyHost :: (HasGame m) => AttachmentId -> m Bool
attachmentHasFriendlyHost aid = getsGame $ \g -> case lookup aid (gameAttachments g) of
  Nothing -> False
  Just att -> let a = toAttrs att in
    maybe False (`HashMap.member` gamePlayers g) (Attachment.attachmentIdentity a)
      || maybe False (`HashMap.member` gameAllies g) (Attachment.attachmentAlly a)

-- | Test meaningful exhaust-cost assignments using only the players currently
-- allowed to pay (including the Peril restriction), before beginning payment.
eventExhaustionsAffordable :: (HasGame m) => IdentityId -> [IdentityId] -> PlayerCard -> Cost -> m Bool
eventExhaustionsAffordable payer payers card cost
  | null traits = pure True
  | otherwise = do
      candidates <- traverse (\trait -> concatMapM (characters trait) payers) traits
      assign [] candidates
 where
  traits = [trait | ExhaustCharacterWithTraitCost trait <- flattenCosts cost]
  characters trait who = (<>)
    <$> selectMap IdentityCharacter (IdentityWithId who <> UnexhaustedIdentity <> IdentityWithTrait trait)
    <*> selectMap AllyCharacter (UnexhaustedAlly <> AllyControlledBy (IdentityWithId who) <> AllyWithTrait trait)
  assign used [] = eventExhaustionsValid payer card (reverse used)
  assign used (xs : rest) = anyM (\character ->
    if character `elem` used then pure False else assign (character : used) rest) xs

-- | The counter is state on the scheme, so player effects can still choose any scheme.
getGliderSchemes :: (HasGame m) => m [MainSchemeId]
getGliderSchemes = getsGame $ map fst . filter (mainSchemeHasGlider . toAttrs . snd) . HashMap.toList . gameMainSchemes

-- | Crisis and patrol restrict the glider scheme in Venom Goblin.
isRestrictedMainScheme :: (HasGame m) => MainSchemeId -> m Bool
isRestrictedMainScheme sid = do
  gliders <- getGliderSchemes
  pure $ null gliders || sid `elem` gliders

getMainSchemeAccelerationTokensIn :: (HasGame m) => AreaId -> m Natural
getMainSchemeAccelerationTokensIn aid = getsGame $ sum . map (mainSchemeAccelerationTokens . toAttrs) . inAreaForTokens
 where
  inAreaForTokens g = inArea g aid MainSchemeRef (gameMainSchemes g)

-- | Physical acceleration tokens on or next to the main scheme, excluding icons
-- and tokens on side schemes. Ordinary tokens live on the area; effects that name
-- a particular scheme can also put tokens directly on that scheme.
getMainSchemeTokenCountIn :: (HasGame m) => AreaId -> m Natural
getMainSchemeTokenCountIn aid = do
  adjacent <- getsGame $ sum . map areaAccelerationTokens . filter ((== aid) . areaId) . gameAreas
  (adjacent +) <$> getMainSchemeAccelerationTokensIn aid

getMainSchemeTokenCount :: (HasGame m) => m Natural
getMainSchemeTokenCount = getMainSchemeTokenCountIn . areaId =<< getYourArea

getAccelerationTokensOnMainScheme :: (HasGame m) => MainSchemeId -> m Natural
getAccelerationTokensOnMainScheme sid = do
  aid <- getAreaOfTarget sid
  getsGame \g ->
    sum [areaAccelerationTokens area | area <- gameAreas g, areaId area == aid]
      + maybe 0 (mainSchemeAccelerationTokens . toAttrs) (lookup sid $ gameMainSchemes g)

getRemovedFromGameEncounterCards :: (HasGame m) => m [EncounterCard]
getRemovedFromGameEncounterCards = getsGame gameRemovedFromGameEncounterCards

-- | In-play controlled cards matching the printed card, and legally discardable.
discardableControlledCardsMatching :: (HasGame m) => IdentityId -> CardMatcher -> m [Target]
discardableControlledCardsMatching who matcher = do
  allies <- selectList (AllyControlledBy $ IdentityWithId who)
  supports <- selectList (SupportControlledBy $ IdentityWithId who)
  upgrades <- selectList (UpgradeControlledBy $ IdentityWithId who)
  schemes <- getControlledPlayerSideSchemes who False
  filterM (\target -> do
    card <- getsGame (`cardInPlay` target)
    modifiers <- getModifiers target
    pure $ maybe False (\c -> cardMatch matcher c && Permanent `notElem` cdKeywords (getCardDef c)) card
      && CannotBeDiscarded `notElem` modifiers && CannotLeavePlay `notElem` modifiers && CannotLeavePlayByCardAbilities `notElem` modifiers)
    (map AllyRef allies <> map SupportRef supports <> map UpgradeRef upgrades <> map SideSchemeRef schemes)

blankIn :: Game -> Target -> Bool
blankIn g target = runIdentity $ runReaderT (isBlanked target) g

-- | Dynamic icons count both in the public board and during the villain phase.
iconModifiersIn :: Game -> AreaId -> [[Modifier]]
iconModifiersIn g aid = runIdentity $ runReaderT (for local $ \target -> do
  mods <- getModifiers target
  pure $ if SuppressSchemeIcons `elem` mods then [] else mods) g
 where
  local = filter (\ref -> (areaId <$> areaOfTarget g ref) == Just aid) $
    map IdentityRef (HashMap.keys $ gamePlayers g)
    <> map VillainRef (HashMap.keys $ gameVillains g)
    <> map MinionRef (HashMap.keys $ gameMinions g)
    <> map AllyRef (HashMap.keys $ gameAllies g)
    <> map SideSchemeRef (HashMap.keys $ gameSideSchemes g)
    <> map MainSchemeRef (HashMap.keys $ gameMainSchemes g)
    <> map AttachmentRef (HashMap.keys $ gameAttachments g)
    <> map EnvironmentRef (HashMap.keys $ gameEnvironments g)
    <> map SupportRef (HashMap.keys $ gameSupports g)
    <> map UpgradeRef (HashMap.keys $ gameUpgrades g)
    <> map ObligationRef (HashMap.keys $ gameObligations g)

-- | A replacement consumes one attachment, before the attack starts. Status
-- replacements are checked by the caller first. Other copies wait for another attack.
replaceHeroAttackWithDiscardAndStun :: (HasGame m, HasQueue m, MonadThrow m, MonadRandom m) => IdentityId -> m Bool
replaceHeroAttackWithDiscardAndStun who = runQueueT do
  hero <- identityMatches HeroIdentity who
  modifiers <- getModifiers (IdentityRef who)
  gripped <- replaceAttackOrThwartWithCounterRemoval who
  let targets = [(t,True) | ReplaceHeroAttackWithDiscardAndStun t <- modifiers, hero]
        <> [(t,False) | ReplacePowerWithDiscard t <- modifiers]
  if gripped then pure True else if null targets then pure False else do
    chooseOrRunOne who [targeting target (removeFromPlay target >> when stunAfter (stunIdentity who)) | (target,stunAfter) <- targets]
    pure True

-- | A consumable attachment replaces one power after status-card replacements.
replacePowerWithDiscard :: (HasGame m, HasQueue m, MonadThrow m, MonadRandom m) => IdentityId -> m Bool
replacePowerWithDiscard you = runQueueT do
  modifiers <- getModifiers (IdentityRef you)
  let targets = [target | ReplacePowerWithDiscard target <- modifiers]
  unless (null targets) $ chooseOrRunOne you [targeting target (removeFromPlay target) | target <- targets]
  pure $ not (null targets)

characterDamageNow :: (HasGame m) => CharacterId -> m Natural
characterDamageNow = \case
  IdentityCharacter p -> getsGame (maybe 0 (playerIdentityDamage . toAttrs) . lookup p . gamePlayers)
  AllyCharacter a -> getsGame (maybe 0 getAllyDamage . lookup a . gameAllies)
  _ -> pure 0

-- | Defending lasts from declaration (including a defense event) until the attack ends.
isIdentityDefending :: (HasGame m) => IdentityId -> m Bool
isIdentityDefending ident = getsGame $ \g -> any defending
  (mapMaybe villainAttackDetails (HashMap.elems $ gameVillains g) <> mapMaybe minionAttackDetails (HashMap.elems $ gameMinions g))
 where
  defending a = attackCharacter a == IdentityCharacter ident && attackDefended a

getZoneHeldCards :: (HasGame m) => Target -> m [PlayerCard]
getZoneHeldCards target = getsGame $ \g -> case target of
  MinionRef mid -> maybe [] (Minion.minionHeldCards . toAttrs) (lookup mid $ gameMinions g)
  AllyRef aid -> maybe [] (Ally.allyHeldCards . toAttrs) (lookup aid $ gameAllies g)
  SupportRef sid -> maybe [] (supportHeldCards . toAttrs) (lookup sid $ gameSupports g)
  UpgradeRef uid -> maybe [] (upgradeHeldCards . toAttrs) (lookup uid $ gameUpgrades g)
  SideSchemeRef sid -> maybe [] (sideSchemeHeldCards . toAttrs) (lookup sid $ gameSideSchemes g)
  MainSchemeRef sid -> maybe [] (mainSchemeHeldCards . toAttrs) (lookup sid $ gameMainSchemes g)
  EnvironmentRef eid -> maybe [] (MGEnvironment.environmentHeldCards . toAttrs) (lookup eid $ gameEnvironments g)
  _ -> []

modifyEnemyAttack :: EnemyId -> (Attack -> Attack) -> Game -> Game
modifyEnemyAttack enemy f g = case enemy of
  EnemyAttachmentId _ -> g
  EnemyVillainId vid -> g & entitiesL . villainsL . ix vid %~ (\(Villain.Villain v) -> Villain.Villain $ Villain.overVillainAttrs (\a -> a {Villain.villainAttacking = f <$> Villain.villainAttacking a}) v)
  EnemyMinionId mid -> g & entitiesL . minionsL . ix mid %~ Minion.overMinion (\a -> a {Minion.minionAttacking = f <$> Minion.minionAttacking a})

getEnemyKeywords :: HasGame m => EnemyId -> m [Keyword]
getEnemyKeywords enemy = case enemy of
  EnemyAttachmentId _ -> pure []
  EnemyMinionId mid -> getsGame (lookup mid . gameMinions) >>= maybe (pure []) (Minion.getModifiedKeywords . toAttrs)
  EnemyVillainId vid -> getsGame (lookup vid . gameVillains) >>= maybe (pure []) (Villain.getModifiedKeywords . toAttrs)

-- | The enemy's modified ATK outside any particular attack.
getEnemyAttackValue :: HasGame m => EnemyId -> m Natural
getEnemyAttackValue = \case
  EnemyAttachmentId _ -> pure 0
  EnemyMinionId mid -> getsGame (lookup mid . gameMinions) >>= maybe (pure 0) (Minion.getModifiedAttack . toAttrs)
  EnemyVillainId vid -> getsGame (lookup vid . gameVillains) >>= maybe (pure 0) (Villain.getModifiedAttack . toAttrs)

getEnemyScheme :: HasGame m => EnemyId -> m Natural
getEnemyScheme enemy = case enemy of
  EnemyAttachmentId _ -> pure 0
  EnemyMinionId mid -> getsGame (lookup mid . gameMinions) >>= maybe (pure 0) (Minion.getModifiedScheme . toAttrs)
  EnemyVillainId vid -> getsGame (lookup vid . gameVillains) >>= maybe (pure 0) (Villain.getModifiedScheme . toAttrs)

-- | The defined base, distinct from a printed star and from additive modifiers.
-- A removed minion uses the snapshot taken immediately before its departure.
getEnemyBaseScheme :: HasGame m => EnemyId -> m Natural
getEnemyBaseScheme = \case
  EnemyAttachmentId _ -> pure 0
  EnemyVillainId vid -> do
    printed <- getEnemyPrintedScheme (EnemyVillainId vid)
    modifiers <- getModifiers (VillainRef vid)
    pure $ fromMaybe printed (headMay [n | BaseScheme n <- modifiers])
  EnemyMinionId mid -> do
    live <- getsGame (lookup mid . gameMinions)
    case live of
      Just minion -> Minion.minionBaseScheme (toAttrs minion)
      Nothing -> do
        removed <- getsGame (lookup mid . entitiesMinions . gameRemovedEntities)
        pure $ case removed of
          Just minion -> let attrs = toAttrs minion in
            fromMaybe (unSch $ Minion.minionScheme attrs) (Minion.minionDefeatedBaseScheme attrs)
          Nothing -> 0

getEnemyPrintedScheme :: HasGame m => EnemyId -> m Natural
getEnemyPrintedScheme enemy = getsGame $ \g -> case enemy of
  EnemyAttachmentId _ -> 0
  EnemyVillainId vid -> maybe 0 (unSch . Villain.villainScheme . toAttrs) (lookup vid $ gameVillains g)
  EnemyMinionId mid -> maybe 0 (unSch . Minion.minionScheme . toAttrs) (lookup mid (gameMinions g) <|> lookup mid (entitiesMinions $ gameRemovedEntities g))

getEnemyPrintedAttack :: (HasGame m) => EnemyId -> m Natural
getEnemyPrintedAttack enemy = getsGame $ \g -> case enemy of
  EnemyAttachmentId _ -> 0
  EnemyVillainId vid -> maybe 0 (unAtk . Villain.villainAttack . toAttrs) (lookup vid $ gameVillains g)
  EnemyMinionId mid -> maybe 0 (unAtk . Minion.minionAttack . toAttrs) (lookup mid $ gameMinions g)

getAttachmentIdentity :: (HasGame m) => AttachmentId -> m (Maybe IdentityId)
getAttachmentIdentity aid = getsGame $ \g -> Attachment.attachmentIdentity . toAttrs =<< lookup aid (gameAttachments g)

-- | Only parenthetical ability labels designate a triggered attack/defense/thwart;
-- printed traits alone never do. Basic powers handle their own status cards.
labeledAbilityTypes :: Game -> Ability -> [Ability.AbilitySubType]
labeledAbilityTypes g ab
  | abilityType ab == Basic || engineAttackResponse ab = []
  | otherwise = nub $ maybe [] cdAbilitySubTypes (abilityCardDef g $ refocus $ abilitySource ab)
      <> maybeToList (abilitySubType ab)

-- | A canceled ability still returns to the window which offered it. Its card
-- effects, including effects which remove a consumable card, do not resolve.
isAbilityWindowContinuation :: Choice -> Bool
isAbilityWindowContinuation = \case
  Run [CheckWindowsFor _ _] -> True
  _ -> False

cancelLabeledAbility :: ReverseQueue m => IdentityId -> [Ability.AbilitySubType] -> m Bool
cancelLabeledAbility you labels = do
  stunned <- identityMatches StunnedIdentity you
  confused <- identityMatches ConfusedIdentity you
  let stops = Ability.Attack `elem` labels && stunned
      baffles = Ability.Thwart `elem` labels && confused
  when stops $ removeStun you
  when baffles $ removeConfused you
  if stops || baffles then pure True
  else if any (`elem` labels) [Ability.Attack,Ability.Thwart] then do
    replaced <- replaceAttackOrThwartWithCounterRemoval you
    if replaced then pure True
    else if Ability.Attack `elem` labels then replaceHeroAttackWithDiscardAndStun you else replacePowerWithDiscard you
  else if Ability.Attack `elem` labels then replaceHeroAttackWithDiscardAndStun you
  else if Ability.Thwart `elem` labels then replacePowerWithDiscard you
  else pure False

-- | Resolving a defense-labeled ability is a defense, without applying basic DEF.
-- An ally already defending remains the defender. Generic defend interrupts open
-- for a defending identity, including one already defending this attack.
beginLabeledDefense :: ReverseQueue m => IdentityId -> m ()
beginLabeledDefense you = do
  g <- getGame
  let attacks = mapMaybe villainAttackDetails (HashMap.elems $ gameVillains g)
        <> mapMaybe minionAttackDetails (HashMap.elems $ gameMinions g)
      eligible attack = not (attackDefended attack) || case attackCharacter attack of
        IdentityCharacter who -> who == you
        AllyCharacter aid -> maybe False ((== you) . getAllyController) (lookup aid $ gameAllies g)
        _ -> False
  for_ (headMay $ filter eligible attacks) \attack -> do
    let enemy = attackEnemy attack
    blocked <- elem AttacksCannotBeDefended <$> getModifiers (enemyRef enemy)
    unless (blocked || isDefendingOtherSimultaneousAttack you g) do
      unless (attackDefended attack) $ enemyDefendedBy enemy (IdentityCharacter you)
      when (not (attackDefended attack) || attackCharacter attack == IdentityCharacter you) $
        checkWindows [W.Window W.When $ W.HeroDefends you enemy]

-- | A defense interrupt may precede the basic defense declared by another event.
usesAttackForDefense :: HasGame m => IdentityId -> EnemyId -> m Bool
usesAttackForDefense you enemy = getsGame $ \g ->
  let details = case enemy of
        EnemyVillainId vid -> lookup vid (gameVillains g) >>= villainAttackDetails
        EnemyMinionId mid -> lookup mid (gameMinions g) >>= minionAttackDetails
        EnemyAttachmentId _ -> Nothing
   in maybe False (\a -> attackCharacter a == IdentityCharacter you && attackUsesAttackForDefense a == Just you) details

canMoveSchemeThreatFrom :: (HasGame m, IsRef source) => source -> SchemeId -> m Bool
canMoveSchemeThreatFrom source scheme = do
  exists <- not . null <$> selectList (SchemeWithId scheme)
  modifiers <- getModifiers (schemeRef scheme)
  controller <- getSourceController (toSource source)
  let allowedByController = all (\case
        ThreatRemovableOnlyBy who -> maybe True (== who) controller
        ThreatRemovableOnlyByIdentity who -> controller == Just who
        _ -> True) modifiers
  blockedByCrisis <- case scheme of
    SchemeMainSchemeId sid -> do
      crisis <- hasCrisisInYourArea
      restricted <- isRestrictedMainScheme sid
      ownIgnores <- elem IgnoresCrisis <$> getModifiers source
      inheritedIgnores <- case toSource source of
        AllyRef _ -> pure False
        _ -> maybe (pure False) (fmap (elem IgnoresCrisis) . getModifiers . IdentityRef) controller
      let playerCard = case toSource source of
            IdentityRef _ -> True
            AllyRef _ -> True
            EventRef _ -> True
            SupportRef _ -> True
            UpgradeRef _ -> True
            _ -> False
      pure $ crisis && restricted && playerCard && not (ownIgnores || inheritedIgnores)
    _ -> pure False
  pure $ exists && ThreatCannotBeRemoved `notElem` modifiers && allowedByController && not blockedByCrisis

-- | Resolving a Special needs a potential game effect (RR Resolve). Blanking an
-- already blank non-Elite minion still establishes a new lasting effect.
weatherSpecialCanResolve :: (HasGame m) => IdentityId -> SupportId -> m Bool
weatherSpecialCanResolve you sid = do
  card <- getsGame (\g -> cardInPlay g (SupportRef sid))
  blanked <- isBlanked sid
  case if blanked then Nothing else toCardCode <$> card of
    Just "36002" -> do
      p <- getIdentity you
      let a = toAttrs p
      pure $ not (null (unDeck $ playerIdentityDeck a) && null (unDiscard $ playerIdentityDiscard a))
    Just "36003" -> do
      schemes <- selectList (SchemeWithThreat $ GreaterThan $ Static 0)
      anyM (canMoveSchemeThreatFrom sid) schemes
    Just "36004" -> selectAny DamageableEnemy
    Just "36005" -> selectAny (NotMinion $ MinionWithTrait Elite)
    _ -> pure False

-- | Threat on a character is distinct from its damage and other counters.
getCharacterThreat :: (HasGame m) => CharacterId -> m Natural
getCharacterThreat character = getsGame (fromMaybe 0 . Data.List.lookup character . gameCharacterThreat)

-- | MojoMania moves threat when a character flips or leaves play.
transferMojoThreat :: (HasGame m, HasQueue m, MonadRandom m, MonadThrow m) => CharacterId -> m ()
transferMojoThreat character = runQueueT do
  active <- getsGame (any ((== "39025b") . toCardCode . getCardDef) . HashMap.elems . gameMainSchemes)
  amount <- getCharacterThreat character
  when (active && amount > 0) $ moveCharacterThreatToMainScheme character

-- | Check a searched card's complete play legality before offering its free play.
canPlayCardIgnoringResourceCostFromAnyZone :: (HasGame m, HasQueue m) => IdentityId -> PlayerCard -> m Bool
canPlayCardIgnoringResourceCostFromAnyZone ident card = do
  g <- getGame
  let withCard = g & entitiesL . playersL . ix ident %~ \(PlayerIdentity attrs) ->
        PlayerIdentity attrs {playerIdentityHand = Hand $ card : filter (/= card) (unHand $ playerIdentityHand attrs)}
  runReaderT (canPlayCardIgnoringResourceCost ident card) withCard

-- | Evaluate continuous card modifiers while the card is being revealed.
getRevealingCardModifiers :: (HasGame m, IsRef a, IsCard a) => a -> m [Modifier]
getRevealingCardModifiers card = do
  g <- getGame
  runReaderT (getModifiers card) g {gameFocusedCards = toCard card : gameFocusedCards g}

-- | Actual departures, including the continuation after a leave-play window.
departingThreatCharacter :: Message -> Maybe CharacterId
departingThreatCharacter message = target message >>= \case
  IdentityRef who -> Just $ IdentityCharacter who
  AllyRef who -> Just $ AllyCharacter who
  VillainRef who -> Just $ VillainCharacter who
  MinionRef who -> Just $ MinionCharacter who
  _ -> Nothing
 where
  target = \case
    RemoveFromPlay ref -> Just ref
    RemoveFromPlayAfterWindow ref -> Just ref
    RemoveFromGame ref -> Just ref
    AddToVictoryDisplay ref -> Just ref
    ReturnToHand ref -> Just ref
    _ -> Nothing


-- | Printed THW with current modifiers, for abilities such as Askani'son.
characterThwartValue :: HasGame m => CharacterId -> m Natural
characterThwartValue = \case
  IdentityCharacter ident -> currentIdentity <$> getIdentity ident >>= \case
    HeroSide hero -> Hero.getModifiedThwart (toAttrs hero)
    AlterEgoSide _ -> pure 0
  AllyCharacter aid -> getsGame (lookup aid . gameAllies) >>= maybe (pure 0) (Ally.getModifiedThwart . toAttrs)
  _ -> pure 0

-- | Check the upgrade's actual legal targets, including its attachment limits.
canAttachPlayerUpgradeToAlly :: HasGame m => AllyId -> PlayerCard -> m Bool
canAttachPlayerUpgradeToAlly aid card = do
  owner <- getsGame (fmap getAllyController . lookup aid . gameAllies)
  case owner of
    Nothing -> pure False
    Just you -> do
      case createUpgrade you card of
        Upgrade upgrade -> do
          targets <- withMissionTargets $ upgradeTargets upgrade
          uniques <- getUniqueTitlesInPlay
          let def = getCardDef card
              clash = cdUnique def && any (uniqueMatch (title $ cdName def, maybe [] pure $ subtitle $ cdName def)) uniques
          pure $ not clash && maybe False (elem $ AllyRef aid) targets


canPlayerRemoveThreatFrom :: HasGame m => IdentityId -> SchemeId -> m Bool
canPlayerRemoveThreatFrom you scheme = do
  modifiers <- getModifiers (IdentityRef you)
  pure $ all (\case
    CanOnlyRemoveThreatFrom allowed -> allowed == scheme
    CannotRemoveThreatFrom blocked -> blocked /= scheme
    _ -> True) modifiers

canRemoveThreatFrom :: HasGame m => Source -> SchemeId -> m Bool
canRemoveThreatFrom source scheme = do
  controller <- getSourceController source
  playerAllowed <- maybe (pure True) (`canPlayerRemoveThreatFrom` scheme) controller
  targetMods <- getModifiers (schemeRef scheme)
  character <- case source of
    IdentityRef you -> pure $ Just $ IdentityCharacter you
    AllyRef aid -> pure $ Just $ AllyCharacter aid
    EventRef _ -> fmap IdentityCharacter <$> getHeroSourceController source
    UpgradeRef uid -> do
      ally <- getsGame (fmap (upgradeAttachedAlly . toAttrs) . lookup uid . gameUpgrades)
      case join ally of
        Just aid -> pure $ Just $ AllyCharacter aid
        Nothing -> fmap IdentityCharacter <$> getHeroSourceController source
    _ -> pure Nothing
  activeCable <- case character of
    Just (IdentityCharacter you) -> selectAny (IdentityWithId you <> HeroIdentity)
    _ -> pure False
  heroOrAlly <- case character of
    Just (IdentityCharacter who) -> identityMatches HeroIdentity who
    Just (AllyCharacter _) -> pure True
    _ -> pure False
  let characterAllowed = (not heroOrAlly || ThreatCannotBeRemovedByHeroOrAlly `notElem` targetMods) && all (\case
        ThreatRemovableOnlyByCharacter allowed -> maybe True (\actual -> actual == allowed && activeCable) character
        _ -> True) targetMods
  sourceModifiers <- getModifiers source
  sourceDef <- getsGame (`abilityCardDef` refocus source)
  let isThwart = case source of IdentityRef _ -> True; AllyRef _ -> True; _ -> maybe False (elem Ability.Thwart . cdAbilitySubTypes) sourceDef
      characterThwarts = all (\case ThreatRemovableOnlyByCharacterThwarts allowed -> character == Just allowed && isThwart; ThreatRemovableOnlyByIdentity you -> character == Just (IdentityCharacter you); _ -> True) targetMods
      sourceAllowed = all (\case CannotThwartScheme forbidden -> forbidden /= scheme; _ -> True) sourceModifiers
  missionAllowed <- getsGame (\g -> not (isMissionTarget g $ schemeRef scheme) || missionSourceAllowed g source)
  pure $ missionAllowed && playerAllowed && characterAllowed && characterThwarts && sourceAllowed

canPlayerDamageEnemy :: HasGame m => IdentityId -> EnemyId -> m Bool
canPlayerDamageEnemy you enemy = do
  modifiers <- getModifiers (IdentityRef you)
  engaged <- case enemy of
    EnemyAttachmentId _ -> pure Nothing
    EnemyVillainId _ -> pure Nothing
    EnemyMinionId mid -> getsGame (fmap getMinionEngagedIdentity . lookup mid . gameMinions)
  pure $ all (\case
    CanOnlyDamageEngagedEnemies -> engaged == Just you
    CannotDamageEnemiesEngagedWith blocked -> engaged /= Just blocked
    _ -> True) modifiers

sourceRestrictedFromEnemy :: HasGame m => Source -> Target -> m Bool
sourceRestrictedFromEnemy source target = do
  controller <- getSourceController source
  let enemy = case target of
        VillainRef v -> Just $ EnemyVillainId v
        MinionRef m -> Just $ EnemyMinionId m
        EnemyRef e -> Just e
        CharacterRef (VillainCharacter v) -> Just $ EnemyVillainId v
        CharacterRef (MinionCharacter m) -> Just $ EnemyMinionId m
        _ -> Nothing
  case (controller, enemy) of
    (Just you, Just e) -> not <$> canPlayerDamageEnemy you e
    _ -> pure False


incomingEnemyDamage :: HasGame m => EnemyId -> Source -> Damage -> m Damage
incomingEnemyDamage enemy source original = do
  prevented <- attackDamagePreventedFrom source original
  if prevented then pure original {damageAmount = 0} else incoming
 where
  incoming = case enemy of
    EnemyAttachmentId _ -> pure $ if damageUnpreventable original then original else original {damageAmount = subtractNatural (damagePrevention original) (damageAmount original)}
    EnemyMinionId mid -> getsGame (lookup mid . gameMinions) >>= maybe (pure original {damageAmount = 0}) (\m -> Minion.incomingMinionDamage (toAttrs m) source original)
    EnemyVillainId vid -> do
      villain <- getsGame (lookup vid . gameVillains)
      case villain of
        Nothing -> pure original {damageAmount = 0}
        Just v -> do
          let attrs = toAttrs v
          modifiers <- getModifiers vid
          let reduced = Villain.reducedDamage modifiers original
              sustained = subtractNatural (fromIntegral $ unHp $ Villain.villainHp attrs) (fromIntegral $ unHp $ Villain.villainMaxHp attrs)
              n = minimum (damageAmount reduced : [subtractNatural sustained cap | MaximumSustainedDamage cap <- modifiers])
          pure $ if damageUnpreventable original then reduced else reduced {damageAmount = subtractNatural (damagePrevention original) n}

-- | Interrupts can change the amount still pending. A later interrupt answers
-- that updated amount, rather than the amount before the first interrupt.
refreshDamageInterruptWindow :: (HasGame m,HasQueue m) => W.Window -> m W.Window
refreshDamageInterruptWindow window = case W.windowType window of
  W.VillainDamageFrom vid source _ -> refresh (EnemyVillainId vid) (Just source) $ \case W.VillainDamageFrom {} -> True; _ -> False
  W.DamagedVillain vid _ -> refresh (EnemyVillainId vid) Nothing $ \case W.DamagedVillain {} -> True; _ -> False
  W.EnemyWouldBeDamaged enemy source _ -> refresh enemy (Just source) $ \case W.EnemyWouldBeDamaged {} -> True; _ -> False
  _ -> pure window
 where
  refresh enemy source sameKind = do
    pending <- withQueue $ \queue -> (queue,headMay
      [message | message <- concatMap queuedMessageLeaves queue,Just (target,from,_) <- [enemyDamageMessage message],target == enemy,maybe True (== from) source])
    case pending of
      Nothing -> pure window
      Just message -> do
        current <- enemyDamageTakenWindows message
        pure $ fromMaybe window $ find (sameKind . W.windowType) current


canSourceDamageEnemy :: HasGame m => Source -> EnemyId -> m Bool
canSourceDamageEnemy source enemy = getSourceController source >>= maybe (pure True) (`canPlayerDamageEnemy` enemy)


cleanUpAttachedMinions :: (HasGame m, HasQueue m, MonadThrow m, MonadRandom m) => Game -> Game -> m Game
cleanUpAttachedMinions before after = runQueueT do
  for_ (HashMap.toList $ gameMinions before) $ \(mid, minion) ->
    for_ (Minion.minionAttachedTo $ toAttrs minion) $ \host -> do
      let sourcePresent = HashMap.member mid (gameMinions after)
          hostWasPresent = HashMap.member host (gameMinions before)
            || HashMap.member (AllyId $ unMinionId host) (gameAllies before)
          hostPresent = HashMap.member host (gameMinions after)
          hostStillAlly = HashMap.member (AllyId $ unMinionId host) (gameAllies after)
      when (sourcePresent && hostWasPresent && not hostPresent && not hostStillAlly) $ removeFromPlay mid
      when (not sourcePresent && hostPresent) $ minionBecomesAlly host
  pure after

-- | The original target, before another character defended. A redirect before
-- declaration updates the already-stored attack and the forthcoming target together.
enemyAttackTargetsIdentity :: HasGame m => EnemyId -> IdentityId -> m Bool
enemyAttackTargetsIdentity enemy ident = do
  current <- getEnemyAttack enemy
  pure $ maybe True (\a -> fromMaybe (attackCharacter a) (attackTarget a) == IdentityCharacter ident) current

getEnemyAttack :: HasGame m => EnemyId -> m (Maybe Attack)
getEnemyAttack = \case
  EnemyAttachmentId _ -> pure Nothing
  EnemyVillainId vid -> getVillainAttack vid
  EnemyMinionId mid -> getsGame (\g -> (Minion.minionAttacking . toAttrs =<< lookup mid (gameMinions g)) <|> friendlyBoostAttackView g mid)


-- | Remaining hit points used for highest/lowest target comparisons.
getEnemyRemainingHitPoints :: HasGame m => EnemyId -> m Natural
getEnemyRemainingHitPoints = \case
  EnemyAttachmentId aid -> getsGame $ maybe 0 (\a -> subtractNatural (Attachment.attachmentDamage $ toAttrs a) 3) . lookup aid . gameAttachments
  EnemyVillainId vid -> getsGame $ maybe 0 (fromIntegral . max 0 . unHp . Villain.villainHp . toAttrs) . lookup vid . gameVillains
  EnemyMinionId mid -> getsGame (lookup mid . gameMinions) >>= \case
    Nothing -> pure 0
    Just minion -> do
      hp <- Minion.getModifiedHitPoints (toAttrs minion)
      pure $ subtractNatural (Minion.minionDamage $ toAttrs minion) hp

-- | Count physical status cards, including a steady character's second card.
getCharacterStatusCount :: HasGame m => CharacterId -> StatusCard -> m Natural
getCharacterStatusCount character status = getsGame $ \g ->
  let counts stunned additionalStun confused additionalConfuse tough extra = case status of
        StunnedStatus -> (if stunned then 1 else 0) + (if additionalStun then 1 else 0)
        ConfusedStatus -> (if confused then 1 else 0) + (if additionalConfuse then 1 else 0)
        ToughStatus -> (if tough then 1 else 0) + extra
   in case character of
     AttachmentCharacter _ -> 0
     IdentityCharacter you -> maybe 0 (\p -> let a = toAttrs p in counts (playerIdentityStunned a) (playerIdentityAdditionalStun a) (playerIdentityConfused a) (playerIdentityAdditionalConfuse a) (playerIdentityTough a) (playerIdentityAdditionalTough a)) (lookup you $ gamePlayers g)
     AllyCharacter aid -> maybe 0 (\ally -> let a = toAttrs ally in counts (Ally.allyStunned a) (Ally.allyAdditionalStun a) (Ally.allyConfused a) (Ally.allyAdditionalConfuse a) (Ally.allyTough a) (Ally.allyAdditionalTough a)) (lookup aid $ gameAllies g)
     MinionCharacter mid -> maybe 0 (\minion -> let a = toAttrs minion in counts (Minion.minionStunned a) (Minion.minionAdditionalStun a) (Minion.minionConfused a) (Minion.minionAdditionalConfuse a) (Minion.minionTough a) (Minion.minionAdditionalTough a)) (lookup mid $ gameMinions g)
     VillainCharacter vid -> maybe 0 (\villain -> let a = toAttrs villain in counts (Villain.villainStunned a) (Villain.villainAdditionalStun a) (Villain.villainConfused a) (Villain.villainAdditionalConfuse a) (Villain.villainTough a) 0) (lookup vid $ gameVillains g)


enemyDamageTakenWindows :: HasGame m => Message -> m [W.Window]
enemyDamageTakenWindows original = case enemyDamageMessage original of
  Nothing -> pure []
  Just (enemy, source, damage) -> do
    let target = enemyRef enemy
    present <- selectAny $ EnemyWithId enemy
    if not present then pure [] else do
      cannot <- case enemy of
        EnemyAttachmentId _ -> cannotTakeDamageFrom target source
        EnemyVillainId vid -> (||) <$> cannotTakeDamageFrom target source <*> villainPreventsAttackDamage vid source damage
        EnemyMinionId _ -> cannotTakeDamageFrom target source
      aerialAllowed <- canDamageThroughAerialRestriction target source damage
      let character = case enemy of EnemyVillainId vid -> VillainCharacter vid; EnemyMinionId mid -> MinionCharacter mid; EnemyAttachmentId aid -> AttachmentCharacter aid
      tough <- characterMatches (CharacterWithStatus ToughStatus) character
      defeated <- case enemy of
        EnemyAttachmentId _ -> pure False
        EnemyVillainId _ -> pure False
        EnemyMinionId _ -> do
          hp <- getCharacterRemainingHitPoints character
          modifiers <- getModifiers target
          pure $ hp == 0 && CannotBeDefeated `notElem` modifiers
      incoming <- if ((cannot || not aerialAllowed) && not (damageUnpreventable damage)) || defeated || (tough && preventedByTough damage)
        then pure damage {damageAmount = 0}
        else incomingEnemyDamage enemy source damage
      let specific = case enemy of
            EnemyAttachmentId _ -> []
            EnemyVillainId vid -> [W.Window W.When $ W.DamagedVillain vid incoming, W.Window W.When $ W.VillainDamageFrom vid source incoming]
            EnemyMinionId _ -> []
      pure $ W.Window W.When (W.EnemyWouldBeDamaged enemy source incoming) : specific


canDamageThroughAerialRestriction :: HasGame m => Target -> Source -> Damage -> m Bool
canDamageThroughAerialRestriction target source damage = do
  modifiers <- getModifiers target
  if OnlyAerialOrRangedAttacksCanDamage `notElem` modifiers then pure True else do
    let attacking = case damageSource damage of
          FromPlayerAttack _ -> True
          FromAllyAttack _ -> True
          FromAttack -> True
          FromOverkill -> True
          _ -> False
    attacker <- case damageSource damage of
      FromPlayerAttack you -> pure $ Just $ IdentityCharacter you
      FromAllyAttack aid -> pure $ Just $ AllyCharacter aid
      _ -> case source of
        AllyRef aid -> pure $ Just $ AllyCharacter aid
        IdentityRef you -> pure $ Just $ IdentityCharacter you
        MinionRef mid -> pure $ Just $ MinionCharacter mid
        VillainRef vid -> pure $ Just $ VillainCharacter vid
        _ -> fmap IdentityCharacter <$> getHeroSourceController source
    aerialAttacker <- maybe (pure False) (characterMatches $ CharacterWithTrait Aerial) attacker
    aerialCard <- getsGame $ maybe False (member Aerial . cdTraits) . (`abilityCardDef` refocus source)
    pure $ attacking && (damageRanged damage || aerialAttacker || aerialCard)

friendlyCharactersWithFewestRemainingHitPoints :: HasGame m => m [CharacterId]
friendlyCharactersWithFewestRemainingHitPoints = do
  players <- getPlayers
  identities <- for players $ \you -> do
    (remaining,_,_) <- getRemainingHitPoints you
    pure (IdentityCharacter you, remaining)
  allies <- getsGame $ filter ((`elem` players) . getAllyController) . HashMap.elems . gameAllies
  scored <- for allies $ \ally -> do
    hp <- Ally.getModifiedHitPoints (toAttrs ally)
    pure (AllyCharacter $ toId ally, subtractNatural (getAllyDamage ally) hp)
  let characters = identities <> scored
      smallest = minimum (map snd characters)
  pure [character | (character, remaining) <- characters, remaining == smallest]

characterToughCount :: HasGame m => CharacterId -> m Natural
characterToughCount = \case
  IdentityCharacter you -> getsGame $ maybe 0 (\p -> let a = toAttrs p in (if playerIdentityTough a then 1 else 0) + playerIdentityAdditionalTough a) . lookup you . gamePlayers
  AllyCharacter aid -> getsGame $ maybe 0 (\ally -> let a = toAttrs ally in (if Ally.allyTough a then 1 else 0) + Ally.allyAdditionalTough a) . lookup aid . gameAllies
  VillainCharacter vid -> getsGame $ maybe 0 (\v -> if villainIsTough v then 1 else 0) . lookup vid . gameVillains
  MinionCharacter mid -> getsGame $ maybe 0 (\m -> let a = toAttrs m in (if minionTough a then 1 else 0) + Minion.minionAdditionalTough a) . lookup mid . gameMinions
  AttachmentCharacter _ -> pure 0


getCharacterRemainingHitPoints :: HasGame m => CharacterId -> m Natural
getCharacterRemainingHitPoints = \case
  AttachmentCharacter aid -> getEnemyRemainingHitPoints (EnemyAttachmentId aid)
  IdentityCharacter you -> (\(remaining, _, _) -> remaining) <$> getRemainingHitPoints you
  AllyCharacter aid -> getsGame (lookup aid . gameAllies) >>= \case
    Nothing -> pure 0
    Just ally -> do
      hp <- Ally.getModifiedHitPoints (toAttrs ally)
      pure $ subtractNatural (Ally.allyDamage $ toAttrs ally) hp
  MinionCharacter mid -> getEnemyRemainingHitPoints (EnemyMinionId mid)
  VillainCharacter vid -> getEnemyRemainingHitPoints (EnemyVillainId vid)

-- | Player side schemes retain their controller in the villain play area.
getControlledPlayerSideSchemes :: HasGame m => IdentityId -> Bool -> m [SideSchemeId]
getControlledPlayerSideSchemes you onlyReady = getsGame $ \g ->
  [toId scheme | scheme <- HashMap.elems (gameSideSchemes g), let a = toAttrs scheme,
   maybe False ((== Just you) . pcController) (SideScheme.sideSchemePlayerCard a),
   not onlyReady || not (SideScheme.sideSchemeExhausted a)]

-- | Steady requires two matching status cards before the character is stunned/confused.
characterHasCondition :: HasGame m => StatusCard -> CharacterId -> m Bool
characterHasCondition status c = do
  n <- getCharacterStatusCount c status
  mods <- getModifiers (characterRef c)
  def <- getsGame (`abilityCardDef` characterRef c)
  let keywords = maybe [] (toList . cdKeywords) def <> [k | KeywordModifier k <- mods]
  pure $ n >= if Steady `elem` keywords && status /= ToughStatus then 2 else 1

-- | Metagame positions are stable, zero-based indices into the printed board.
upgradeSlotIsEmpty :: HasGame m => UpgradeId -> Int -> m Bool
upgradeSlotIsEmpty uid slot = getsGame $ \g -> case lookup uid (gameUpgrades g) of
  Nothing -> False
  Just upgrade -> let a = toAttrs upgrade in
    slot >= 0 && slot < length a.upgradeResourceSlots && slot `notElem` a.upgradeOccupiedSlots

-- | Whether one of I Got This's icon-dependent effects can change the game.
poolIconEffectsCanResolve :: HasGame m => IdentityId -> Source -> m Bool
poolIconEffectsCanResolve who source = asYou who do
  (crisis, acceleration, amplify, hazard) <- getEncounterIconCounts
  damage <- if crisis > 0 then selectAny (EnemyCanTakeDamageFrom source) else pure False
  threat <- if acceleration > 0
    then anyM (canRemoveThreatFrom source) =<< selectList ThwartableScheme
    else pure False
  ally <- if amplify > 0 then selectAny (AllyControlledBy (IdentityWithId who) <> AllyCanReady) else pure False
  player <- getIdentity who
  let a = toAttrs player
      draw = hazard > 0 && (notNull (unDeck a.playerIdentityDeck) || notNull (unDiscard a.playerIdentityDiscard))
  pure $ damage || threat || ally || draw

-- | Both sides of an Armed to the Teeth swap must be legal before its cost is
-- offered. A same-title swap keeps its existing host and does not leave play.
getLegalHeldWeaponSwaps ::
  (HasGame m, HasQueue m, MonadThrow m, Projection m PlayerIdentity) =>
  IdentityId -> UpgradeId -> m [(UpgradeId, PlayerCard)]
getLegalHeldWeaponSwaps who host = do
  held <- getsGame $ maybe [] (upgradeHeldCards . toAttrs) . lookup host . gameUpgrades
  weapons <- selectList (UpgradeControlledBy (IdentityWithId who) <> UpgradeWithTrait Weapon)
  pairs <- pure [(uid, card) | uid <- weapons, uid /= host, card <- held]
  filterM legal pairs
 where
  legal (uid, card) = do
    current <- getsGame $ lookup uid . gameUpgrades
    case current of
      Nothing -> pure False
      Just outgoing -> do
        let same = cdName (getCardDef outgoing) == cdName (getCardDef card)
        modifiers <- getModifiers outgoing
        uniqueConflict <- matchesUniqueInPlay who (UpgradeRef uid) (getCardDef card)
        limited <- case cdLimit (getCardDef card) of
          Just (MaxPerPlayer n) ->
            (>= n) . fromIntegral . length . filter (/= uid)
              <$> selectList (UpgradeControlledBy (IdentityWithId who) <> UpgradeNamed (cdName $ getCardDef card))
          _ -> pure False
        targets <- if same then pure Nothing else getPlayTargets who card
        let leaves = not same && (Permanent `elem` cdKeywords (getCardDef outgoing)
              || any (`elem` modifiers) [CannotLeavePlay, CannotLeavePlayByCardAbilities])
        pure $ not (uniqueConflict || limited || leaves) && maybe True notNull targets

getPreviousGameWon :: HasGame m => IdentityId -> m (Maybe Bool)
getPreviousGameWon who = getsGame $ \g -> playerIdentityPreviousGameWon . toAttrs =<< lookup who (gamePlayers g)

canResolvePlayerCardAbilities :: HasGame m => IdentityId -> m Bool
canResolvePlayerCardAbilities who = notElem CannotResolvePlayerCardAbilities <$> getModifiers (IdentityRef who)

-- | The attack captures its blanking property and completion request at declaration.
prepareEnemyAttack :: HasGame m => EnemyId -> Attack -> m Attack
prepareEnemyAttack enemy current = do
  modifiers <- getModifiers (enemyRef enemy)
  request <- getsGame $ fmap trackedAttackId . find ((== enemy) . trackedAttackEnemy) . gameTrackedEnemyAttacks
  pure current {attackBlanksDefender = AttacksBlankDefender `elem` modifiers, attackCompletionId = request}

attackBlanksTarget :: Game -> Target -> Bool
attackBlanksTarget g target = any ((== target) . characterRef) characters
 where
  active = mapMaybe (Villain.villainAttacking . toAttrs) (HashMap.elems $ gameVillains g)
    <> mapMaybe (Minion.minionAttacking . toAttrs) (HashMap.elems $ gameMinions g)
    <> mapMaybe trackedAttackPrevious (gameTrackedEnemyAttacks g)
    <> [attack' | Just group <- [gameSimultaneousMinionAttack g], (_, attack', _, _, _) <- simultaneousAttacks group]
  characters = [attackCharacter a | a <- active, attackBlanksDefender a, nonVillain $ attackCharacter a]
    <> [character | (_,character) <- gameBlankedAttackTargets g]
  nonVillain (VillainCharacter _) = False
  nonVillain _ = True

attackBlankingStarts :: HasGame m => [Window] -> m [(Source, CharacterId)]
attackBlankingStarts windows = do
  existing <- getsGame gameBlankedAttackTargets
  let announced = nub $ concat [case event of
        W.IdentityAttack who (EnemyMinionId mid) -> [(IdentityRef who, MinionCharacter mid)]
        W.FriendlyPlayerAttack who target -> [(IdentityRef who, target) | nonVillain target]
        W.FriendlyCharacterAttack attacker who -> [(refocus $ characterRef attacker, IdentityCharacter who)]
        W.EnemyAttackOnEnemy enemy (EnemyMinionId mid) -> [(refocus $ enemyRef enemy, MinionCharacter mid)]
        _ -> [] | W.Window W.When event <- windows]
      additional = [(IdentityRef who, MinionCharacter mid) | W.Window W.When (W.IdentityAttackTarget who (EnemyMinionId mid)) <- windows,
        (IdentityRef who, MinionCharacter mid) `notElem` (announced <> existing)]
  filterM (fmap (elem AttacksBlankDefender) . getModifiers . fst) (announced <> additional)
 where
  nonVillain (VillainCharacter _) = False
  nonVillain _ = True

clearCompletedAttackBlanking :: Game -> Game
clearCompletedAttackBlanking g = g {gameBlankedAttackTargets = foldl' (flip Data.List.delete) (gameBlankedAttackTargets g) ended}
 where
  ended = concat [case event of
    W.IdentityAttack who enemy -> [(IdentityRef who, toCharacter enemy)]
    W.FriendlyPlayerAttack who target -> [(IdentityRef who, target)]
    W.FriendlyCharacterAttack attacker who -> [(refocus $ characterRef attacker, IdentityCharacter who)]
    W.EnemyAttackOnEnemy enemy target -> [(refocus $ enemyRef enemy, toCharacter target)]
    _ -> [] | W.Window W.After event <- fromMaybe [] (headMay $ gameWindows g)]

captureEnemyAttackCompletion :: EnemyId -> Game -> Game
captureEnemyAttackCompletion enemy g = case attackNow of
  Just current | Just nonce <- attackCompletionId current -> g
    {gameTrackedEnemyAttacks = [if trackedAttackId tracked == nonce then tracked {trackedAttackResult = Just current} else tracked | tracked <- gameTrackedEnemyAttacks g]}
  _ -> g
 where
  attackNow = case enemy of
    EnemyAttachmentId _ -> Nothing
    EnemyVillainId vid -> Villain.villainAttacking . toAttrs =<< lookup vid (gameVillains g)
    EnemyMinionId mid -> Minion.minionAttacking . toAttrs =<< lookup mid (gameMinions g)

restoreEnemyAttack :: EnemyId -> Maybe Attack -> Game -> Game
restoreEnemyAttack enemy previous g = case enemy of
  EnemyAttachmentId _ -> g
  EnemyVillainId vid -> g & entitiesL . villainsL . ix vid %~ (\(Villain.Villain villain) -> Villain.Villain $ Villain.overVillainAttrs (\a -> a {Villain.villainAttacking = previous}) villain)
  EnemyMinionId mid -> g & entitiesL . minionsL . ix mid %~ Minion.overMinion (\a -> a {Minion.minionAttacking = previous})

controlsAspectCard :: HasGame m => IdentityId -> Aspect -> m Bool
controlsAspectCard who aspect = getsGame $ \g -> any ((== Just aspect) . cdAspect) $
  [getCardDef card | card <- HashMap.elems (gameAllies g), getAllyController card == who]
  <> [supportCardDef (toAttrs card) | card <- HashMap.elems (gameSupports g), getSupportController card == who]
  <> [getCardDef card | card <- HashMap.elems (gameUpgrades g), getUpgradeController card == who]
  <> [eventCardDef (toAttrs card) | card <- HashMap.elems (gameEvents g), eventController (toAttrs card) == who]
  <> [getCardDef card | scheme <- HashMap.elems (gameSideSchemes g), Just card <- [SideScheme.sideSchemePlayerCard $ toAttrs scheme], pcController card == Just who]

characterHasAttachment :: HasGame m => CharacterId -> CardDef -> m Bool
characterHasAttachment character def = getsGame $ \g -> any matches $ HashMap.elems (gameAttachments g)
 where
  matches attachment = let a = toAttrs attachment in sameCard (getCardDef attachment) def && case character of
    IdentityCharacter who -> Attachment.attachmentIdentity a == Just who
    AllyCharacter aid -> Attachment.attachmentAlly a == Just aid || Attachment.attachmentEnemy a == Just (EnemyMinionId $ MinionId $ unAllyId aid)
    MinionCharacter mid -> Attachment.attachmentEnemy a == Just (EnemyMinionId mid)
    VillainCharacter vid -> Attachment.attachmentEnemy a == Just (EnemyVillainId vid)
    AttachmentCharacter aid -> Attachment.attachmentEnemy a == Just (EnemyAttachmentId aid)

-- | The cards eligible for an explicit "play from hand" opportunity. This does
-- not make a deck card a resource source, a hand-discard cost or a put-into-play target.
getPlayableHandCards :: HasGame m => IdentityId -> m [PlayerCard]
getPlayableHandCards you = do
  echoCards <- echoReplayCards you
  player <- getIdentity you
  let attrs = toAttrs player
      hand = unHand (playerIdentityHand attrs)
  if playerIdentityTopDeckPlayUsed attrs || isJust (playerIdentityPendingTopDeckPlay attrs)
    then pure $ hand <> echoCards
    else do
      modifiers <- getModifiers player
      blanked <- isBlanked player
      let top = [card | PlayTopOfDeckAsHand `elem` modifiers, not blanked, card <- take 1 (unDeck $ playerIdentityDeck attrs)]
      pure $ hand <> top <> echoCards

isPlayableHandCard :: HasGame m => IdentityId -> PlayerCard -> m Bool
isPlayableHandCard you card = do
  player <- getIdentity you
  available <- getPlayableHandCards you
  pure $ card `elem` available || playerIdentityPendingTopDeckPlay (toAttrs player) == Just card

-- | An attack's cap follows its own metadata, including indirect shares and
-- overkill. Defending allies and a later attack retain their ordinary damage.
capIdentityAttackDamage :: HasGame m => IdentityId -> Source -> Damage -> m Damage
capIdentityAttackDamage you source damage
  | damageUnpreventable damage = pure damage
  | damageSource damage `notElem` [FromAttack, FromOverkill] = pure damage
  | Just enemy <- sourceEnemy source = do
      current <- getEnemyAttack enemy
      let limits = [subtractNatural taken cap | attack' <- maybeToList current, (who, cap, taken) <- attackIdentityDamageCaps attack', who == you]
      pure damage {damageAmount = minimum (damageAmount damage : limits)}
  | otherwise = pure damage

-- | The cap follows one attack and target and is applied after reductions and
-- prevention, so armor does not reduce an already-capped amount a second time.
capEnemyAttackDamageTaken :: HasGame m => EnemyId -> Source -> Damage -> m Damage
capEnemyAttackDamageTaken target source damage
  | damageUnpreventable damage = pure damage
  | damageSource damage `notElem` [FromAttack,FromOverkill] = pure damage
  | Just attacker <- sourceEnemy source = do
      current <- getEnemyAttack attacker
      let limits = [subtractNatural taken cap | a <- maybeToList current,(who,cap,taken) <- attackEnemyDamageCaps a,who == target]
      pure damage {damageAmount = minimum (damageAmount damage : limits)}
  | otherwise = pure damage

getMission :: HasGame m => m (Maybe MissionState)
getMission = getsGame (scenarioMission . toAttrs . gameScenario)

-- | Mission entities retain physical ownership, but are outside every player's area.
isMissionTarget :: Game -> Target -> Bool
isMissionTarget g target = maybe False (go target) (scenarioMission $ toAttrs $ g.gameScenario)
 where
  go ref m = case ref of
    AllyRef aid -> aid `elem` missionAllies m
    MinionRef mid -> mid `elem` missionMinions m
    SideSchemeRef sid -> sid == missionScheme m
    UpgradeRef uid -> maybe False (maybe False (`elem` missionAllies m) . upgradeAttachedAlly . toAttrs) (lookup uid $ gameUpgrades g)
    CharacterRef (AllyCharacter aid) -> go (AllyRef aid) m
    CharacterRef (MinionCharacter mid) -> go (MinionRef mid) m
    EnemyRef (EnemyMinionId mid) -> go (MinionRef mid) m
    SchemeRef (SchemeSideSchemeId sid) -> go (SideSchemeRef sid) m
    _ -> False

missionAlly :: Game -> Target -> Bool
missionAlly g target = case target of
  AllyRef _ -> isMissionTarget g target
  _ -> False

missionSourceAllowed :: Game -> Source -> Bool
missionSourceAllowed g source = maybe False (\m -> source == SideSchemeRef (missionScheme m)
  || maybe False ((`elem` ["45164", "45165", "45171a", "45179a", "45180a", "45181a", "45182a", "45183a"]) . toCardCode) (abilityCardDef g $ refocus source))
  (scenarioMission $ toAttrs $ g.gameScenario)

withMissionTargets :: HasGame m => ReaderT Game m a -> m a
withMissionTargets body = getGame >>= runReaderT body . (\g -> g {gameMissionTargets = True})

withMissionPlayDestination :: HasGame m => Bool -> ReaderT Game m a -> m a
withMissionPlayDestination destination body = getGame >>= runReaderT body . (\g -> g {gameMissionPlayDestination = Just destination})

missionDestinationPreview :: HasGame m => m Bool
missionDestinationPreview = getsGame (isJust . gameMissionPlayDestination)

missionAllyReduction :: HasGame m => PlayerCard -> m Natural
missionAllyReduction card = getsGame \g -> case scenarioMission (toAttrs $ g.gameScenario) of
  Just m | not (missionFinished m), missionDiscount m, cdCardType (getCardDef card) == AllyType
    , fromMaybe (missionPendingAlly m == Just (pcCardId card)) (gameMissionPlayDestination g) -> 2
  _ -> 0

modifyMissionInGame :: (MissionState -> MissionState) -> Game -> Game
modifyMissionInGame f g = g {gameScenario = case g.gameScenario of
  Scenario inner -> Scenario $ overScenarioAttrs (\a -> a {scenarioMission = f <$> scenarioMission a}) inner}

getSchemeThreatAmount :: HasGame m => SchemeId -> m Natural
getSchemeThreatAmount scheme = getsGame $ \g -> case scheme of
  SchemeSideSchemeId sid -> maybe 0 getSideSchemeThreat (lookup sid $ gameSideSchemes g)
  SchemeMainSchemeId mid -> maybe 0 getMainSchemeThreat (lookup mid $ gameMainSchemes g)

getMissionAllyStats :: HasGame m => AllyId -> m (Natural, Natural)
getMissionAllyStats aid = getsGame (lookup aid . gameAllies) >>= maybe (pure (0,0)) (\ally ->
  (,) <$> Ally.getModifiedThwart (toAttrs ally) <*> Ally.getModifiedAttack (toAttrs ally))

getAllyAttachedUpgrades :: HasGame m => AllyId -> m [UpgradeId]
getAllyAttachedUpgrades aid = getsGame (maybe [] (toList . allyUpgrades . toAttrs) . lookup aid . gameAllies)

pruneMissionEntities :: Game -> Game
pruneMissionEntities g = modifyMissionInGame (\m -> if missionSettingUp m then m else m
  {missionAllies = filter (`HashMap.member` gameAllies g) (missionAllies m)
  ,missionMinions = filter (`HashMap.member` gameMinions g) (missionMinions m)}) g

playerCardEntryAllowedIn :: Game -> PlayerCard -> Bool
playerCardEntryAllowedIn g card = not professorBlocked && not shapeBlocked
 where
  professorBlocked = title (cdName $ getCardDef card) == "Professor X"
    && maybe False ((== "45170a") . missionCard) (scenarioMission $ toAttrs $ g.gameScenario)
  shapeBlocked = Shapeshift `member` cdTraits (getCardDef card) && case pcController card <|> pcOwner card of
    Nothing -> False
    Just you -> maybe False ((== "56029b") . toCardCode) (lookup you $ gamePlayers g)
      && any (\u -> getUpgradeController u == you && Shapeshift `member` cdTraits (getCardDef u)) (HashMap.elems $ gameUpgrades g)

playerCardEntryAllowed :: HasGame m => PlayerCard -> m Bool
playerCardEntryAllowed card = do
  allowed <- getsGame (\g -> playerCardEntryAllowedIn g card)
  if not allowed then pure False else
    if toCardCode card `elem` ["56131","56135","56213","57034"] then case pcController card <|> pcOwner card of
      Nothing -> pure False
      Just you -> asYou you $ case createUpgrade you card of
        Upgrade upgrade -> do
          targets <- upgradeTargets upgrade
          maybe (pure True) (fmap notNull . filterM attachable) targets
    else pure True

-- | Find searches every eligible known zone, excluding facedown encounter hands,
-- the victory display and removed cards (RR Find).
findEncounterCards :: HasGame m => CardMatcher -> m [EncounterCard]
findEncounterCards matcher = getsGame \g ->
  let scenario = toAttrs (g.gameScenario)
      inPlay = concat
        [map toCard $ HashMap.elems $ gameMinions g, map toCard $ HashMap.elems $ gameAttachments g
        ,map toCard $ HashMap.elems $ gameSideSchemes g, map toCard $ HashMap.elems $ gameEnvironments g
        ,map toCard $ HashMap.elems $ gameMainSchemes g]
      playerCards = concat [unHand a.playerIdentityHand <> unDeck a.playerIdentityDeck <> unDiscard a.playerIdentityDiscard
        | p <- HashMap.elems (gamePlayers g), let a = toAttrs p]
        <> concatMap (Ally.allyHeldCards . toAttrs) (HashMap.elems $ gameAllies g)
        <> concatMap (Minion.minionHeldCards . toAttrs) (HashMap.elems $ gameMinions g)
        <> concatMap (supportHeldCards . toAttrs) (HashMap.elems $ gameSupports g)
        <> concatMap (upgradeHeldCards . toAttrs) (HashMap.elems $ gameUpgrades g)
        <> concatMap (Obligation.obligationHeldCards . toAttrs) (HashMap.elems $ gameObligations g)
        <> concatMap (sideSchemeHeldCards . toAttrs) (HashMap.elems $ gameSideSchemes g)
        <> concatMap (mainSchemeHeldCards . toAttrs) (HashMap.elems $ gameMainSchemes g)
      cards = onlyEncounterCards (scenarioSetAsideCards scenario <> gameCollection g <> inPlay)
        <> scenarioEncounterDeck scenario <> scenarioDiscard scenario
        <> concat [pileDeck pile <> pileDiscard pile | pile <- HashMap.elems $ scenarioDecks scenario]
        <> mapMaybe playerZoneEncounterCard playerCards
        <> concatMap (Minion.minionHeldTreacheries . toAttrs) (HashMap.elems $ gameMinions g)
        <> concatMap (Ally.allyHeldTreacheries . toAttrs) (HashMap.elems $ gameAllies g)
  in Data.List.nubBy (\a b -> ecCardId a == ecCardId b) $ filter (cardMatch matcher) cards

canResolveSettingSpecial :: HasGame m => IdentityId -> m Bool
canResolveSettingSpecial you = AoaSettings.currentSetting >>= \case
  Nothing -> pure False
  Just (eid, def) -> do
    blank <- isBlanked eid
    if blank then pure False else case toCardCode def of
      "45127" -> not . null . unDeck . playerIdentityDeck . toAttrs <$> getIdentity you
      "45133" -> selectAny EncounterMainScheme
      "45139" -> not <$> cannotTakeDamageFrom (IdentityRef you) (EnvironmentRef eid)
      _ -> pure False

-- The mission area's immunity includes continuous card effects. Attached upgrades
-- may modify their host; mission cards may affect their explicitly named area.
-- | Ordinary continuous effects cannot reach across the table. An upgrade on
-- the other leader, or an effect explicitly attached to that leader, lives with
-- its host and can modify that host normally.
modifierProviderAllowed :: Game -> Target -> Target -> Bool
modifierProviderAllowed g target provider = missionProviderAllowed g target provider &&
  (isNothing (gameCompetition g) || location target == location provider)
 where
  location GameRef = areaId <$> areaOfTarget g (IdentityRef $ fromMaybe (g.gameActivePlayer) (gameYou g))
  location ref = areaId <$> areaOfTarget g ref

missionProviderAllowed :: Game -> Target -> Target -> Bool
missionProviderAllowed g target provider
  | not (isMissionTarget g target) = True
  | provider == target = True
  | missionSourceAllowed g (refocus provider) = True
  | otherwise = case provider of
      UpgradeRef uid -> case target of
        AllyRef aid -> maybe False ((== Just aid) . upgradeAttachedAlly . toAttrs) (lookup uid $ gameUpgrades g)
        _ -> False
      EffectRef eid -> maybe False (missionProviderAllowed g target . refocus . Effect.effectSource . toAttrs) (lookup eid $ gameEffects g)
      _ -> False

getStartingHeroCodes :: HasGame m => m [CardCode]
getStartingHeroCodes = getsGame $ \g -> map (toHeroCardCode . toCardCode)
  (HashMap.elems (gamePlayers g) <> HashMap.elems (entitiesPlayers $ gameRemovedEntities g))

iconsSuppressedIn :: Game -> Target -> Bool
iconsSuppressedIn g target = runIdentity $ runReaderT (elem SuppressSchemeIcons <$> getModifiers target) g

-- | Defined base powers before additive modifiers. Gambit's X is defined by his
-- tucked card and is a base value (Rogue's v1.8 erratum).
getCharacterBasePowers :: HasGame m => CharacterId -> m (Natural, Natural)
getCharacterBasePowers = \case
  AllyCharacter aid -> getsGame $ \g -> case lookup aid (gameAllies g) of
    Nothing -> (0,0)
    Just ally -> let attrs = toAttrs ally in
      if toCardCode (Ally.allyCardDef attrs) == "48021"
        then let n = sum [boostCount $ cdBoostIcons $ getCardDef card | card <- Ally.allyHeldTreacheries attrs] in (n,n)
        else (unThw $ Ally.allyThwart attrs, unAtk $ Ally.allyAttack attrs)
  IdentityCharacter you -> getsGame $ \g -> case lookup you (gamePlayers g) of
    Just player -> case currentIdentity player of
      HeroSide hero -> let attrs = toAttrs hero in (unThw $ Hero.heroBaseThwart attrs, unAtk $ Hero.heroBaseAttack attrs)
      _ -> (0,0)
    _ -> (0,0)
  _ -> pure (0,0)

getCharacterThwart :: HasGame m => CharacterId -> m Natural
getCharacterThwart = \case
  AllyCharacter aid -> getsGame (lookup aid . gameAllies) >>= maybe (pure 0) (Ally.getModifiedThwart . toAttrs)
  IdentityCharacter you -> getsGame (lookup you . gamePlayers) >>= maybe (pure 0) getIdentityHeroThwart
  _ -> pure 0

getUpgradeAttachedEnemy :: HasGame m => UpgradeId -> m (Maybe EnemyId)
getUpgradeAttachedEnemy uid = getsGame $ \g -> Upgrade.upgradeAttachedEnemy . toAttrs =<< lookup uid (gameUpgrades g)

getMinionEngagedPlayer :: HasGame m => MinionId -> m (Maybe IdentityId)
getMinionEngagedPlayer mid = getsGame $ fmap (Minion.minionEngagedIdentity . toAttrs) . lookup mid . gameMinions

getAllyControllerId :: HasGame m => AllyId -> m (Maybe IdentityId)
getAllyControllerId aid = getsGame $ fmap getAllyController . lookup aid . gameAllies

-- | Conditional THW for the basic thwart against this scheme, before clamping.
getCharacterThwartAgainstScheme :: HasGame m => CharacterId -> SchemeId -> m Natural
getCharacterThwartAgainstScheme character scheme = do
  schemeModifiers <- getModifiers (schemeRef scheme)
  let grants = [(trait,n) | BasicThwartModifierForTrait trait n <- schemeModifiers]
  substitutes <- elem UseDefenseForAttackAndThwart <$> getModifiers (characterRef character)
  if null grants || substitutes then getCharacterThwart character else do
    bonus <- fmap sum $ for grants $ \(trait,n) -> do
      matches <- characterMatches (CharacterWithTrait trait) character
      pure $ if matches then n else 0
    (base, _) <- getCharacterBasePowers character
    modifiers <- getModifiers (characterRef character)
    let definedBase = fromMaybe base (headMay [n | BaseThwart n <- modifiers])
        signed = toInteger definedBase + sum [toInteger n | ThwartModifier n <- modifiers]
          - sum [toInteger n | ThwartPenalty n <- modifiers]
    pure $ fromInteger $ max 0 (signed + toInteger bonus)

-- | Raw powers and hit points before the ally leaves play, including a minion
-- converted into an ally. Modifiers and tucked-card definitions are not printed.
getAllyPrintedStats :: HasGame m => AllyId -> m (Natural, Natural, Natural)
getAllyPrintedStats aid = getsGame $ \g -> case lookup aid (gameAllies g) of
  Nothing -> (0,0,0)
  Just ally -> let attrs = toAttrs ally in (unHp $ Ally.allyHitPoints attrs, unThw $ Ally.allyThwart attrs, unAtk $ Ally.allyAttack attrs)

getShieldLog :: HasGame m => m Campaign.ShieldLog
getShieldLog = getsGame $ \g -> maybe (gameShieldLog g) (.campaignLog.shield) (g.gameCampaign)

allyDefeatedByConsequential :: HasGame m => AllyId -> m Bool
allyDefeatedByConsequential aid = getsGame $ maybe False (Ally.allyDefeatedByConsequential . toAttrs) . lookup aid . gameAllies

-- | An attachment explicitly attackable as a minion remains an attachment outside
-- that attack. Non-attack minion effects never discover these ids.
attackableAttachmentTargets :: HasGame m => EnemyMatcher -> m [EnemyId]
attackableAttachmentTargets matcher
  | not (relevant matcher) = pure []
  | otherwise = do
      attachments <- getsGame $ HashMap.elems . gameAttachments
      filterM (matches matcher) [EnemyAttachmentId (toId a) | a <- attachments, toCardCode (getCardDef a) == "54035"]
 where
  relevant = \case
    AttackableBy _ -> True
    AttackableByAlly _ -> True
    EnemyWithId (EnemyAttachmentId _) -> True
    EnemyMatches xs -> any relevant xs
    _ -> False
  matches m enemy@(EnemyAttachmentId aid) = case m of
    AnyEnemy -> pure True
    EnemyInPlay -> pure True
    MinionEnemy -> pure True
    DamageableEnemy -> pure True
    AttackableBy who -> do
      players <- selectList who
      anyM (\you -> permitsIceAttack aid enemy <$> getModifiers (IdentityRef you)) players
    AttackableByAlly ally -> permitsIceAttack aid enemy <$> getModifiers (AllyRef ally)
    EnemyWithId other -> pure $ other == enemy
    EnemyMatches xs -> allM (\x -> matches x enemy) xs
    NotEnemy x -> not <$> matches x enemy
    EnemyWithTitle name -> pure $ name == "Encased in Ice"
    EnemyIs def -> pure $ toCardCode def == "54035"
    EnemyCanTakeDamageFrom source -> not <$> cannotTakeDamageFrom (AttachmentRef aid) source
    _ -> pure False
  matches _ _ = pure False

-- | Eligible cards for Organizational Support, with their current printed icons.
organizationalSupportCandidates :: HasGame m => IdentityId -> m [(Target,[Resource])]
organizationalSupportCandidates you = do
  ownTraits <- getCharacterTraits (IdentityCharacter you)
  allies <- selectList (AllyControlledBy (IdentityWithId you) <> UnexhaustedAlly)
  supports <- selectList (SupportMatches [SupportControlledBy (IdentityWithId you),UnexhaustedSupport])
  let targets = map AllyRef allies <> map SupportRef supports
  fmap catMaybes $ for targets $ \target -> do
    def <- getsGame (`abilityCardDef` target)
    gained <- getGainedTraits target
    pure $ do
      card <- def
      guard $ not $ HashSet.null $ HashSet.intersection ownTraits (cdTraits card <> gained)
      pure (target,printedResources card)

organizationalSupportPotential :: HasGame m => IdentityId -> m [Resource]
organizationalSupportPotential you = concatMap snd . take 3 . Data.List.sortOn (Down . length . snd) <$> organizationalSupportCandidates you

conditionalAttackReduction :: HasGame m => Target -> Source -> Damage -> m Natural
conditionalAttackReduction target source damage = do
  modifiers <- getModifiers target
  let enemyReduction = if damageSource damage `elem` [FromAttack,FromOverkill] && isJust (sourceEnemy source) then sum [n | EnemyAttackDamageReduction n <- modifiers] else 0
      amounts = [n | AttackDamageReductionUnlessAerialOrRanged n <- modifiers]
      isAttack = damageSource damage `elem` [FromAttack,FromOverkill] || case damageSource damage of FromPlayerAttack _ -> True; FromAllyAttack _ -> True; _ -> False
  if null amounts || not isAttack then pure enemyReduction else do
    def <- getsGame (`abilityCardDef` refocus source)
    traits <- case damageSource damage of
      FromPlayerAttack you -> getCharacterTraits (IdentityCharacter you)
      FromAllyAttack aid -> getCharacterTraits (AllyCharacter aid)
      _ -> maybe (pure mempty) (getCharacterTraits . toCharacter) (sourceEnemy source)
    attack <- maybe (pure Nothing) getEnemyAttack (sourceEnemy source)
    let aerial = Aerial `member` traits || maybe False (member Aerial . cdTraits) def
        ranged = damageRanged damage || maybe False attackRanged attack
    pure $ enemyReduction + if aerial || ranged then 0 else sum amounts

discardIsFromEncounter :: HasGame m => m Bool
discardIsFromEncounter = do
  g <- getGame
  let source = (\(s,_,_) -> s) <$> headMay (gameResolvingAbilitySubTypes g)
      costing = activeCostSource <$> headMay (gameActiveCost g)
  case source <|> costing of
    Just GameRef -> pure False
    Just from -> not <$> isPlayerCardSource from
    Nothing -> pure $ gamePhase g == VillainPhase || not (HashMap.null $ gameBoostCards g) || any (\case EncounterCard _ -> True; _ -> False) (gameFocusedCards g)

-- The response window names the physical card originally assigned. A prior copy of
-- Up, Up, and Away may have exchanged it; the most recently assigned position is
-- retained by an exchange. Villain and minion boost lists use opposite orders.
getAssignedBoostInWindow :: HasGame m => EnemyId -> EncounterCard -> m (Maybe EncounterCard)
getAssignedBoostInWindow enemy original = getsGame $ \g -> case enemy of
  EnemyVillainId vid -> do
    villain <- lookup vid (gameVillains g)
    let cards = Villain.villainBoostCards $ toAttrs villain
    if original `elem` cards then Just original else headMay cards
  EnemyMinionId mid -> do
    minion <- lookup mid (gameMinions g)
    let cards = Minion.minionBoostCards $ toAttrs minion
    if original `elem` cards then Just original else headMay (reverse cards)
  EnemyAttachmentId _ -> Nothing

allyCanPayBasicDefense :: ReverseQueue m => AllyId -> m Bool
allyCanPayBasicDefense aid = do
  modifiers <- getModifiers (AllyRef aid)
  controller <- getSourceController (AllyRef aid)
  case controller of
    Nothing -> pure False
    Just you -> if CannotBasicDefend `elem` modifiers then pure False else
      asYou you $ canAffordCost you (AllyRef aid) (mconcat [cost | AllyBasicPowerAdditionalCost cost <- modifiers])

-- The damage message reaching this point survived would-be-dealt replacements.
-- Tough and take-damage prevention must not change this separate amount.
enemyDamageDealtWindows :: HasGame m => Message -> m [W.Window]
enemyDamageDealtWindows original = case enemyDamageMessage original of
  Nothing -> pure []
  Just (enemy,source,damage) -> do
    present <- selectAny (EnemyWithId enemy)
    let amount = fromMaybe (damageAmount damage) (damageDealtAmount damage)
    pure [W.Window W.After $ W.EnemyWasDealtDamage enemy (damage {damageAmount=amount}) source | present && amount > 0]

isEnemyDealtDamageWindow :: Message -> Bool
isEnemyDealtDamageWindow message = case queuedMessage message of
  CheckWindows windows -> any (\case
    W.Window W.After (W.EnemyWasDealtDamage _ _ _) -> True
    W.Window W.After (W.EnemyTookDamageFrom _ _ _) -> True
    W.Window W.After (W.EnemyAttacksAndDamages _ _ _) -> True
    _ -> False) windows
  _ -> False

getMinionDamageInPlay :: HasGame m => MinionId -> m Natural
getMinionDamageInPlay mid = getsGame $ maybe 0 (Minion.minionDamage . toAttrs) . lookup mid . gameMinions

permitsIceAttack :: AttachmentId -> EnemyId -> [Modifier] -> Bool
permitsIceAttack target enemy modifiers = CannotAttack `notElem` modifiers && CannotAttackMinions `notElem` modifiers
  && CannotAttackEnemy enemy `notElem` modifiers
  && all (\case OnlyAttackAttachment allowed -> allowed == target; CannotAttackTitled title -> title /= "Encased in Ice"; _ -> True) modifiers

-- Intelligence inspects only the encounter cards in its controller's game area.
intelligenceCards :: HasGame m => IdentityId -> m [EncounterCard]
intelligenceCards you = do
  players <- asYou you getPlayers
  assigned <- getsGame $ \g -> concatMap (\ident -> maybe [] (playerIdentityEncounterCards . toAttrs) (lookup ident $ gamePlayers g)) players
  top <- take 1 . pileDeck <$> (getPile =<< getDeckKeyFor you)
  pure $ assigned <> top

canAllyDefendEnemy :: HasGame m => EnemyId -> AllyId -> m Bool
canAllyDefendEnemy enemy aid = do
  mods <- getModifiers (AllyRef aid)
  pure $ CannotDefend `notElem` mods && case enemy of
    EnemyVillainId _ -> CannotDefendAgainstVillain `notElem` mods
    _ -> True

attachmentIsReady :: HasGame m => AttachmentId -> m Bool
attachmentIsReady aid = getsGame $ maybe False (not . field AttachmentExhausted) . lookup aid . gameAttachments

getActiveCostValue :: HasGame m => ActiveCostId -> m (Maybe Cost)
getActiveCostValue acid = getsGame $ fmap activeCostCost . find ((== acid) . activeCostId) . gameActiveCost

getCardsInPlayTargets :: HasGame m => m [Target]
getCardsInPlayTargets = getsGame $ \g ->
  map IdentityRef (HashMap.keys $ gamePlayers g) <> map VillainRef (HashMap.keys $ gameVillains g)
  <> map MinionRef (HashMap.keys $ gameMinions g) <> map AllyRef (HashMap.keys $ gameAllies g)
  <> map MainSchemeRef (HashMap.keys $ gameMainSchemes g) <> map SideSchemeRef (HashMap.keys $ gameSideSchemes g)
  <> map SupportRef (HashMap.keys $ gameSupports g) <> map UpgradeRef (HashMap.keys $ gameUpgrades g)
  <> map AttachmentRef (HashMap.keys $ gameAttachments g) <> map EnvironmentRef (HashMap.keys $ gameEnvironments g)
  <> map ObligationRef (HashMap.keys $ gameObligations g)

-- | Printed stats of an out-of-play encounter card, without creating a game entity.
encounterCardPrintedAttack :: IdentityId -> EncounterCard -> Natural
encounterCardPrintedAttack you card = unAtk $ Minion.minionAttack $ toAttrs $ createMinion you card

encounterCardPrintedScheme :: IdentityId -> EncounterCard -> Natural
encounterCardPrintedScheme you card = unSch $ Minion.minionScheme $ toAttrs $ createMinion you card

encounterCardPrintedHitPoints :: IdentityId -> EncounterCard -> Natural
encounterCardPrintedHitPoints you card = unHp $ Minion.getMinionPrintedHitPoints $ createMinion you card

sourceAttackCharacter :: HasGame m => Source -> m (Maybe CharacterId)
sourceAttackCharacter source = case source of
  IdentityRef you -> pure $ Just $ IdentityCharacter you
  AllyRef aid -> pure $ Just $ AllyCharacter aid
  _ -> do
    def <- getsGame (`abilityCardDef` refocus source)
    if maybe False (elem Ability.Attack . cdAbilitySubTypes) def
      then fmap IdentityCharacter <$> getHeroSourceController source
      else pure Nothing

characterAttackDamageAllowed :: HasGame m => Target -> Damage -> m Bool
characterAttackDamageAllowed target damage = do
  modifiers <- getModifiers target
  pure $ all (\case
    DamageOnlyFromCharacterAttacks (IdentityCharacter you) -> damageSource damage == FromPlayerAttack you
    DamageOnlyFromCharacterAttacks (AllyCharacter aid) -> damageSource damage == FromAllyAttack aid
    DamageOnlyFromCharacterAttacks _ -> False
    _ -> True) modifiers

-- Printed per-player symbols on this physical card can be fixed independently of
-- player reactions and other cards, whose normal table size is preserved.
asPrintedPlayerCount :: HasGame m => Target -> ReaderT Game m a -> m a
asPrintedPlayerCount target body = do
  g <- getGame
  runReaderT body g {gamePlayerCountContext = Data.List.lookup target (gamePrintedPlayerCounts g)}

herculesCanRevealLaborFor :: HasGame m => IdentityId -> m Bool
herculesCanRevealLaborFor = HCHelpers.herculesLaborAvailable
herculesHasGiftFor :: HasGame m => IdentityId -> m Bool
herculesHasGiftFor you = (> 0) <$> HCHelpers.giftCount you

activeEncounterAllowsContributions :: HasGame m => m Bool
activeEncounterAllowsContributions = do
  g <- getGame
  case headMay (gameActiveCost g) of
    Just cost | ForAbility ability <- activeCostTarget cost -> elem AllowsCostContributions <$> getModifiers (abilitySource ability)
    _ -> case gamePayingForAbility g of
      Nothing -> pure False
      Just (code,_) -> do
        targets <- getCardsInPlayTargets
        let candidates = filter (\target -> maybe False ((== code) . toCardCode) (abilityCardDef g target)) targets
        anyM (fmap (elem AllowsCostContributions) . getModifiers) candidates

attackTargetCostsAffordable :: HasGame m => IdentityId -> EnemyId -> m Bool
attackTargetCostsAffordable you enemy = do
  modifiers <- getModifiers (enemyRef enemy)
  hand <- projectMap PlayerIdentityHand unHand you
  let costs = concatMap flattenCosts [cost | EnemyAttacksAdditionalCost cost <- modifiers]
      discarded = sum [n | DiscardHandCardsCost n _ <- costs]
  pure $ fromIntegral (length hand) >= discarded

senseUpgradeDeparture :: Message -> Maybe UpgradeId
senseUpgradeDeparture = \case
  RemoveFromPlay (UpgradeRef uid) -> Just uid
  RemoveFromGame (UpgradeRef uid) -> Just uid
  ReturnToHand (UpgradeRef uid) -> Just uid
  AddToVictoryDisplay (UpgradeRef uid) -> Just uid
  _ -> Nothing

canPlayTopSenseFor :: (HasGame m, HasQueue m) => IdentityId -> m Bool
canPlayTopSenseFor you = do
  cards <- CampaignPlayerHelpers.identitySenseDeck you
  maybe (pure False) (canPlaySearchedCardWithReduction you 0) (headMay cards)
hasEchoTuckedEventFor :: HasGame m => IdentityId -> m Bool
hasEchoTuckedEventFor you = notNull <$> CampaignPlayerHelpers.tuckedPlayerCards you

hasTuckedPlayerCardFor :: HasGame m => IdentityId -> CardMatcher -> m Bool
hasTuckedPlayerCardFor you matcher = any (cardMatch matcher) <$> CampaignPlayerHelpers.tuckedPlayerCards you

-- Generic attachments leave only after their host actually leaves play.
cleanUpGenericUpgrades :: (HasGame m, HasQueue m, MonadThrow m, MonadRandom m) => Game -> m Game
cleanUpGenericUpgrades g = runQueueT do
  let targets = map IdentityRef (HashMap.keys $ gamePlayers g) <> map VillainRef (HashMap.keys $ gameVillains g)
        <> map MinionRef (HashMap.keys $ gameMinions g) <> map AllyRef (HashMap.keys $ gameAllies g)
        <> map MainSchemeRef (HashMap.keys $ gameMainSchemes g) <> map SideSchemeRef (HashMap.keys $ gameSideSchemes g)
        <> map SupportRef (HashMap.keys $ gameSupports g) <> map UpgradeRef (HashMap.keys $ gameUpgrades g)
        <> map AttachmentRef (HashMap.keys $ gameAttachments g) <> map EnvironmentRef (HashMap.keys $ gameEnvironments g)
        <> map ObligationRef (HashMap.keys $ gameObligations g)
  for_ (HashMap.elems $ gameUpgrades g) $ \upgrade ->
    for_ (Upgrade.upgradeAttachedCard $ toAttrs upgrade) $ \host ->
      when (host `notElem` targets) do
        pending <- isQueued $ \case
          RemoveFromPlay ref -> ref == toTarget upgrade
          RemoveFromPlayAfterWindow ref -> ref == toTarget upgrade
          _ -> False
        unless pending $ removeFromPlay upgrade
  pure g

-- Choose which physical Reflexes copy is discarded before checking affordability.
-- This isolated preview keeps typed payment choices and the real hand unchanged.
canAffordEchoReplayFor :: (HasGame m, HasQueue m) => Bool -> IdentityId -> PlayerCard -> m Bool
canAffordEchoReplayFor ignoringCost you card = do
  g <- getGame
  let reflexes = [c | Just player <- [lookup you $ gamePlayers g], c <- unHand (playerIdentityHand $ toAttrs player), toCardCode c `elem` ["60040a","60040b","60040c"]]
  anyM (preview g) reflexes
 where
  preview g reflexes = case lookup you (gamePlayers g) of
    Nothing -> pure False
    Just player -> do
      let replayed = card {pcController = Just you}
      app <- newApp g []
      reserved <- runGameApp app $ runMessage (CardSpecific (IdentityRef you) "fneReserveReplay" $ toJSON (replayed,reflexes)) player
      let attrs = toAttrs reserved
          stagedPlayer = PlayerIdentity attrs {playerIdentityHand = Hand (replayed : filter ((/= pcCardId reflexes) . pcCardId) (unHand $ playerIdentityHand attrs))}
          staged = g & entitiesL . playersL . ix you .~ stagedPlayer
      queryApp <- newApp staged []
      runGameApp queryApp $ isPlayableWith ignoringCost stagedPlayer replayed

getEffectiveHerculesGiftCount :: HasGame m => IdentityId -> m Natural
getEffectiveHerculesGiftCount = HCHelpers.giftCount

-- RR Find searches each eligible game zone regardless of owner. A card already
-- in play cannot enter play again, even if an effect suspends uniqueness.
findPlayerCardsForEntry :: HasGame m => IdentityId -> CardCode -> m [PlayerCard]
findPlayerCardsForEntry you code = asYou you do
  owners <- getPlayers
  candidates <- catMaybes <$> traverse (`findOwnedPlayerCard` code) owners
  unique <- getUniqueTitlesInPlay
  snapshot <- getGame
  let live = [pcCardId c | entity <- map toCard (HashMap.elems $ gameAllies snapshot)
                <> map toCard (HashMap.elems $ gameUpgrades snapshot)
                <> map (toCard . toAttrs) (HashMap.elems $ gameSupports snapshot), PlayerCard c <- [entity]]
      allowed c = pcCardId c `notElem` live && (not (cdUnique $ getCardDef c)
        || not (any (uniqueMatch (title $ cdName $ getCardDef c, maybe [] pure $ subtitle $ cdName $ getCardDef c)) unique))
  filterM (\c -> if allowed c then playerCardEntryAllowed (c {pcController = Just you}) else pure False) candidates

-- RR1.8: keyword Forced Responses share the attack's occurrence with card abilities.
keywordRetaliation :: Message -> Maybe (CharacterId,CharacterId)
keywordRetaliation = \case
  VillainMessage vid (VillainRetaliates attacker) -> Just (VillainCharacter vid,attacker)
  MinionMessage mid (MinionRetaliates attacker) -> Just (MinionCharacter mid,attacker)
  IdentityMessage you (IdentityRetaliate enemy) -> Just (IdentityCharacter you,toCharacter enemy)
  AllyMessage aid (AllyRetaliates enemy _) -> Just (AllyCharacter aid,toCharacter enemy)
  _ -> Nothing

attackCompletionFor :: CharacterId -> CharacterId -> Message -> Bool
attackCompletionFor target attacker message = case queuedMessage message of
  CheckWindows windows -> any completion windows
  _ -> False
 where
  isTarget enemy = toCharacter enemy == target
  isAttacker enemy = toCharacter enemy == attacker
  completion (W.Window W.After window) = case (attacker,window) of
    (IdentityCharacter you,W.IdentityAttack who enemy) -> you == who && isTarget enemy
    (IdentityCharacter you,W.PlayerAttackResolved _ who enemies) -> you == who && any isTarget enemies
    (IdentityCharacter you,W.MadeBasicAttack who) -> you == who
    (IdentityCharacter you,W.MadeBasicAttackAgainst who enemy) -> you == who && isTarget enemy
    (AllyCharacter aid,W.AllyAttack who enemy) -> aid == who && isTarget enemy
    (AllyCharacter aid,W.AllyAttackCompletedAgainst who enemies) -> aid == who && any isTarget enemies
    (AllyCharacter aid,W.PlayerAttackResolved (AllyRef who) _ enemies) -> aid == who && any isTarget enemies
    (_,W.EnemyAttackOnEnemy other enemy) -> isTarget enemy && isAttacker other
    (_,W.EnemyAttack enemy you) -> target == IdentityCharacter you && isAttacker enemy
    (_,W.EnemyAttackAlly enemy aid _) -> target == AllyCharacter aid && isAttacker enemy
    (_,W.AllyDefendedAgainst aid enemy) -> target == AllyCharacter aid && isAttacker enemy
    _ -> False
  completion _ = False

retaliationAmount :: HasGame m => CharacterId -> m Natural
retaliationAmount character = do
  snapshot <- getGame
  let present = case character of
        IdentityCharacter you -> HashMap.member you (gamePlayers snapshot)
        AllyCharacter aid -> HashMap.member aid (gameAllies snapshot)
        VillainCharacter vid -> HashMap.member vid (gameVillains snapshot)
        MinionCharacter mid -> HashMap.member mid (gameMinions snapshot)
        AttachmentCharacter aid -> HashMap.member aid (gameAttachments snapshot)
      target = characterRef character
  if not present then pure 0 else do
    blanked <- isBlanked target
    modifiers <- getModifiers target
    let printed = if blanked then [] else maybe [] (toList . cdKeywords) (abilityCardDef snapshot target)
        modify (KeywordModifier keyword) = (keyword :)
        modify (LoseKeyword keyword) = filter (/= keyword)
        modify _ = id
    pure $ sum [n | Retaliate n <- foldr modify printed modifiers]

retaliationAbilities :: HasGame m => m [Ability]
retaliationAbilities = do
  windows <- getsGame $ concat . take 1 . gameWindows
  fmap catMaybes $ for [(target,attacker) | W.Window W.After (W.RetaliationPending target attacker) <- windows] $ \(target,attacker) -> do
    amount <- retaliationAmount target
    let matching = W.WindowOneOf $ case sourceEnemy (toSource $ characterRef target) of
          Just enemy -> [W.EnemyWasAttacked W.After (EnemyWithId enemy),W.IdentityAttacked W.After AnyIdentity (EnemyWithId enemy),W.AllyAttacked W.After AnyAlly (EnemyWithId enemy)]
          Nothing -> case sourceEnemy (toSource $ characterRef attacker) of
            Just enemy -> [W.EnemyAttackedAnyTarget W.After (EnemyWithId enemy)]
              <> [W.AllyDefendsAgainst W.After (AllyWithId aid) (EnemyWithId enemy) | AllyCharacter aid <- [target]]
            Nothing -> []
        response = Ability.label "$label.resolveRetaliate" $ Ability.windowAbility (characterRef target) 98000 matching Ability.ForcedResponse NoCost
          (Run [CardSpecific GameRef "resolveKeywordRetaliate" $ toJSON (target,attacker)])
    pure $ if amount > 0 then Just response else Nothing

-- Explicit references add only their named entities to an otherwise local query.
inCompetitiveAreaOrExplicit :: HasGame m => [Target] -> (a -> Target) -> [a] -> m [a]
inCompetitiveAreaOrExplicit explicit ref entities = do
  local <- inYourAreaOnly ref entities
  competitive <- getCompetition
  pure $ if isNothing competitive then local else
    [entity | entity <- entities, ref entity `elem` (explicit <> map ref local)]

explicitEnemyTargets :: EnemyMatcher -> [Target]
explicitEnemyTargets = \case
  EnemyWithId enemy -> [enemyRef enemy]
  EnemyMatches matchers -> concatMap explicitEnemyTargets matchers
  DefeatedEnemy matcher -> explicitEnemyTargets matcher
  _ -> []

explicitVillainTargets :: VillainMatcher -> [Target]
explicitVillainTargets = \case
  VillainWithId vid -> [VillainRef vid]
  VillainMatches matchers -> concatMap explicitVillainTargets matchers
  _ -> []

attackCompletionWindowFor :: EnemyId -> Message -> Bool
attackCompletionWindowFor enemy = \case
  CheckWindows windows -> any (\case
    W.Window W.After (W.EnemyAttack attacker _) -> enemy == attacker
    W.Window W.After (W.EnemyAttackAlly attacker _ _) -> enemy == attacker
    W.Window W.After (W.EnemyAttackOnEnemy attacker _) -> enemy == attacker
    _ -> False) windows
  _ -> False

-- Cap's Shield checks use of ATK, including ATK-based attack events (FFG,
-- December 17, 2025). Flat damage attacks and attacks using only allies' ATK do not.
sourceATKAttackIdentity :: HasGame m => Source -> m (Maybe IdentityId)
sourceATKAttackIdentity source = do
  snapshot <- getGame
  let hero you = case lookup you (gamePlayers snapshot) of Just identity | isHero identity -> Just you; _ -> Nothing
  pure $ case source of
    IdentityRef you -> hero you
    EventRef eid -> do
      event <- lookup eid (gameEvents snapshot)
      let attrs = toAttrs event
          code = canonicalCardCode $ eventCardDef attrs
          you = eventController attrs
          contributes = IdentityCharacter you `elem` paymentExhaustedCharacters (eventPaidWith attrs)
      guard $ code `elem` ["01077","10002","19007","19014","21016","21062","25018","29017","32045","35023","58005"] || (code == "23032" && contributes)
      hero you
    _ -> Nothing

engineAttackResponse :: Ability -> Bool
engineAttackResponse ability = abilityIndex ability >= 98000 && abilityIndex ability < 99000

delayedAttackDamageAbilities :: HasGame m => m [Ability]
delayedAttackDamageAbilities = do
  windows <- getsGame $ concat . take 1 . gameWindows
  pure
    [ Ability.label (countLabel "dealDamageToAnEnemy" amount) $ Ability.windowAbility source (98001 + index)
        (W.EnemyAttackedAnyTarget W.After $ EnemyWithId enemy) Ability.ForcedResponse NoCost
        (Run [CardSpecific GameRef "resolveDelayedAttackDamage" $ toJSON (enemy,source,amount)])
    | (index,(_,enemy,source,amount)) <- zip [0..] [(you,enemy,source,amount) | W.Window W.After (W.DelayedAttackDamagePending you enemy source amount) <- windows]
    ]

inCompetitiveAreaOnlyOrExplicit :: HasGame m => [Target] -> (a -> Target) -> [a] -> m [a]
inCompetitiveAreaOnlyOrExplicit explicit ref entities = do
  competitive <- getCompetition
  if isNothing competitive then pure entities else inCompetitiveAreaOrExplicit explicit ref entities

explicitAllyTargets :: AllyMatcher -> [Target]
explicitAllyTargets = \case
  AllyWithId aid -> [AllyRef aid]
  AllyMatches matchers -> concatMap explicitAllyTargets matchers
  _ -> []

explicitUpgradeTargets :: UpgradeMatcher -> [Target]
explicitUpgradeTargets = \case
  UpgradeWithId uid -> [UpgradeRef uid]
  UpgradeMatches matchers -> concatMap explicitUpgradeTargets matchers
  UpgradeOneOf matchers -> concatMap explicitUpgradeTargets matchers
  _ -> []

explicitMinionTargets :: MinionMatcher -> [Target]
explicitMinionTargets = \case
  MinionWithId mid -> [MinionRef mid]
  MinionMatches matchers -> concatMap explicitMinionTargets matchers
  LeftPlayMinion matcher -> explicitMinionTargets matcher
  _ -> []

explicitSchemeTargets :: SchemeMatcher -> [Target]
explicitSchemeTargets = \case
  SchemeWithId sid -> [SchemeRef sid]
  SchemeMatches matchers -> concatMap explicitSchemeTargets matchers
  _ -> []

explicitSideSchemeTargets :: SideSchemeMatcher -> [Target]
explicitSideSchemeTargets = \case
  SideSchemeWithId sid -> [SideSchemeRef sid]
  SideSchemeMatchAll matchers -> concatMap explicitSideSchemeTargets matchers
  LeftPlaySideScheme matcher -> explicitSideSchemeTargets matcher
  _ -> []

explicitAttachmentTargets :: Game -> AttachmentMatcher -> [Target]
explicitAttachmentTargets game = \case
  AttachmentWithId aid -> [AttachmentRef aid]
  AttachmentOnEnemy enemy -> onEnemy enemy
  AttachmentOnEnemyWithHeroAbility enemy -> onEnemy enemy
  AttachmentOnVillain matcher -> concat [onEnemy (EnemyVillainId vid) | VillainRef vid <- explicitVillainTargets matcher]
  AttachmentMatches matchers -> concatMap (explicitAttachmentTargets game) matchers
  _ -> []
 where
  onEnemy enemy = [AttachmentRef aid | (aid,attachment) <- HashMap.toList (gameAttachments game), Attachment.attachmentEnemy (toAttrs attachment) == Just enemy]
    <> [AttachmentRef (AttachmentId uuid) | u <- HashMap.elems (gameUpgrades game),
      let UpgradeId uuid = toId u,isJust (Upgrade.getUpgradeAttachmentTraits u),upgradeAttachedEnemy (toAttrs u) == Just enemy]

-- | Defeat responses still have the departing ally's physical card, including a
-- converted encounter minion, in the removed-entity snapshot.
getAllyPhysicalCard :: HasGame m => AllyId -> m (Maybe Card)
getAllyPhysicalCard aid = getsGame $ \g -> toCard <$> (lookup aid (gameAllies g) <|> lookup aid (entitiesAllies $ gameRemovedEntities g))

-- | After defeat, match the departing ally as it last existed. Its live slot is
-- already empty, but its controller, traits and physical board still matter.
allyMatchesAtDefeat :: HasGame m => AllyMatcher -> AllyId -> m Bool
allyMatchesAtDefeat matcher aid = do
  g <- getGame
  let departed = lookup aid (gameAllies g) <|> lookup aid (entitiesAllies $ gameRemovedEntities g)
      view = maybe g (\ally -> g & entitiesL . alliesL . at aid ?~ ally) departed
  runReaderT (allyMatches matcher aid) view

inPlayBoostCallbackTarget :: Message -> Maybe Target
inPlayBoostCallbackTarget = \case
  CardSpecific target _ _ -> Just target
  WithDiscarded target _ _ -> Just target
  WithDiscardedMatch target _ _ -> Just target
  WithChosen target _ _ -> Just target
  WithBoostIcons target _ _ -> Just target
  _ -> Nothing

inPlayBoostCard :: Game -> Target -> Maybe EncounterCard
inPlayBoostCard g target = do
  mid <- case target of MinionRef ident -> Just ident; _ -> Nothing
  guard $ any (\(source,_,_) -> source == mid) (gameInPlayBoosts g)
  minion <- lookup mid (gameMinions g)
  case toCard minion of EncounterCard card -> Just card; _ -> Nothing

dropFirstInPlayBoost :: MinionId -> [(MinionId,IdentityId,Minion)] -> [(MinionId,IdentityId,Minion)]
dropFirstInPlayBoost _ [] = []
dropFirstInPlayBoost mid (scope@(other,_,_) : rest)
  | mid == other = rest
  | otherwise = scope : dropFirstInPlayBoost mid rest

-- | Putting an ally into play ignores play costs and play-only restrictions, but
-- cannot duplicate a physical card or a friendly unique character.
canPutPlayerAllyIntoPlay :: HasGame m => IdentityId -> PlayerCard -> m Bool
canPutPlayerAllyIntoPlay you card = asYou you do
  targets <- getCardsInPlayTargets
  live <- getsGame $ \g -> any ((== pcCardId card) . toCardId) (mapMaybe (cardInPlay g) targets)
  uniques <- getUniqueTitlesInPlay
  let def = getCardDef card
      name = (title $ cdName def, maybe [] pure $ subtitle $ cdName def)
      clash = cdUnique def && any (uniqueMatch name) uniques
  allowed <- playerCardEntryAllowed (card {pcController = Just you})
  pure $ cdCardType def == AllyType && not live && not clash && allowed

-- | Check the original removal before its replacement window. A prohibited
-- removal never becomes imminent merely because another scheme can receive it.
characterThreatRemovalAllowed :: HasGame m => Bool -> Bool -> Source -> SchemeId -> m Bool
characterThreatRemovalAllowed basic thwarting source scheme = do
  allowed <- canRemoveThreatFrom source scheme
  modifiers <- getModifiers (schemeRef scheme)
  controller <- getSourceController source
  own <- getModifiers source
  let locked = ThreatCannotBeRemoved `elem` modifiers || (thwarting && ThreatCannotBeRemovedByThwarting `elem` modifiers)
  if not allowed || locked then pure False else case scheme of
    SchemeSideSchemeId _ -> do
      forbidden <- maybe (pure False) (fmap (elem CannotThwartSideSchemes) . getModifiers . IdentityRef) controller
      let onlyBy = [you | ThreatRemovableOnlyBy you <- modifiers]
      pure $ not (thwarting && forbidden) && not (any (\you -> maybe False (/= you) controller) onlyBy)
    SchemeMainSchemeId sid -> do
      restricted <- isRestrictedMainScheme sid
      if thwarting then do
        patrolled <- maybe (pure False) isPatrolled controller
        pure $ not (patrolled && restricted && IgnoresPatrol `notElem` own && not (basic && BasicThwartIgnoresPatrol `elem` own))
      else do
        crisis <- hasCrisisInYourArea
        controllerIgnores <- case source of
          AllyRef _ -> pure False
          _ -> maybe (pure False) (fmap (elem IgnoresCrisis) . getModifiers . IdentityRef) controller
        let playerCard = case source of
              IdentityRef _ -> True
              AllyRef _ -> True
              EventRef _ -> True
              SupportRef _ -> True
              UpgradeRef _ -> True
              _ -> False
        pure $ not (crisis && restricted && playerCard && IgnoresCrisis `notElem` own && not controllerIgnores)

-- | Check that placing one more status would change this character's state.
characterCanGainStatus :: HasGame m => CharacterId -> StatusCard -> m Bool
characterCanGainStatus character status = do
  present <- characterMatches AnyCharacter character
  modifiers <- getModifiers (characterRef character)
  def <- getsGame (`abilityCardDef` characterRef character)
  blanked <- isBlanked (characterRef character)
  count <- getCharacterStatusCount character status
  let printed = if blanked then [] else maybe [] (toList . cdKeywords) def
      keywords = filter (\keyword -> LoseKeyword keyword `notElem` modifiers) (printed <> [keyword | KeywordModifier keyword <- modifiers])
      blocked = case status of StunnedStatus -> CannotBeStunned `elem` modifiers; ConfusedStatus -> CannotBeConfused `elem` modifiers; ToughStatus -> False
      limit = if status == ToughStatus then 1 + sum [n | AdditionalToughStatusCards n <- modifiers] else if Steady `elem` keywords then 2 else 1
      unlimited = status == ToughStatus && CanHaveAnyNumberOfToughStatusCards `elem` modifiers
  pure $ present && not blocked && (status == ToughStatus || Stalwart `notElem` keywords) && (unlimited || count < limit)

getTargetCardDef :: HasGame m => Target -> m (Maybe CardDef)
getTargetCardDef target = getsGame (`abilityCardDef` target)

-- Dealing damage precedes taking it and all damage replacements. An absolute
-- prohibition therefore answers before tough or Out Front can replace the packet.
damageDealingForbidden :: HasGame m => Message -> m Bool
damageDealingForbidden message = case enemyDamageMessage message of
  Nothing -> pure False
  Just (enemy,_,_) -> elem CannotBeDealtDamage <$> getModifiers (enemyRef enemy)

-- "The main scheme" belongs to the source's play area in Protection Racket.
-- An unqualified "a scheme" continues to offer every main scheme.
mainSchemeMatcherForSource :: HasGame m => IdentityId -> Source -> SchemeMatcher -> m SchemeMatcher
mainSchemeMatcherForSource you source matcher = do
  owner <- playAreaOwner source
  owned <- maybe (pure False) (selectAny . MainSchemeOwnedBy) owner
  pure $ if owned then maybe matcher (\who -> replace who matcher) owner else matcher
 where
  replace who = \case EncounterMainScheme -> MainSchemeOwnedBy who; SchemeMatches xs -> SchemeMatches $ map (replace who) xs; other -> other
  playAreaOwner from = case refocus from of
    TreacheryRef _ -> pure $ Just you
    EffectRef eid -> getsGame (fmap (Effect.effectSource . toAttrs) . lookup eid . gameEffects) >>= maybe (pure Nothing) playAreaOwner
    _ -> cardPlayAreaOwner $ refocus from

-- Attachments and upgrades occupy their host's play area even when another
-- player owns or paid for the card. Villains and shared schemes are outside it.
cardPlayAreaOwner :: HasGame m => Target -> m (Maybe IdentityId)
cardPlayAreaOwner target = case target of
  CharacterRef character -> characterPlayAreaOwner character
  EnemyRef enemy -> characterPlayAreaOwner $ toCharacter enemy
  IdentityRef you -> pure $ Just you
  AllyRef aid -> characterPlayAreaOwner $ AllyCharacter aid
  MinionRef mid -> characterPlayAreaOwner $ MinionCharacter mid
  VillainRef _ -> pure Nothing
  MainSchemeRef mid -> getsGame $ join . fmap (mainSchemeOwner . toAttrs) . lookup mid . gameMainSchemes
  SchemeRef (SchemeMainSchemeId mid) -> cardPlayAreaOwner $ MainSchemeRef mid
  SchemeRef (SchemeSideSchemeId sid) -> cardPlayAreaOwner $ SideSchemeRef sid
  UpgradeRef uid -> do
    upgrade <- getsGame $ \g -> lookup uid (gameUpgrades g) <|> lookup uid (entitiesUpgrades $ gameRemovedEntities g)
    case upgrade of
      Nothing -> pure Nothing
      Just value -> do
        let a = toAttrs value
            host = Upgrade.upgradeAttachedCard a
              <|> (AllyRef <$> Upgrade.upgradeAttachedAlly a)
              <|> (enemyRef <$> Upgrade.upgradeAttachedEnemy a)
              <|> (IdentityRef <$> Upgrade.upgradeAttachedIdentity a)
              <|> (MainSchemeRef <$> Upgrade.upgradeAttachedMainScheme a)
              <|> (SideSchemeRef <$> Upgrade.upgradeAttachedSideScheme a)
        maybe (pure $ Just $ Upgrade.upgradeController a) cardPlayAreaOwner host
  AttachmentRef aid -> do
    attachment <- getsGame $ \g -> lookup aid (gameAttachments g) <|> lookup aid (entitiesAttachments $ gameRemovedEntities g)
    case attachment of
      Nothing -> pure Nothing
      Just value -> do
        let a = toAttrs value
            host = (AllyRef <$> Attachment.attachmentAlly a)
              <|> (enemyRef <$> Attachment.attachmentEnemy a)
              <|> (IdentityRef <$> Attachment.attachmentIdentity a)
              <|> (MainSchemeRef <$> Attachment.attachmentMainScheme a)
              <|> (SideSchemeRef <$> Attachment.attachmentSideScheme a)
        maybe (pure Nothing) cardPlayAreaOwner host
  EnvironmentRef eid -> getsGame $ join . fmap (MGEnvironment.environmentOwner . toAttrs) . lookup eid . gameEnvironments
  _ -> getSourceController $ refocus target

getOwnedMainScheme :: HasGame m => IdentityId -> m (Maybe MainSchemeId)
getOwnedMainScheme you = selectOne (MainSchemeOwnedBy you) <&> (>>= \case SchemeMainSchemeId mid -> Just mid; _ -> Nothing)

-- A play area here is a player's cards and the minions engaged with that player;
-- all players still share one game area and may affect one another's schemes.
characterPlayAreaOwner :: HasGame m => CharacterId -> m (Maybe IdentityId)
characterPlayAreaOwner character = case character of
  IdentityCharacter you -> pure $ Just you
  AllyCharacter aid -> getSourceController $ AllyRef aid
  MinionCharacter mid -> getsGame $ fmap (minionEngagedIdentity . toAttrs) . (\g -> lookup mid (gameMinions g) <|> lookup mid (entitiesMinions $ gameRemovedEntities g))
  _ -> pure Nothing

replaceAttackOrThwartWithCounterRemoval :: ReverseQueue m => IdentityId -> m Bool
replaceAttackOrThwartWithCounterRemoval you = do
  modifiers <- getModifiers (IdentityRef you)
  let attachments = [aid | ReplaceAttackOrThwartWithCounterRemoval aid <- modifiers]
  unless (null attachments) $ chooseOrRunOne you [targeting aid (removeCounters aid 1) | aid <- attachments]
  pure $ notNull attachments

boostAssignmentEnemy :: Message -> Maybe EnemyId
boostAssignmentEnemy = \case
  DealBoost target -> case target of VillainRef vid -> Just $ EnemyVillainId vid; MinionRef mid -> Just $ EnemyMinionId mid; EnemyRef enemy -> Just enemy; _ -> Nothing
  CardSpecific GameRef "giveCardAsBoost" (fromJSON -> Success (_ :: Card,enemy :: EnemyId)) -> Just enemy
  CardSpecific GameRef "giveDefeatedMinionAsBoost" (fromJSON -> Success (_ :: MinionId,enemy :: EnemyId)) -> Just enemy
  _ -> Nothing

friendlyProxyAlly :: Game -> MinionId -> Maybe AllyId
friendlyProxyAlly g mid = friendlyBoostAlly <$> find ((== unMinionId mid) . unAllyId . friendlyBoostAlly) (g.gameFriendlyBoosts)

modifyFriendlyBoost :: AllyId -> (FriendlyBoostContext -> FriendlyBoostContext) -> Game -> Game
modifyFriendlyBoost aid f g = g {gameFriendlyBoosts=[if friendlyBoostAlly context == aid then f context else context | context <- g.gameFriendlyBoosts]}

friendlyBoostTarget :: Game -> Target -> Target
friendlyBoostTarget g target = case target of
  MinionRef mid | Just aid <- friendlyProxyAlly g mid -> AllyRef aid
  EnemyRef (EnemyMinionId mid) | Just aid <- friendlyProxyAlly g mid -> AllyRef aid
  CharacterRef (MinionCharacter mid) | Just aid <- friendlyProxyAlly g mid -> AllyRef aid
  _ -> target

friendlyBoostMessage :: Game -> Message -> Maybe Message
friendlyBoostMessage g = \case
  GiveStatus target status | let actual = friendlyBoostTarget g target,actual /= target -> Just $ GiveStatus actual status
  MinionMessage mid message | Just aid <- friendlyProxyAlly g mid -> case message of
    MinionStunned _ -> Just $ AllyMessage aid AllyStunned
    MinionConfused _ -> Just $ GiveStatus (AllyRef aid) ConfusedStatus
    MinionBecomeTough -> Just $ GiveStatus (AllyRef aid) ToughStatus
    MinionDamaged source damage -> Just $ AllyMessage aid $ AllyDamaged source damage
    MinionHealed n -> Just $ AllyMessage aid $ AllyHealed n
    _ -> Nothing
  _ -> Nothing

friendlyBoostAttackView :: Game -> MinionId -> Maybe Attack
friendlyBoostAttackView g mid = do
  context <- find ((== unMinionId mid) . unAllyId . friendlyBoostAlly) (g.gameFriendlyBoosts)
  guard $ friendlyBoostAttack context
  villain <- headMay $ HashMap.keys $ gameVillains g
  pure $ attackWith (friendlyBoostKeywords context) (EnemyMinionId mid) (VillainCharacter villain) 0

-- Runtime metadata stays with the same physical ally while its current counters,
-- damage and attachments are restored. This also preserves controlled encounter
-- allies, whose runtime builder differs from the card's original minion builder.
setAllyAttrs :: Ally -> Attrs Ally -> Ally
setAllyAttrs (Ally.Ally (original :: a)) attrs = case (toJSON original,toJSON attrs) of
  (Object metadata,Object fields) -> case fromJSON (Object $ KeyMap.union fields metadata) of
    Success (value :: a) -> Ally.Ally value
    Error err -> error $ "invalid restored ally state: " <> err
  _ -> error "ally state is not an object"

setBlackCatAllyAttrs :: IdentityId -> AllyId -> Attrs Ally -> Ally
setBlackCatAllyAttrs controller aid attrs = case lookupAlly "60169" controller aid of
  Ally.Ally (_ :: a) -> case fromJSON (toJSON attrs) of
    Success (value :: a) -> Ally.Ally value
    Error err -> error $ "invalid Black Cat ally state: " <> err

-- Track the physical card, then commit only cards still removed at victory.
-- Removing a card temporarily in order to tuck it or make it a boost is not a
-- campaign removal once that same card is present in its new zone.
knownGameCards :: Game -> [Card]
knownGameCards g =
  [PlayerCard card | player <- HashMap.elems $ gamePlayers g,let a=toAttrs player,card <- unHand a.playerIdentityHand <> unDeck a.playerIdentityDeck <> unDiscard a.playerIdentityDiscard]
  <> [PlayerCard card | player <- HashMap.elems $ gamePlayers g,Just card <- [playerIdentityPendingTopDeckPlay $ toAttrs player]]
  <> concat [storedIdentityCards $ toJSON side | player <- HashMap.elems $ gamePlayers g,side <- HashMap.elems $ playerIdentitySides $ toAttrs player]
  <> [PlayerCard card | cost <- gameActiveCost g,ForCard card <- [activeCostTarget cost]]
  <> [PlayerCard card | cost <- gameActiveCost g,card <- activeCostSpentCards cost]
  <> map (PlayerCard . snd) (gamePlayerBoostCards g)
  <> map toCard (HashMap.elems $ gameAllies g)
  <> map (toCard . toAttrs) (HashMap.elems $ gameSupports g)
  <> map toCard (HashMap.elems $ gameUpgrades g)
  <> map toCard (HashMap.elems $ gameMinions g)
  <> map toCard (HashMap.elems $ gameAttachments g)
  <> map PlayerCard (concatMap (Ally.allyHeldCards . toAttrs) (HashMap.elems $ gameAllies g)
      <> concatMap (Minion.minionHeldCards . toAttrs) (HashMap.elems $ gameMinions g)
      <> concatMap (supportHeldCards . toAttrs) (HashMap.elems $ gameSupports g)
      <> concatMap (upgradeHeldCards . toAttrs) (HashMap.elems $ gameUpgrades g)
      <> concatMap (SideScheme.sideSchemeHeldCards . toAttrs) (HashMap.elems $ gameSideSchemes g)
      <> concatMap (MS.mainSchemeHeldCards . toAttrs) (HashMap.elems $ gameMainSchemes g)
      <> concatMap (MGEnvironment.environmentHeldCards . toAttrs) (HashMap.elems $ gameEnvironments g)
      <> concatMap (Obligation.obligationHeldCards . toAttrs) (HashMap.elems $ gameObligations g))
  <> scenarioSetAsideCards (toAttrs $ g.gameScenario) <> gameCollection g
  <> map EncounterCard (scenarioEncounterDeck (toAttrs $ g.gameScenario) <> scenarioDiscard (toAttrs $ g.gameScenario)
      <> concat [pileDeck pile <> pileDiscard pile | pile <- HashMap.elems $ scenarioDecks $ toAttrs $ g.gameScenario]
      <> HashMap.elems (gameBoostCards g) <> gameVictoryDisplay g)
  <> map EncounterCard (concatMap (Villain.villainBoostCards . toAttrs) (HashMap.elems $ gameVillains g)
      <> concatMap (Minion.minionBoostCards . toAttrs) (HashMap.elems $ gameMinions g)
      <> concat [friendlyBoostPending context <> friendlyBoostRevealed context | context <- gameFriendlyBoosts g])
  <> map PlayerCard (gamePlayerVictoryDisplay g)
  <> [EncounterCard card | player <- HashMap.elems $ gamePlayers g,let a=toAttrs player
     ,card <- playerIdentityEncounterCards a <> playerIdentityTuckedEncounterCards a <> playerIdentityBoostCards a]
 where
  -- Read actual identity storage, rather than card snapshots or past receipts.
  storedIdentityCards = \case
    Object fields -> concatMap readField (KeyMap.toList fields)
    Array values -> concatMap storedIdentityCards $ toList values
    _ -> []
  readField (key,value)
    | key `elem` ["campaignSenseCards","campaignEchoCards","giftDeck","laborDeck"] = case fromJSON value of
        Success cards -> map PlayerCard cards
        Error _ -> case fromJSON value of Success cards -> map EncounterCard cards; Error _ -> []
    | otherwise = storedIdentityCards value

rememberFearNoEvilRemoval :: Message -> Game -> Game -> Game
rememberFearNoEvilRemoval message before after = case (message,gameCampaign after) of
  (RemoveFromGame target,Just campaign) | campaign.kind == Campaign.FearNoEvilCampaign ->
    let card = cardInPlay before target <|> case target of CardIdRef cid -> find ((== cid) . toCardId) $ knownGameCards before; _ -> Nothing
    in case card of
      Just physical | let def=getCardDef physical,cdUnique def,cdCardType def `elem` [AllyType,SupportType] ->
        after {gameCampaign=Just $ Campaign.recordInLog (Campaign.RecordFearNoEvilRemoved (toCardId physical) $ title $ cdName def) campaign}
      _ -> after
  _ -> after

fearNoEvilRemovedTitles :: HasGame m => m [Text]
fearNoEvilRemovedTitles = getsGame $ \g -> case g.gameCampaign of
  Nothing -> []
  Just campaign -> let live=map toCardId $ knownGameCards g
    in nub [name | (cid,name) <- campaign.campaignLog.fearNoEvil.pendingRemoved,cid `notElem` live]

getRound :: HasGame m => m Int
getRound = getsGame gameRound

-- | The environment is in play, in the player's area.
environmentIntoPlay :: IdentityId -> Environment -> Game -> Game
environmentIntoPlay ident environment g =
  g
    & (entitiesL . environmentsL %~ insertMap (toId environment) environment)
    & intoAreaOf ident (\a -> a {areaEnvironments = areaEnvironments a <> [toId environment]})
