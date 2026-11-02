module Marvel.CivilWar.TeamSpec (spec,competitive,addSecondBoard) where

import Control.Exception (evaluate)
import Control.Monad.Random (evalRand)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Strict qualified as HashMap
import Marvel.CivilWar.Types qualified as CW
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers qualified as CWHelp
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as M
import Marvel.MainScheme.Types qualified as Main
import Marvel.Keyword qualified as K
import Marvel.EncounterSet qualified as E
import Marvel.Hp (HP (..),unHp)
import Marvel.CivilWar.LeaderSpec (withLeader)
import Marvel.SideScheme.Cards qualified as S
import Marvel.Game.Public (publicGameFor)
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query
import Marvel.Scenario.Types
import Marvel.Villain.Types qualified as V
import System.Random (mkStdGen)
import Test.Hspec qualified as H
import TestImport

newtype CompetitiveFixture = CompetitiveFixture (Attrs Scenario `With` CW.CompetitionSetup)
  deriving newtype (Show, Eq, ToJSON, FromJSON)
instance IsScenario CompetitiveFixture where
  toScenarioAttrs (CompetitiveFixture (attrs `With` _)) = attrs
  overScenarioAttrs f (CompetitiveFixture (attrs `With` setup)) = CompetitiveFixture (f attrs `With` setup)
  scenarioCompetitionSetup (CompetitiveFixture (_ `With` setup)) = Just setup
instance RunMessage CompetitiveFixture where
  runMessage msg (CompetitiveFixture (attrs `With` setup)) =
    (\attrs' -> CompetitiveFixture (attrs' `With` setup)) <$> runMessage msg attrs

setup :: CW.CompetitionSetup
setup = CW.CompetitionSetup
  (fromMaybe (error "missing Iron Man setup") $ CW.defaultTeamScenario "56059")
  (fromMaybe (error "missing Captain America setup") $ CW.defaultTeamScenario "56137")

competitive :: [Text] -> IO TestGame
competitive heroes = do
  let standard = ScenarioSetup "01094" [] Normal
      scenario = fromMaybe (error "missing Rhino") $ lookupScenario standard
      fixture = Scenario $ CompetitiveFixture (toAttrs scenario `With` setup)
  identities <- either (error . show) pure . sequence $ evalRand (traverse (loadPlayer . starter) heroes) (mkStdGen 1)
  seated <- case identities of
    [] -> error "missing fixture players"
    p : ps -> pure (p :| ps)
  app <- newGameApp (newTable fixture seated) [StartGame] (mkStdGen 1) (const $ pure ())
  ran <- newIORef []
  logged <- newIORef []
  let tg = TestGame app ran logged (map toId identities)
  inGame tg $ runGame >> toFirstTurn
  pure tg

addSecondBoard :: TestGame -> GameAppT VillainId
addSecondBoard tg = do
  let p = player1 tg
      q = last (seats tg)
  resolve [SetActivePlayer q, AddVillain "01113", AddMainScheme "01116", SetActivePlayer p]
  fromMaybe (error "missing second leader") <$> getActiveVillainIn (CW.teamArea CW.Resistance)

spec :: Spec
spec = describe "Civil War team boundaries" do
  it "keeps legacy scenario setup JSON compatible" do
    let old = ScenarioSetup "01094" [] Normal
    fromJSON (object ["scenarioCode" .= ("01094" :: Text), "modulars" .= ([] :: [Text]), "difficulty" .= ("Normal" :: Text)]) `H.shouldBe` Success old
    fromJSON (toJSON old) `H.shouldBe` Success old

  it "round trips custom schemes and both teams without changing the setup's flat shape" do
    let custom = CivilWarScenarioSetup "56059" [] Expert ["56121b","56124b"] (Just setup)
    fromJSON (toJSON custom) `shouldBe` Success custom
    case toJSON custom of
      Object _ -> pure ()
      _ -> expectationFailure "scenario setup must stay an object"

  it "accepts each preconstructed pairing and rejects modules from the wrong faction" do
    CW.validateCompetitionSetup setup `H.shouldBe` Right ()
    let wrong = setup {CW.competitionRegistration = (CW.competitionRegistration setup) {CW.teamScenarioModulars = take 4 CW.resistanceModulars}}
    CW.validateCompetitionSetup wrong `H.shouldBe` Left "error.cwModularFaction"

  it "counts a two-player competition as one player per team" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      getStartingPlayerCount `shouldReturn` 1
      getPlayerCount `shouldReturn` 1
      getPlayers `shouldReturn` [player1 tg]
      asYou (player2 tg) getPlayers `shouldReturn` [player2 tg]
      asYou (player2 tg) getPlayerCount `shouldReturn` 1
      v <- getVillain
      liftIO $ unHp (V.villainMaxHp $ toAttrs v) `shouldBe` 14

  it "uses two-player hit points on each board of a four-player competition" do
    tg <- competitive ["spider-man","she-hulk","captain-marvel","iron-man"]
    inGame tg do
      getStartingPlayerCount `shouldReturn` 2
      v <- getVillain
      liftIO $ unHp (V.villainMaxHp $ toAttrs v) `shouldBe` 28
      second <- addSecondBoard tg
      villains <- getsGame gameVillains
      liftIO $ unHp (V.villainMaxHp $ toAttrs $ villains HashMap.! second) `shouldBe` 24

  it "has a separate first player on each team" do
    tg <- competitive ["spider-man","she-hulk","captain-marvel","iron-man"]
    inGame tg do
      first <- selectJust FirstPlayer
      liftIO $ first `shouldBe` player1 tg
      let resistance = seats tg !! 2
      asYou resistance (selectJust FirstPlayer) `shouldReturn` resistance
      asYou resistance getPlayers `shouldReturn` drop 2 (seats tg)

  it "finishes registration's player phase before resistance begins its turns" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      _ <- addSecondBoard tg
      endTurn
      competition <- getCompetition
      liftIO $ fmap CW.competitivePhase competition `shouldBe` Just CW.ResistancePlayerPhase
      getActivePlayerId `shouldReturn` player2 tg
      getPhasePlayers `shouldReturn` [player2 tg]
      offers (== EndTurn) `shouldReturn` True

  it "plays both villain phases before starting registration's next round" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      _ <- addSecondBoard tg
      roundBefore <- getsGame gameRound
      endTurn
      endTurn
      toFirstTurn
      competition <- getCompetition
      liftIO $ fmap CW.competitivePhase competition `shouldBe` Just CW.RegistrationPlayerPhase
      getActivePlayerId `shouldReturn` player1 tg
      getsGame gameRound `shouldReturn` (roundBefore + 1)

  it "resolves a leader attack with a boost card and no declared defender" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      attacker <- villainIdNow
      defender <- addSecondBoard tg
      _ <- addMinion (player1 tg) M.armoredGuard
      _ <- stackEncounterDeck [S.breakinAndTakin]
      hpBefore <- getsGame $ unHp . V.villainHp . toAttrs . (HashMap.! defender) . gameVillains
      messages <- L.runQueueT $ L.capture $ CWHelp.leaderAttacksLeader attacker defender
      waiting <- openQuestion
      start messages
      openQuestion `shouldReturn` waiting
      hpAfter <- getsGame $ unHp . V.villainHp . toAttrs . (HashMap.! defender) . gameVillains
      liftIO $ hpBefore - hpAfter `shouldBe` 4
      getActivePlayerId `shouldReturn` player1 tg

  it "does not trigger Hunting Rebel Heroes for an Unregistered Super revealed on the opposite board" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let p = player1 tg
          q = player2 tg
      _ <- addSecondBoard tg
      changeForm p
      resolve [IdentityMessage q $ ChangedToForm A]
      old <- fromMaybe (error "missing local scheme") . headMay <$> getMainSchemesIn (CW.teamArea CW.Registration)
      resolve [ReplaceMainScheme old "56097b"]
      card <- encounterCard C.cw56106
      resolve [RevealEncounterCard q card]
      damageOf p `shouldReturn` 0
      damageOf q `shouldReturn` 0

  it "resolves Guerilla Warfare on an inactive board when Mass Increase stuns its leader during a cross-board attack" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let p = player1 tg
          q = player2 tg
      defender <- villainIdNow
      attacker <- addSecondBoard tg
      old <- fromMaybe (error "missing opposite main scheme") . headMay <$> getMainSchemesIn (CW.teamArea CW.Resistance)
      resolve [SetActivePlayer q,ReplaceMainScheme old "56201b",SetActivePlayer p,ReplaceVillain defender "57040" Swapped,VillainMessage defender SetVillainHp]
      sid <- fromMaybe (error "missing Guerilla Warfare") . headMay <$> getMainSchemesIn (CW.teamArea CW.Resistance)
      dense <- encounterCard C.cw57046a
      increase <- encounterCard C.cw57049
      resolve [RevealEncounterCard p dense,RevealEncounterCard p increase]
      _ <- stackEncounterDeck [M.hydraMercenary]
      messages <- L.runQueueT $ L.capture $ CWHelp.leaderAttacksLeader attacker defender
      start messages
      choose "prevent the leader attack with Mass Increase" (isAbilityOf $ AttachmentId $ unCardId $ ecCardId increase)
      choose "place threat on the inactive board's Guerilla Warfare" (isAbilityOf sid)
      pass
      getsGame (fmap Main.getMainSchemeThreat . lookup sid . gameMainSchemes) `shouldReturn` Just 1
      getCharacterStatusCount (VillainCharacter attacker) StunnedStatus `shouldReturn` 1

  it "resolves boost-card 'you' as the defending leader's first player" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      defender <- villainIdNow
      attacker <- addSecondBoard tg
      _ <- stackEncounterDeck [M.hydraHunter,S.breakinAndTakin]
      let threatOnBoard team = do
            schemes <- getMainSchemesIn (CW.teamArea team)
            getsGame $ \g -> sum [Main.getMainSchemeThreat scheme | sid <- schemes, Just scheme <- [lookup sid $ gameMainSchemes g]]
      ownThreat <- threatOnBoard CW.Registration
      otherThreat <- threatOnBoard CW.Resistance
      messages <- L.runQueueT $ L.capture $ CWHelp.leaderAttacksLeader attacker defender
      resolve messages
      threatOnBoard CW.Registration `shouldReturn` ownThreat
      threatOnBoard CW.Resistance `shouldReturn` (otherThreat + 1)
      getActivePlayerId `shouldReturn` player1 tg

  for_ [("answers a leader attack with retaliate",False,1),("respects ranged against a leader's retaliate",True,0)] $ \(description,ranged,expectedDamage) ->
    it description do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        attacker <- villainIdNow
        defender <- addSecondBoard tg
        _ <- stackEncounterDeck [S.breakinAndTakin]
        effects <- L.runQueueT $ L.capture do
          L.lastingModifiers defender (SourceEntity $ VillainRef defender) [KeywordModifier $ K.Retaliate 1] False
          when ranged $ L.lastingModifiers attacker (SourceEntity $ VillainRef attacker) [KeywordModifier K.Ranged] False
        resolve effects
        messages <- L.runQueueT $ L.capture $ CWHelp.leaderAttacksLeader attacker defender
        resolve messages
        villainDamageNow `shouldReturn` expectedDamage

  it "resolves Energy Channel across boards after the first leader attack finishes" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      attacker <- villainIdNow
      defender <- addSecondBoard tg
      energy <- encounterCard C.cw56098
      let aid = AttachmentId $ unCardId $ ecCardId energy
      resolve [SetActivePlayer (player2 tg),ReplaceVillain defender "56092" Swapped,VillainMessage defender SetVillainHp,RevealEncounterCard (player2 tg) energy,PlaceCounters (AttachmentRef aid) 3,SetActivePlayer (player1 tg)]
      selectList (AttachmentIs C.cw56098) `shouldReturn` []
      selectList (AttachmentWithId aid) `shouldReturn` [aid]
      selectList (AttachmentOnEnemy $ EnemyVillainId defender) `shouldReturn` [aid]
      _ <- stackEncounterDeck [M.hydraMercenary,M.hydraMercenary]
      before <- getsGame $ unHp . V.villainHp . toAttrs . (HashMap.! attacker) . gameVillains
      messages <- L.runQueueT $ L.capture $ CWHelp.leaderAttacksLeader attacker defender
      resolve messages
      after <- getsGame $ unHp . V.villainHp . toAttrs . (HashMap.! attacker) . gameVillains
      liftIO $ before - after `shouldBe` 3
      getCountersOn aid `shouldReturn` 0
      getActivePlayerId `shouldReturn` player1 tg
      getEnemyAttack (EnemyVillainId attacker) `shouldReturn` Nothing
      getEnemyAttack (EnemyVillainId defender) `shouldReturn` Nothing

  it "uses Mass Increase against a leader attacking Vision across the two boards" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      attacker <- villainIdNow
      defender <- addSecondBoard tg
      form <- encounterCard C.cw57046a
      mass <- encounterCard C.cw57049
      let aid = AttachmentId $ unCardId $ ecCardId mass
      resolve [SetActivePlayer (player2 tg),ReplaceVillain defender "57040" Swapped,VillainMessage defender SetVillainHp,RevealEncounterCard (player2 tg) form,RevealEncounterCard (player2 tg) mass,SetActivePlayer (player1 tg)]
      _ <- stackEncounterDeck [M.hydraMercenary]
      before <- getsGame $ unHp . V.villainHp . toAttrs . (HashMap.! defender) . gameVillains
      messages <- L.runQueueT $ L.capture $ CWHelp.leaderAttacksLeader attacker defender
      resolve messages
      after <- getsGame $ unHp . V.villainHp . toAttrs . (HashMap.! defender) . gameVillains
      liftIO $ after `shouldBe` before
      enemyMatches StunnedEnemy (EnemyVillainId attacker) `shouldReturn` True
      getAttachedCharacter aid `shouldReturn` Nothing
      resolve [RemoveStatus (VillainRef attacker) StunnedStatus]
      _ <- stackEncounterDeck [M.hydraMercenary]
      secondAttack <- L.runQueueT $ L.capture $ CWHelp.leaderAttacksLeader attacker defender
      resolve secondAttack
      getsGame (unHp . V.villainHp . toAttrs . (HashMap.! defender) . gameVillains) `shouldReturn` (before - 3)

  for_ [False,True] $ \eliminated ->
    it ("Self-propelled Glide protects a cross-board leader with its physical team eliminated: " <> show eliminated) do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        attacker <- villainIdNow
        defender <- addSecondBoard tg
        finesse <- encounterCard C.cw56174
        glide <- encounterCard C.cw56179
        let aid = AttachmentId $ unCardId $ ecCardId finesse
        resolve [SetActivePlayer (player2 tg),ReplaceVillain defender "56168" Swapped,VillainMessage defender SetVillainHp,RevealEncounterCard (player2 tg) finesse,PlaceCounters (AttachmentRef aid) 2,RevealEncounterCard (player2 tg) glide,SetActivePlayer (player1 tg)]
        when eliminated $ resolve [IdentityMessage (player2 tg) IdentityDefeated]
        saved <- getsGame toJSON
        case fromJSON @Game saved of
          Success restored -> withGame_ $ const restored
          Error reason -> liftIO $ expectationFailure reason
        _ <- stackEncounterDeck [M.hydraMercenary]
        before <- getsGame $ unHp . V.villainHp . toAttrs . (HashMap.! defender) . gameVillains
        messages <- L.runQueueT $ L.capture $ CWHelp.leaderAttacksLeader attacker defender
        resolve messages
        after <- getsGame $ unHp . V.villainHp . toAttrs . (HashMap.! defender) . gameVillains
        liftIO $ before - after `shouldBe` 1
        getCountersOn aid `shouldReturn` 2

  for_ [("56096b",M.hydraMercenary),("56141b",M.hydraMercenary),("56123b",S.breakinAndTakin),("56173b",S.breakinAndTakin)] $ \(code,selected) ->
    it (show code <> " lets the opponent choose a facedown card while searching and shuffling only the receiving team's deck") do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        let p = player1 tg
            q = player2 tg
        leader <- villainIdNow
        opponent <- addSecondBoard tg
        ownCards <- traverse encounterCard [selected,M.hydraSoldier]
        otherCards <- traverse encounterCard [M.hydraMercenary,M.hydraSoldier,M.hydraBomber]
        messages <- L.runQueueT $ L.capture $ do
          L.createDeck (L.VillainDeckKey leader) ownCards
          L.createDeck (L.VillainDeckKey opponent) otherCards
        resolve messages
        otherBefore <- asYou q getEncounterDeck
        old <- fromMaybe (error "missing receiving board scheme") . headMay <$> getMainSchemesIn (CW.teamArea CW.Registration)
        start [ReplaceMainScheme old code]
        fmap fst <$> asked `shouldReturn` Just q
        choose "the opponent chooses the facedown card" $ \case CardLabel candidate _ -> candidate == toCardCode selected;_ -> False
        map toCardCode . playerIdentityEncounterCards <$> playerAttrs p `shouldReturn` [toCardCode selected]
        playerIdentityEncounterCards <$> playerAttrs q `shouldReturn` []
        asYou q getEncounterDeck `shouldReturn` otherBefore
        remaining <- asYou p getEncounterDeck
        liftIO $ length remaining `shouldBe` 1

  it "lets the opponent choose Stark Tower's attachment from the defeated scheme's own encounter deck" do
    tg <- competitive ["black-widow","captain-marvel"]
    inGame tg do
      let you = player1 tg
      leader <- villainIdNow
      opponent <- addSecondBoard tg
      resolve [ReplaceVillain leader "56059" Swapped,VillainMessage leader SetVillainHp]
      ownCards <- traverse encounterCard [C.cw56065,C.cw56067,C.cw56068]
      otherCards <- traverse encounterCard [M.hydraMercenary,M.hydraSoldier,M.hydraBomber]
      messages <- L.runQueueT $ L.capture $ do
        L.createDeck (L.VillainDeckKey leader) ownCards
        L.createDeck (L.VillainDeckKey opponent) otherCards
      resolve messages
      otherBefore <- asYou (player2 tg) getEncounterDeck
      tower <- encounterCard C.cw56072
      resolve [RevealEncounterCard you tower]
      covert <- putInHand you "08003"
      resources <- traverse (putInHand you) ["01089","01093"]
      playWith covert resources
      choose "defeat Stark Tower" (isTargetChoice $ SideSchemeId $ unCardId $ ecCardId tower)
      pressWhenRevealed
      fmap fst <$> asked `shouldReturn` Just (player2 tg)
      choose "the opponent chooses Mark V Helmet" $ \case CardLabel code _ -> code == "56067"; _ -> False
      pass
      let helmet = ownCards !! 1
      getAttachedCharacter (AttachmentId $ unCardId $ ecCardId helmet) `shouldReturn` Just (VillainCharacter leader)
      asYou (player2 tg) getEncounterDeck `shouldReturn` otherBefore
      remaining <- asYou you getEncounterDeck
      liftIO $ length remaining `shouldBe` 2
      liftIO $ helmet `shouldSatisfy` (`notElem` remaining)

  it "keeps Iron Man's setup search and shuffle on his encounter deck when the opponent chooses" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      leader <- villainIdNow
      opponent <- addSecondBoard tg
      resolve [ReplaceVillain leader "56059" Swapped,VillainMessage leader SetVillainHp]
      ownCards <- traverse encounterCard [C.cw56065,C.cw56067,C.cw56068]
      otherCards <- traverse encounterCard [M.hydraMercenary,M.hydraSoldier,M.hydraBomber]
      messages <- L.runQueueT $ L.capture $ do
        L.createDeck (L.VillainDeckKey leader) ownCards
        L.createDeck (L.VillainDeckKey opponent) otherCards
      resolve messages
      otherBefore <- asYou (player2 tg) getEncounterDeck
      start [CardSpecific (VillainRef leader) "cwLeaderSetup" Null]
      fmap fst <$> asked `shouldReturn` Just (player2 tg)
      choose "the opponent chooses Mark V Helmet" $ \case CardLabel code _ -> code == "56067"; _ -> False
      pass
      asYou (player2 tg) getEncounterDeck `shouldReturn` otherBefore
      remaining <- asYou you getEncounterDeck
      liftIO $ length remaining `shouldBe` 2
      getAttachedCharacter (AttachmentId $ unCardId $ ecCardId $ ownCards !! 1) `shouldReturn` Just (VillainCharacter leader)

  it "spends stun instead of attacking the other leader" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      attacker <- villainIdNow
      defender <- addSecondBoard tg
      resolve [GiveStatus (VillainRef attacker) StunnedStatus]
      hpBefore <- getsGame $ unHp . V.villainHp . toAttrs . (HashMap.! defender) . gameVillains
      messages <- L.runQueueT $ L.capture $ CWHelp.leaderAttacksLeader attacker defender
      resolve messages
      hpAfter <- getsGame $ unHp . V.villainHp . toAttrs . (HashMap.! defender) . gameVillains
      liftIO $ hpAfter `shouldBe` hpBefore
      enemyMatches StunnedEnemy (EnemyVillainId attacker) `shouldReturn` False

  for_ [("retaliate first",98000),("the printed response first",1)] $ \(description,firstIndex) ->
    it ("orders " <> description <> " with Sebastian Shaw's forced response") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        changeForm you
        mid <- addMinion you M.sebastianShaw
        effects <- L.runQueueT $ L.capture $ L.lastingModifiers mid (MinionEntity $ MinionWithId mid) [KeywordModifier $ K.Retaliate 1] False
        resolve effects
        choose "make a basic attack" $ \case UseAbility a -> abilityIndex a == 301; _ -> False
        choose "attack Sebastian Shaw" (isTargetChoice mid)
        let indexed index = \case UseAbility a -> abilitySource a == MinionRef mid && abilityIndex a == index; _ -> False
        offers (indexed 98000) `shouldReturn` True
        offers (indexed 1) `shouldReturn` True
        choose "resolve the selected forced response" (indexed firstIndex)
        pass
        -- A lone keyword resolves automatically; the remaining printed response
        -- still needs its acknowledgement if retaliate was chosen first.
        when (firstIndex == 98000) $ choose "resolve Shaw" (indexed 1)
        pass
        damageOf you `shouldReturn` 1
        minionDamageOf mid `shouldReturn` Just 0
        getsGame (maybe False (minionTough . toAttrs) . lookup mid . gameMinions) `shouldReturn` False

  it "does not retroactively trigger retaliate gained during an after-attack response" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      mid <- addMinion you M.sebastianShaw
      choose "make a basic attack" $ \case UseAbility a -> abilityIndex a == 301; _ -> False
      choose "attack Sebastian Shaw" (isTargetChoice mid)
      effects <- L.runQueueT $ L.capture $ L.lastingModifiers mid (MinionEntity $ MinionWithId mid) [KeywordModifier $ K.Retaliate 1] False
      resolve effects
      choose "resolve Shaw" $ \case UseAbility a -> abilitySource a == MinionRef mid && abilityIndex a == 1; _ -> False
      pass
      damageOf you `shouldReturn` 0

  it "keeps an eliminated team's board separate during the reply" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      second <- addSecondBoard tg
      resolve [IdentityMessage (player2 tg) IdentityDefeated]
      competition <- getCompetition
      liftIO $ fmap (\c -> CW.teamOutcome c CW.Resistance) competition `shouldBe` Just (Just CW.AllHeroesDefeated)
      getsGame gameState `shouldReturn` InProgress
      getActiveVillainIn (CW.teamArea CW.Resistance) `shouldReturn` Just second
      getActiveVillainIn (CW.teamArea CW.Registration) >>= liftIO . (`shouldSatisfy` (/= Just second))
      (length <$> getAreas) `shouldReturn` 2

  it "allows an eliminated first player to make required enemy-team choices" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      resolve [IdentityMessage (player2 tg) IdentityDefeated]
      withGame_ $ questionL .~ mempty
      push $ Ask (player2 tg) $ QuestionLabel "$label.cwEnemyTeamChooses" Nothing $ ChooseOne [Label "$label.done" []]
      runGameMessages
      question <- openQuestion
      liftIO $ fmap fst question `shouldBe` Just (player2 tg)

  it "gives resistance its reply before resolving a registration victory" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      start [CardSpecific GameRef "cwFinishBoard" $ toJSON (CW.Registration,CW.EnemyLeaderDefeated)]
      getsGame gameState `shouldReturn` InProgress
      getPhasePlayers `shouldReturn` [player2 tg]
      start [CardSpecific GameRef "cwFinishBoard" $ toJSON (CW.Resistance,CW.EnemyLeaderDefeated)]
      getsGame gameState `shouldReturn` Finished CompetitiveDraw

  it "ends the winning player's turn, skips their teammate's queued turn, and completes both players' hand refill and readying" do
    tg <- competitive ["spider-man","she-hulk","captain-marvel","iron-man"]
    inGame tg do
      let p = player1 tg
          teammate = seats tg !! 1
          reply = seats tg !! 2
      changeForm p
      clearHand p
      clearHand teammate
      vid <- withLeader "56060"
      _ <- addSecondBoard tg
      withGame_ $ entitiesL . villainsL . ix vid %~ \(V.Villain leader) -> V.Villain $ V.overVillainAttrs (\a -> a {V.villainHp = HP 1}) leader
      messages <- L.runQueueT $ L.capture $ L.exhaust teammate
      resolve messages
      choose "make the winning basic attack" $ \case UseAbility ability -> abilitySource ability == IdentityRef p && abilityIndex ability == 301;_ -> False
      choose "defeat the final leader stage" (isTargetChoice vid)
      fmap fst <$> asked `shouldReturn` Just reply
      isExhaustedNow p `shouldReturn` False
      isExhaustedNow teammate `shouldReturn` False
      length <$> handOf p `shouldReturn` 5
      length <$> handOf teammate `shouldReturn` 6
      playerIdentityPassed <$> playerAttrs p `shouldReturn` True
      playerIdentityPassed <$> playerAttrs teammate `shouldReturn` True
      getPhasePlayers `shouldReturn` drop 2 (seats tg)
      getActiveVillainIn (CW.teamArea CW.Registration) `shouldReturn` Just vid

  it "skips all queued activations and encounter deals when the threat step completes a competitive main scheme" do
    tg <- competitive ["spider-man","she-hulk","captain-marvel","iron-man"]
    inGame tg do
      let p = player1 tg
          reply = seats tg !! 2
      _ <- addSecondBoard tg
      old <- fromMaybe (error "missing registration main scheme") . headMay <$> getMainSchemesIn (CW.teamArea CW.Registration)
      resolve [ReplaceMainScheme old "56124b"]
      sid <- fromMaybe (error "missing final main scheme") . headMay <$> getMainSchemesIn (CW.teamArea CW.Registration)
      resolve [MainSchemeMessage sid $ MainSchemePlaceThreat 16]
      _ <- stackEncounterDeck (replicate 12 M.hydraMercenary)
      endTurn
      endTurn
      changeForm reply
      clearHand reply
      endTurn
      endTurn
      let decline = whenM (offers isNoResponse) $ choose "decline optional responses" isNoResponse >> decline
      decline
      getsGame (fmap Main.getMainSchemeThreat . lookup sid . gameMainSchemes) `shouldReturn` Just 18
      playerIdentityEncounterCards <$> playerAttrs p `shouldReturn` []
      playerIdentityEncounterCards <$> playerAttrs (seats tg !! 1) `shouldReturn` []
      competition <- getCompetition
      liftIO $ fmap CW.competitivePhase competition `shouldBe` Just CW.ResistanceVillainPhase
      liftIO $ fmap (\c -> CW.teamOutcome c CW.Registration) competition `shouldBe` Just (Just CW.MainSchemeCompleted)
      fmap fst <$> asked `shouldReturn` Just reply

  it "drops the remaining phase minion-activation question when Energy Barrier defeats the enemy leader during a minion attack" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let p = player1 tg
      changeForm p
      vid <- withLeader "56060"
      withGame_ $ entitiesL . villainsL . ix vid %~ \(V.Villain leader) -> V.Villain $ V.overVillainAttrs (\a -> a {V.villainHp = HP 1}) leader
      barrier <- putIntoPlay p "05017"
      firstCard <- encounterCard M.weaponsRunner
      resolve [PutEncounterCardIntoPlay p firstCard]
      let first = minionOf firstCard
      _ <- addMinion p M.hydraMercenary
      _ <- stackEncounterDeck (replicate 12 M.hydraMercenary)
      endTurn
      endTurn
      let decline = whenM (offers isNoResponse) $ choose "decline optional responses" isNoResponse >> decline
      decline
      choose "defend the villain attack" isDefend
      decline
      choose "activate the first minion" (isTargetChoice first)
      chooseIf (isLabel "$label.noDefender")
      choose "use Energy Barrier" (isAbilityOf $ upgradeIdOf barrier)
      choose "redirect the damage to the leader" (isTargetChoice vid)
      pass
      getsGame gameState `shouldReturn` Finished RegistrationWon
      damageOf p `shouldReturn` 0
      playerIdentityEncounterCards <$> playerAttrs p `shouldReturn` []

  it "passes only the active team's first-player token" do
    tg <- competitive ["spider-man","she-hulk","captain-marvel","iron-man"]
    inGame tg do
      resolve [PassFirstPlayer]
      let ownNext = seats tg !! 1
          opposingFirst = seats tg !! 2
      selectJust FirstPlayer `shouldReturn` ownNext
      asYou opposingFirst (selectJust FirstPlayer) `shouldReturn` opposingFirst
      competition <- getCompetition
      liftIO $ fmap (\c -> CW.teamFirstPlayer c CW.Registration) competition `shouldBe` Just ownNext
      liftIO $ fmap (\c -> CW.teamFirstPlayer c CW.Resistance) competition `shouldBe` Just opposingFirst

  it "keeps broad enemy and identity matches local, while allowing explicit references" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      otherLeader <- addSecondBoard tg
      enemyMatches AnyEnemy (EnemyVillainId otherLeader) `shouldReturn` False
      enemyMatches (EnemyWithId $ EnemyVillainId otherLeader) (EnemyVillainId otherLeader) `shouldReturn` True
      selectList (EnemyWithId $ EnemyVillainId otherLeader) `shouldReturn` [EnemyVillainId otherLeader]
      selectList (VillainWithId otherLeader) `shouldReturn` [otherLeader]
      identityMatches AnyIdentity (player2 tg) `shouldReturn` False
      identityMatches (IdentityWithId $ player2 tg) (player2 tg) `shouldReturn` True

  it "allows named cards and schemes across boards without widening ordinary queries" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      _ <- addSecondBoard tg
      ally <- putIntoPlay (player2 tg) "01084"
      upgrade <- putIntoPlay (player2 tg) "01093"
      minion <- addMinion (player2 tg) M.hydraMercenary
      scheme <- encounterCard S.crowdControl
      resolve [SetActivePlayer (player2 tg),RevealEncounterCard (player2 tg) scheme,SetActivePlayer (player1 tg)]
      selectList AnyAlly `shouldReturn` []
      selectList (AllyWithId $ allyIdOf ally) `shouldReturn` [allyIdOf ally]
      selectList (UpgradeNamed "Tenacity") `shouldReturn` []
      selectList (UpgradeWithId $ upgradeIdOf upgrade) `shouldReturn` [upgradeIdOf upgrade]
      selectList AnyMinion `shouldReturn` []
      selectList (MinionWithId minion) `shouldReturn` [minion]
      let sid = SideSchemeId $ unCardId $ ecCardId scheme
      selectList (SideSchemeWithId sid) `shouldReturn` [sid]
      mains <- getMainSchemesIn (CW.teamArea CW.Resistance)
      for_ mains $ \mid -> selectList (SchemeWithId $ SchemeMainSchemeId mid) `shouldReturn` [SchemeMainSchemeId mid]

  it "does not offer an opposing hero's basic powers" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      _ <- addSecondBoard tg
      -- The opposing hero's basic powers and form change are never offered here.
      abilities <- getAbilitiesFor (player1 tg)
      liftIO $ map abilitySource abilities `shouldSatisfy` notElem (IdentityRef $ player2 tg)

  it "keeps supports and upgrades in their controller's game area" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      qSupport <- putIntoPlay (player2 tg) "01091"
      qUpgrade <- putIntoPlay (player2 tg) "01093"
      selectListCount (SupportControlledBy AnyIdentity) `shouldReturn` 0
      upgradeMatches (UpgradeWithId $ upgradeOf qUpgrade) (upgradeOf qUpgrade) `shouldReturn` True
      asYou (player2 tg) (selectListCount $ SupportControlledBy $ IdentityWithId $ player2 tg) `shouldReturn` 1
      -- The normal player-facing support list excludes the opponent's card.
      available <- selectList $ SupportControlledBy AnyIdentity
      liftIO $ available `shouldSatisfy` notElem (SupportId $ unCardId $ pcCardId qSupport)

  it "does not apply a side scheme's enemy attack bonus to the opposite board" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      here <- villainIdNow
      other <- addSecondBoard tg
      _ <- revealNow (player1 tg) S.soundTheAlarms
      getModifiers (VillainRef here) >>= (`shouldSatisfy` elem (AttackModifier 1))
      getModifiers (VillainRef other) >>= (`shouldSatisfy` notElem (AttackModifier 1))

  it "keeps Lead from the Front's character bonuses on the acting team's board" do
    tg <- competitive ["spider-man","captain-marvel"]
    let p = player1 tg
    inGame tg do
      _ <- addSecondBoard tg
      changeForm p
      pass
      friendly <- putIntoPlay p "01084"
      opponent <- putIntoPlay (player2 tg) "01083"
      event <- putInHand p "01070"
      resources <- putInHand p "01088"
      playWith event [resources]
      pass
      getModifiers (AllyRef $ AllyId $ unCardId $ pcCardId friendly) >>= (`shouldSatisfy` elem (AttackModifier 1))
      getModifiers (AllyRef $ AllyId $ unCardId $ pcCardId opponent) >>= (`shouldSatisfy` notElem (AttackModifier 1))

  it "does not let an enemy version of Nick Fury prevent the ally from entering play" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      vid <- villainIdNow
      withGame_ $ entitiesL . villainsL . ix vid %~ \(V.Villain v) -> V.Villain $ V.overVillainAttrs
        (\a -> a {V.villainCardDef = (V.villainCardDef a) {cdName = "Nick Fury",cdUnique = True}}) v
      let p = player1 tg
      clearHand p
      card <- putInHand p "01084"
      energy <- putInHand p "01088"
      genius <- putInHand p "01089"
      offers (isPlay card) `shouldReturn` True
      playWith card [energy,genius]
      choose "resolve Nick Fury" (isAbilityOf $ allyIdOf card)
      choose "draw three cards" (isLabel $ countLabel "drawCards" 3)
      length <$> alliesWith "01084" `shouldReturn` 1

  for_ [("two",["spider-man","captain-marvel"],7,1),("four",["spider-man","captain-marvel","black-panther","she-hulk"],14,2)] $ \(description,heroes,threshold,acceleration) ->
    it ("publishes threat targets and acceleration for each team in a " <> description <> " player competition") do
      tg <- competitive heroes
      inGame tg do
        _ <- addSecondBoard tg
        view <- liftIO . publicGameFor (const True) =<< getGame
        let stats = [(target,rate) | Object root <- [view],Just (Array schemes) <- [KeyMap.lookup "mainSchemes" root],Object scheme <- toList schemes,Just target <- [KeyMap.lookup "targetThreat" scheme],Just rate <- [KeyMap.lookup "escalationThreat" scheme]]
        liftIO $ stats `H.shouldMatchList` [(toJSON (threshold :: Natural),toJSON (acceleration :: Natural)),(toJSON (6 * acceleration),toJSON acceleration)]

  it "prevents playing a unique ally that matches the friendly leader" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      vid <- addSecondBoard tg
      withGame_ $ entitiesL . villainsL . ix vid %~ \(V.Villain v) -> V.Villain $ V.overVillainAttrs
        (\a -> a {V.villainCardDef = (V.villainCardDef a) {cdName = "Nick Fury",cdUnique = True}}) v
      card <- putInHand (player1 tg) "01084"
      _ <- putInHand (player1 tg) "01088"
      _ <- putInHand (player1 tg) "01089"
      offers (isPlay card) `shouldReturn` False

  it "has exactly five physical Standard PvP cards despite the duplicate printings" do
    let total = sum [fromMaybe 0 (cdEncounterSetQuantity def) | def <- C.civilWarDefinitions, cdEncounterSet def == Just E.CwStandardPvp]
    total `H.shouldBe` 5

  for_ (CW.registrationModulars <> CW.resistanceModulars) $ \set ->
    it ("has all five printed cards in " <> show set) do
      sum [fromMaybe 0 (cdEncounterSetQuantity def) | def <- C.civilWarDefinitions, cdEncounterSet def == Just set] `H.shouldBe` 5

  it "does not reveal private search choices to opponents or spectators" do
    tg <- competitive ["spider-man","captain-marvel"]
    let p = player1 tg
    inGame tg do
      card <- putInHand p "01070"
      withGame_ $ questionL .~ HashMap.singleton p (Searched [(Nothing,[PlayerCard card])] $ ChooseOne [CardLabel "01070" $ Label "$label.done" []])
      game <- getGame
      private <- liftIO $ publicGameFor (== p) game
      public <- liftIO $ publicGameFor (const False) game
      let choicesShown value = [length xs | Object root <- [value], Just (Object questions) <- [KeyMap.lookup "question" root], Object question <- KeyMap.elems questions, Just (Array xs) <- [KeyMap.lookup "choices" question]]
          searchesShown value = [length xs | Object root <- [value], Just (Object questions) <- [KeyMap.lookup "question" root], Object question <- KeyMap.elems questions, Just (Array xs) <- [KeyMap.lookup "searched" question]]
      liftIO $ choicesShown private `shouldBe` [1]
      liftIO $ searchesShown private `shouldBe` [1]
      liftIO $ choicesShown public `shouldBe` [0]
      liftIO $ searchesShown public `shouldBe` [0]

  it "preserves the team size and configuration when the game is saved and loaded" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      game <- getGame
      case fromJSON (toJSON game) of
        Error err -> liftIO $ expectationFailure err
        Success restored -> do
          liftIO $ gameCompetition restored `shouldBe` gameCompetition game
          liftIO $ startingPlayerCountIn restored `shouldBe` 1
          view <- liftIO $ publicGameFor (const True) restored
          void $ liftIO $ evaluate (length $ show $ toJSON view)
