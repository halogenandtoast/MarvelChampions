module Marvel.Rules.AgeOfApocalypseMissionSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.List (findIndex)
import Data.Text qualified as T
import Marvel.AgeOfApocalypse.Mission (missionResources)
import Marvel.AgeOfApocalypse.MissionTypes
import Marvel.Ally.Types qualified as Ally
import Marvel.Environment.Cards qualified as Environments
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Query
import Marvel.Support.Cards qualified as Supports
import TestImport

mission :: GameAppT MissionState
mission = getMission >>= maybe (error "no mission") pure

callback :: ToJSON a => Text -> a -> GameAppT ()
callback key value = resolve [CardSpecific GameRef key (toJSON value)]

beginMission :: CardCode -> CardCode -> GameAppT ()
beginMission code overseer = callback "aoaMissionSetup" (code,overseer)

playMissionAlly :: IdentityId -> CardCode -> GameAppT PlayerCard
playMissionAlly you code = do
  clearHand you
  card <- putInHand you code
  energy <- putInHand you "01088"
  genius <- putInHand you "01089"
  choose "play ally" (isPlay card)
  choose "send ally to mission" (isLabel "$label.aoaAllyMission")
  for_ [energy,genius] $ \resource -> whenM (offers $ isPayWith resource) $ choose "pay resource" (isPayWith resource)
  payWith []
  pass
  pure card

topCards :: IdentityId -> [CardCode] -> GameAppT [PlayerCard]
topCards you codes = do
  cards <- traverse (putInHand you) codes
  withGame_ $ entitiesL . playersL . ix you %~ \(PlayerIdentity a) -> PlayerIdentity a
    {playerIdentityDeck = Deck (cards <> unDeck a.playerIdentityDeck)
    ,playerIdentityHand = Hand (filter ((`notElem` map pcCardId cards) . pcCardId) $ unHand a.playerIdentityHand)}
  pure cards

attempt :: IdentityId -> GameAppT ()
attempt _you = do
  team <- selectJust (SupportNamed $ cdName Supports.missionTeamFront)
  choose "Mission Team action" (isAbilityOf $ SupportRef team)
  choose "attempt the mission" (isLabel "$label.aoaMissionAttempt")

missionMinion :: GameAppT MinionId
missionMinion = mission >>= maybe (error "no mission minion") pure . headMay . missionMinions

spec :: Spec
spec = describe "Age of Apocalypse mission rules" do
  it "starts with separate mission entities and a first-player Mission Team" do
    tg <- testGame ["spider-man"]
    inGame tg do
      beginMission "45166a" "45179a"
      m <- mission
      length m.missionMinions `shouldBe` 1
      sideSchemeThreat m.missionScheme `shouldReturn` 5
      m.missionSettingUp `shouldBe` False
      selectList AnyMinion `shouldReturn` []
      selectList AnySideScheme `shouldReturn` []
      selectAny (SupportNamed $ cdName Supports.missionTeamFront) `shouldReturn` True

  it "scales the Overseer's printed five hit points per player" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      beginMission "45166a" "45179a"
      mid <- missionMinion
      getEnemyRemainingHitPoints (EnemyMinionId mid) `shouldReturn` 10
      m <- mission
      sideSchemeThreat m.missionScheme `shouldReturn` 10

  it "plays an ally to the mission before its printed enters-play response" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      beginMission "45166a" "45179a"
      card <- playMissionAlly you "01084"
      m <- mission
      m.missionAllies `shouldBe` [allyIdOf card]
      length <$> handOf you >>= (`shouldBe` 0)
      getSourceController (AllyRef $ allyIdOf card) `shouldReturn` Nothing
      selectList (AllyControlledBy $ IdentityWithId you) `shouldReturn` []
      modifiers <- getModifiers (AllyRef $ allyIdOf card)
      isBlanked (AllyRef $ allyIdOf card) `shouldReturn` True
      modifiers `shouldSatisfy` notElem CannotTakeDamage

  it "does not offer mission allies as ordinary action or attack targets" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      beginMission "45166a" "45179a"
      card <- playMissionAlly you "01050"
      selectList AnyAlly `shouldReturn` []
      abilities <- getAbilitiesFor you
      map abilitySource abilities `shouldSatisfy` notElem (AllyRef $ allyIdOf card)
      before <- getsGame (Ally.allyDamage . toAttrs . fromMaybe (error "missing") . HashMap.lookup (allyIdOf card) . gameAllies)
      messages <- L.runQueueT $ L.capture $ L.damageCharacter (allyIdOf card) you (toDamage 1 FromAbility)
      resolve messages
      getsGame (Ally.allyDamage . toAttrs . fromMaybe (error "missing") . HashMap.lookup (allyIdOf card) . gameAllies) `shouldReturn` before

  it "allows an attached upgrade to improve mission stats and adds its wild icon" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      beginMission "45166a" "45179a"
      ally <- playMissionAlly you "01002"
      upgrade <- putInHand you "45176"
      resource <- putInHand you "01088"
      playOn upgrade (AllyRef $ allyIdOf ally) [resource]
      resolve []
      getMissionAllyStats (allyIdOf ally) `shouldReturn` (2,2)
      missionResources (allyIdOf ally) `shouldReturn` [Energy,Wild]
      other <- putInHand you "45176"
      targets <- getPlayTargets you other
      fromMaybe [] targets `shouldSatisfy` notElem (AllyRef $ allyIdOf ally)

  it "matches a discarded icon, deals damage, thwarts, then damages allies and counts attempts" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      beginMission "45166a" "45182a"
      ally <- playMissionAlly you "01002"
      _ <- topCards you ["01088"]
      attempt you
      choose "assign energy" (isTargetChoice $ AllyRef $ allyIdOf ally)
      mid <- missionMinion
      choose "damage overseer" (isTargetChoice $ enemyRef $ EnemyMinionId mid)
      resolve []
      minionDamageOf mid `shouldReturn` Just 1
      m <- mission
      sideSchemeThreat m.missionScheme `shouldReturn` 4
      m.missionAttempts `shouldBe` 1
      getsGame (Ally.allyDamage . toAttrs . fromMaybe (error "missing") . HashMap.lookup (allyIdOf ally) . gameAllies) `shouldReturn` 1

  it "a nonmatching assigned icon contributes neither ATK nor THW" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      beginMission "45166a" "45182a"
      ally <- playMissionAlly you "01002"
      _ <- topCards you ["01089"]
      attempt you
      choose "assign mental" (isTargetChoice $ AllyRef $ allyIdOf ally)
      resolve []
      mid <- missionMinion
      minionDamageOf mid `shouldReturn` Just 0
      m <- mission
      sideSchemeThreat m.missionScheme `shouldReturn` 5
      m.missionAttempts `shouldBe` 1

  it "wild discarded icons match an ally's printed energy icon" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      beginMission "45166a" "45182a"
      ally <- playMissionAlly you "01002"
      _ <- topCards you ["01055"]
      attempt you
      choose "assign wild" (isTargetChoice $ AllyRef $ allyIdOf ally)
      mid <- missionMinion
      choose "damage overseer" (isTargetChoice $ enemyRef $ EnemyMinionId mid)
      resolve []
      minionDamageOf mid `shouldReturn` Just 1

  it "Shadow King adds two threat for every discarded mental resource icon" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      beginMission "45166a" "45180a"
      ally <- playMissionAlly you "01002"
      _ <- topCards you ["01089"]
      attempt you
      choose "assign nonmatching mental" (isTargetChoice $ AllyRef $ allyIdOf ally)
      resolve []
      m <- mission
      sideSchemeThreat m.missionScheme `shouldReturn` 9

  it "Abyss holds discarded wild cards facedown instead of assigning them" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      beginMission "45166a" "45181a"
      _ <- playMissionAlly you "01002"
      cards <- topCards you ["01055"]
      attempt you
      resolve []
      mid <- missionMinion
      held <- getZoneHeldCards (MinionRef mid)
      map pcCardId held `shouldBe` map pcCardId cards
      m <- mission
      sideSchemeThreat m.missionScheme `shouldReturn` 5
      m.missionAttempts `shouldBe` 1

  it "Sugar Man heals three per physical icon before allies contribute damage" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      beginMission "45166a" "45182a"
      ally <- playMissionAlly you "01002"
      mid <- missionMinion
      m <- mission
      messages <- L.runQueueT $ L.capture $ L.dealDamage mid (SideSchemeRef m.missionScheme) (toDamage 4 FromAbility)
      resolve messages
      _ <- topCards you ["01090"]
      attempt you
      choose "assign physical" (isTargetChoice $ AllyRef $ allyIdOf ally)
      resolve []
      minionDamageOf mid `shouldReturn` Just 0

  it "Mikhail deals one mission ally damage for each energy icon before assignment" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      beginMission "45166a" "45183a"
      ally <- playMissionAlly you "01002"
      _ <- topCards you ["01088"]
      attempt you
      resolve []
      getsGame (HashMap.member (allyIdOf ally) . gameAllies) `shouldReturn` False
      m <- mission
      m.missionAllies `shouldBe` []
      sideSchemeThreat m.missionScheme `shouldReturn` 5

  it "Mister Sinister prevents assigning two cards with the same printed resource icon" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      beginMission "45166a" "45179a"
      first <- playMissionAlly you "01002"
      _ <- playMissionAlly you "01084"
      _ <- topCards you ["01055","01072"]
      attempt you
      choose "assign first wild" (isTargetChoice $ AllyRef $ allyIdOf first)
      mid <- missionMinion
      choose "damage Sinister" (isTargetChoice $ enemyRef $ EnemyMinionId mid)
      resolve []
      minionDamageOf mid `shouldReturn` Just 1
      m <- mission
      sideSchemeThreat m.missionScheme `shouldReturn` 4

  it "an Agent added to the mission protects the Overseer and leaves player engagement" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      beginMission "45166a" "45179a"
      overseer <- missionMinion
      card <- revealNow you Minions.agentOfApocalypse
      choose "add Agent" (isLabel "$label.aoaAddAgentToMission")
      resolve []
      m <- mission
      m.missionMinions `shouldSatisfy` elem (minionOf card)
      selectList AnyMinion `shouldReturn` []
      getModifiers (MinionRef overseer) >>= (`shouldSatisfy` elem CannotTakeDamage)
      attrs <- playerAttrs you
      toList attrs.playerIdentityMinions `shouldSatisfy` notElem (minionOf card)

  it "four unsuccessful attempts flip the mission, remove Mission Team, and apply failure" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      beginMission "45166a" "45179a"
      team <- selectJust (SupportNamed $ cdName Supports.missionTeamFront)
      before <- mainSchemeThreat
      for_ [1 .. 4 :: Int] $ \_ -> do
        resolve [SupportMessage team ReadiedSupport]
        attempt you
        resolve []
      m <- mission
      m.missionFinished `shouldBe` True
      m.missionSucceeded `shouldBe` False
      m.missionAttempts `shouldBe` 4
      getsGame (HashMap.member team . gameSupports) `shouldReturn` False
      getsGame (HashMap.member m.missionScheme . gameSideSchemes) `shouldReturn` False
      mainSchemeThreat `shouldReturn` before + 2

  it "Mission Team's discount only applies to the next mission ally this phase" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      beginMission "45166a" "45179a"
      team <- selectJust (SupportNamed $ cdName Supports.missionTeamFront)
      choose "Mission Team action" (isAbilityOf $ SupportRef team)
      choose "discount" (isLabel "$label.aoaMissionDiscount")
      clearHand you
      ally <- putInHand you "01002"
      choose "play discounted ally" (isPlay ally)
      offers (isLabel "$label.aoaAllyOwnArea") `shouldReturn` False
      choose "send discounted ally to mission" (isLabel "$label.aoaAllyMission")
      payWith []
      resolve []
      m <- mission
      m.missionAllies `shouldBe` [allyIdOf ally]
      m.missionDiscount `shouldBe` False

  it "an unused mission discount expires at the end of the phase" do
    tg <- testGame ["spider-man"]
    inGame tg do
      beginMission "45166a" "45179a"
      callback "aoaMissionDiscount" ()
      callback "aoaMissionEndPhase" ()
      m <- mission
      m.missionDiscount `shouldBe` False

  it "Mikhail's two energy icons damage one chosen ally once, without splitting targets" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      beginMission "45166a" "45183a"
      cat <- playMissionAlly you "01002"
      hulk <- playMissionAlly you "01050"
      _ <- topCards you ["01088","01089"]
      attempt you
      choose "deal the whole Mikhail instance to Hulk" (isTargetChoice $ AllyRef $ allyIdOf hulk)
      openQuestion >>= \case
        Just (_, QuestionLabel prompt _ _) -> prompt `shouldBe` "$label.aoaAssignMissionCard"
        asked' -> error $ "expected assignment after one Mikhail damage choice: " <> show asked'
      getsGame (Ally.allyDamage . toAttrs . fromMaybe (error "missing Hulk") . HashMap.lookup (allyIdOf hulk) . gameAllies) `shouldReturn` 2
      getsGame (Ally.allyDamage . toAttrs . fromMaybe (error "missing Black Cat") . HashMap.lookup (allyIdOf cat) . gameAllies) `shouldReturn` 0
      choose "assign energy to Black Cat" (isTargetChoice $ AllyRef $ allyIdOf cat)
      choose "assign mental to Hulk" (isTargetChoice $ AllyRef $ allyIdOf hulk)
      mid <- missionMinion
      choose "damage Overseer" (isTargetChoice $ enemyRef $ EnemyMinionId mid)
      resolve []
      getsGame (Ally.allyDamage . toAttrs . fromMaybe (error "missing Hulk") . HashMap.lookup (allyIdOf hulk) . gameAllies) `shouldReturn` 3
      getsGame (Ally.allyDamage . toAttrs . fromMaybe (error "missing Black Cat") . HashMap.lookup (allyIdOf cat) . gameAllies) `shouldReturn` 1

  it "Shadow King counts only Power of the Mind's one printed mental icon" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      beginMission "45166a" "45180a"
      ally <- playMissionAlly you "01002"
      _ <- topCards you ["40028"]
      attempt you
      choose "assign printed mental" (isTargetChoice $ AllyRef $ allyIdOf ally)
      resolve []
      m <- mission
      sideSchemeThreat m.missionScheme `shouldReturn` 7

  it "Mikhail counts only Power of Flight's one printed energy icon" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      beginMission "45166a" "45183a"
      ally <- playMissionAlly you "01002"
      _ <- topCards you ["42022"]
      attempt you
      getsGame (Ally.allyDamage . toAttrs . fromMaybe (error "conditional icon incorrectly defeated Black Cat") . HashMap.lookup (allyIdOf ally) . gameAllies) `shouldReturn` 1
      choose "assign the printed energy" (isTargetChoice $ AllyRef $ allyIdOf ally)
      mid <- missionMinion
      choose "damage Mikhail" (isTargetChoice $ enemyRef $ EnemyMinionId mid)
      resolve []
      minionDamageOf mid `shouldReturn` Just 1

  it "Abyss neither holds nor matches Band Together's conditional wild resource" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      beginMission "45166a" "45181a"
      ally <- playMissionAlly you "01002"
      _ <- topCards you ["21018"]
      attempt you
      choose "assign a card without printed icons" (isTargetChoice $ AllyRef $ allyIdOf ally)
      resolve []
      mid <- missionMinion
      getZoneHeldCards (MinionRef mid) `shouldReturn` []
      minionDamageOf mid `shouldReturn` Just 0
      m <- mission
      sideSchemeThreat m.missionScheme `shouldReturn` 5

  it "Sinister's assignment restriction ignores a previous card's conditional wild generation" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      beginMission "45166a" "45179a"
      cat <- playMissionAlly you "01002"
      fury <- playMissionAlly you "01084"
      _ <- topCards you ["21018","01072"]
      attempt you
      choose "assign no printed icons to Black Cat" (isTargetChoice $ AllyRef $ allyIdOf cat)
      choose "assign printed wild to Nick Fury" (isTargetChoice $ AllyRef $ allyIdOf fury)
      mid <- missionMinion
      choose "damage Sinister" (isTargetChoice $ enemyRef $ EnemyMinionId mid)
      choose "deal both damage" (isLabel $ L.countLabel "aoaMissionDamageAmount" 2)
      resolve []
      minionDamageOf mid `shouldReturn` Just 2
      m <- mission
      sideSchemeThreat m.missionScheme `shouldReturn` 3

  it "success shuffles the ally and its attached player upgrade before flipping and removing the mission" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      beginMission "45166a" "45179a"
      ally <- playMissionAlly you "01002"
      upgrade <- putInHand you "45176"
      resource <- putInHand you "01088"
      playOn upgrade (AllyRef $ allyIdOf ally) [resource]
      resolve []
      m <- mission
      mid <- missionMinion
      damage <- L.runQueueT $ L.capture $ L.dealDamage mid (SideSchemeRef m.missionScheme) (toDamage 5 FromAbility)
      resolve damage
      liftIO $ writeIORef tg.ran []
      resolve [SideSchemeMessage m.missionScheme (SideSchemeThreatRemoved (SideSchemeRef m.missionScheme) 5)]
      attrs <- playerAttrs you
      let deckIds = map pcCardId $ unDeck attrs.playerIdentityDeck
          discardedIds = map pcCardId $ unDiscard attrs.playerIdentityDiscard
      for_ [ally,upgrade] $ \card -> do
        length (filter (== pcCardId card) deckIds) `shouldBe` 1
        discardedIds `shouldSatisfy` notElem (pcCardId card)
      getsGame (HashMap.member (allyIdOf ally) . gameAllies) `shouldReturn` False
      getsGame (HashMap.member (upgradeOf upgrade) . gameUpgrades) `shouldReturn` False
      getsGame (HashMap.member m.missionScheme . gameSideSchemes) `shouldReturn` False
      trace <- reverse <$> liftIO (readIORef tg.ran)
      let shuffled = findIndex (T.isInfixOf "ShuffleIntoIdentityDeck") trace
          flipped = findIndex (T.isInfixOf "aoaMissionFlip") trace
      (shuffled,flipped) `shouldSatisfy` (\case (Just before,Just after) -> before < after; _ -> False)

  it "failure removes the ally and Abyss's held card instead of discarding either" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      beginMission "45166a" "45181a"
      ally <- playMissionAlly you "01002"
      held <- topCards you ["01055"]
      attempt you
      resolve []
      callback "aoaMissionFinish" False
      attrs <- playerAttrs you
      let remaining = map pcCardId $ unDeck attrs.playerIdentityDeck <> unHand attrs.playerIdentityHand <> unDiscard attrs.playerIdentityDiscard
      for_ (ally : held) $ \card -> remaining `shouldSatisfy` notElem (pcCardId card)
      getsGame (HashMap.member (allyIdOf ally) . gameAllies) `shouldReturn` False
      m <- mission
      m.missionMinions `shouldBe` []
      getsGame (HashMap.member m.missionScheme . gameSideSchemes) `shouldReturn` False

  it "outside Crime and Back-Alley Enclave modifiers cannot change mission ally stats" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      beginMission "45166a" "45179a"
      ally <- playMissionAlly you "01002"
      crime <- encounterCard Environments.mojo39035
      enclave <- encounterCard Environments.hood24060
      resolve [PutEncounterCardIntoPlay you crime,PutEncounterCardIntoPlay you enclave]
      getMissionAllyStats (allyIdOf ally) `shouldReturn` (1,1)
      upgrade <- putInHand you "45176"
      resource <- putInHand you "01088"
      playOn upgrade (AllyRef $ allyIdOf ally) [resource]
      resolve []
      getMissionAllyStats (allyIdOf ally) `shouldReturn` (2,2)

  it "player elimination removes their mission ally and cards held by Abyss" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      beginMission "45166a" "45181a"
      ally <- playMissionAlly you "01002"
      _ <- topCards you ["01055"]
      attempt you
      resolve []
      mid <- missionMinion
      held <- getZoneHeldCards (MinionRef mid)
      length held `shouldBe` 1
      resolve [IdentityMessage you IdentityDefeated]
      getsGame (HashMap.member (allyIdOf ally) . gameAllies) `shouldReturn` False
      getZoneHeldCards (MinionRef mid) `shouldReturn` []
      m <- mission
      m.missionAllies `shouldBe` []
      getPlayers `shouldReturn` [player2 tg]
