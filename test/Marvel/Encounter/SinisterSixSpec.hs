module Marvel.Encounter.SinisterSixSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Modifier
import Marvel.Message.Lifted qualified as L
import Marvel.SideScheme.Types qualified as Side
import Marvel.Villain.Villains.SmSinisterSix qualified as Six
import Control.Monad (replicateM_)
import TestImport

settle :: GameAppT ()
settle = go (150 :: Int)
 where
  go 0 = error "Sinister Six did not settle"
  go n = openQuestion >>= \case
    Just (_, QuestionLabel "$label.yourTurn" _ _) -> pure ()
    Just (you, q) -> do
      let passing c = isNoResponse c || isNoDefender c || isLabel "$label.declineToDefend" c
      answerQuestion you $ fromMaybe 0 $ headMay [i | (i,c) <- zip [0..] (choicesOf q), passing c]
      go (n-1)
    Nothing -> pure ()

light :: GameAppT SideSchemeId
light = getsGame $ \g -> fromMaybe (error "Light missing") $ headMay
  [sid | (sid, s) <- HashMap.toList (gameSideSchemes g), toCardCode (getCardDef s) `elem` ["27102a", "27102b"]]

lightCode :: GameAppT CardCode
lightCode = do
  sid <- light
  getsGame $ \g -> maybe (error "Light missing") (toCardCode . getCardDef) $ HashMap.lookup sid (gameSideSchemes g)

sixGame :: Difficulty -> [Text] -> IO TestGame
sixGame difficulty heroes = do
  let setup = (fromMaybe (error "Six setup missing") $ recommendedSetup "27100") {difficulty = difficulty}
  (app, ran, logged) <- dealtGameLogged setup (map starter heroes) 1
  seats <- runGameApp app $ getsGame gameSeats
  let tg = TestGame app ran logged seats
  inGame tg toFirstTurn
  pure tg

clearLight :: IdentityId -> GameAppT ()
clearLight you = do
  sid <- light
  n <- sideSchemeThreat sid
  runNow [SideSchemeMessage sid $ SideSchemeThreatRemoved (toSource you) n]
  settle

spec :: Spec
spec = describe "Sinister Six scenario" do
  for_ [Normal, Expert] \difficulty ->
    it ("offers encounter-card cancellation only when allowed: " <> show difficulty) do
      tg <- sixGame difficulty ["spider-man"]
      let p = player1 tg
      inGame tg do
        widow <- putIntoPlay p "01075"
        _ <- putInHand p "01089"
        _ <- revealNow p Treacheries.smFrequentFlyers
        offers (\case UseAbility a -> abilitySource a == toSource (AllyRef $ allyIdOf widow) && abilityIndex a == 1; _ -> False) `shouldReturn` (difficulty == Normal)

  for_ [(1, ["spider-man"]), (2, ["spider-man", "captain-marvel"])] \(players, heroes) -> do
    it ("starts with one more villain than players: " <> show players) do
      tg <- sixGame Normal heroes
      inGame tg do
        present <- Six.sixVillains
        liftIO $ length present `shouldBe` players + 1
        light >>= (\sid -> sideSchemeThreat sid `shouldReturn` (10 + fromIntegral players * 10))
        lightCode `shouldReturn` "27102a"
    for_ [Normal, Expert] \difficulty -> do
      it ("places Chase starting threat and carries Ambush threat: " <> show (players, difficulty)) do
        tg <- sixGame difficulty heroes
        inGame tg do
          clearLight (player1 tg)
          lightCode `shouldReturn` "27102b"
          sid <- light
          sideSchemeThreat sid `shouldReturn` (5 + fromIntegral players * 10 + if difficulty == Expert then 2 else 0)
          present <- Six.sixVillains
          liftIO $ length present `shouldBe` players + 2
  it "does not flip Escape when only part of its threat is removed" do
    tg <- sixGame Normal ["spider-man"]
    inGame tg do
      sid <- light
      resolve [SideSchemeMessage sid $ SideSchemeThreatRemoved (toSource $ player1 tg) 1]
      lightCode `shouldReturn` "27102a"
      sideSchemeThreat sid `shouldReturn` 19
  it "keeps the same side scheme and counters when Escape flips" do
    tg <- sixGame Normal ["spider-man"]
    inGame tg do
      sid <- light
      resolve [PlaceCounters (SideSchemeRef sid) 3]
      clearLight (player1 tg)
      light `shouldReturn` sid
      counters <- getsGame $ maybe 0 (Side.sideSchemeCounters . toAttrs) . HashMap.lookup sid . gameSideSchemes
      liftIO $ counters `shouldBe` 3
  it "Ambush adds a set-aside villain and gives it the active counter" do
    tg <- sixGame Normal ["spider-man"]
    inGame tg do
      before <- Six.sixVillains
      runNow =<< L.runQueueT (L.capture Six.ambush)
      settle
      after <- Six.sixVillains
      liftIO $ length after `shouldBe` length before + 1
  it "expert Ambush places two threat on Light at the End" do
    tg <- sixGame Expert ["spider-man"]
    inGame tg do
      sid <- light
      before <- sideSchemeThreat sid
      runNow =<< L.runQueueT (L.capture Six.ambush)
      settle
      sideSchemeThreat sid `shouldReturn` (before + 2)
  for_ [(Normal, 4), (Expert, 4)] \(difficulty, award) ->
    it ("removes the correct threat and sets a defeated villain aside: " <> show difficulty) do
      tg <- sixGame difficulty ["spider-man"]
      inGame tg do
        sid <- light
        before <- sideSchemeThreat sid
        ((vid,_):_) <- Six.sixVillains
        runNow [ResolveVillainDefeat vid]
        settle
        sideSchemeThreat sid `shouldReturn` (before - award)
        present <- Six.sixVillains
        liftIO $ map fst present `shouldNotContain` [vid]
        getsGame gameState `shouldReturn` InProgress
  it "does not win when the last villain is defeated; a later Ambush can bring a villain back" do
    tg <- sixGame Normal ["spider-man"]
    inGame tg do
      present <- Six.sixVillains
      for_ present $ \(vid,_) -> runNow [ResolveVillainDefeat vid] >> settle
      Six.sixVillains `shouldReturn` []
      getsGame gameState `shouldReturn` InProgress
      runNow =<< L.runQueueT (L.capture Six.ambush)
      settle
      returning <- Six.sixVillains
      liftIO $ length returning `shouldBe` 1
  it "wins by clearing Chase rather than by defeating villains" do
    tg <- sixGame Normal ["spider-man"]
    inGame tg do
      clearLight (player1 tg)
      clearLight (player1 tg)
      getsGame gameState `shouldReturn` Finished Won
  it "a failed Ambush does not place the expert two threat" do
    tg <- sixGame Expert ["spider-man"]
    inGame tg do
      replicateM_ 4 $ do
        runNow =<< L.runQueueT (L.capture Six.ambush)
        settle
      allSix <- Six.sixVillains
      liftIO $ length allSix `shouldBe` 6
      sid <- light
      before <- sideSchemeThreat sid
      runNow =<< L.runQueueT (L.capture Six.ambush)
      settle
      sideSchemeThreat sid `shouldReturn` before

  it "removes seven threat when the last villain is defeated" do
    tg <- sixGame Normal ["spider-man"]
    inGame tg do
      [(first,_),(lastOne,_)] <- Six.sixVillains
      runNow [ResolveVillainDefeat first]
      settle
      sid <- light
      before <- sideSchemeThreat sid
      runNow [ResolveVillainDefeat lastOne]
      settle
      sideSchemeThreat sid `shouldReturn` (before - 7)

  it "Heightened Morale adds ATK equal to all villains in play" do
    tg <- sixGame Normal ["spider-man"]
    inGame tg do
      present <- Six.sixVillains
      _ <- revealNow (player1 tg) Attachments.smHeightenedMorale
      settle
      let (highest,_) = last present
      modifiers <- getModifiers highest
      liftIO $ modifiers `shouldContain` [AttackModifier 2]
  it "Taunting Presence prevents removing threat from Light at the End" do
    tg <- sixGame Normal ["spider-man"]
    inGame tg do
      _ <- revealNow (player1 tg) Attachments.smTauntingPresence
      settle
      sid <- light
      modifiers <- getModifiers sid
      liftIO $ modifiers `shouldContain` [ThreatCannotBeRemoved]
      resolve [SideSchemeMessage sid $ SideSchemeThreatRemoved (toSource $ player1 tg) 20]
      sideSchemeThreat sid `shouldReturn` 20
      lightCode `shouldReturn` "27102a"
  it "Team Leader supplies an attachment crisis icon" do
    tg <- sixGame Normal ["spider-man"]
    inGame tg do
      _ <- revealNow (player1 tg) Attachments.smTeamLeader
      settle
      hasCrisisInYourArea `shouldReturn` True
  it "Brute Force Barricade blocks removing threat from other side schemes" do
    tg <- sixGame Normal ["spider-man"]
    inGame tg do
      _ <- revealNow (player1 tg) SideSchemes.smBruteForceBarricade
      settle
      sid <- light
      modifiers <- getModifiers sid
      liftIO $ modifiers `shouldContain` [ThreatCannotBeRemoved]
