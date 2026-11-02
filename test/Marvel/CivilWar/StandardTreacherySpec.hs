module Marvel.CivilWar.StandardTreacherySpec (spec) where

import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers (civilWarDef)
import Marvel.CivilWar.LeaderSpec (withLeader,leaderHp)
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
import Marvel.Game.Public (publicGameFor)
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as M
import Marvel.SideScheme.Cards qualified as S
import Marvel.Treachery.Cards qualified as T
import TestImport

declineOptional :: GameAppT ()
declineOptional = whenM (offers isNoResponse) $ choose "decline optional responses" isNoResponse >> declineOptional

spec :: Spec
spec = describe "Civil War Standard PvP treacheries" do
  for_ ["56125","56203","57036","57078"] $ \code ->
    for_ [("one hero",["spider-man","captain-marvel"],2),("two heroes",["spider-man","she-hulk","captain-marvel","iron-man"],3)] $ \(description,heroes,threat) ->
      it (show code <> " schemes with the printed number of boosts for a team with " <> description) do
        tg <- competitive heroes
        inGame tg do
          _ <- withLeader "57040"
          _ <- stackEncounterDeck [M.hydraMercenary,M.hydraMercenary]
          card <- encounterCard (civilWarDef code)
          resolve [RevealEncounterCard (player1 tg) card]
          mainSchemeThreat `shouldReturn` threat

  it "Righteous Cause does not count an eliminated teammate or the opposing players for its extra boost" do
    tg <- competitive ["spider-man","she-hulk","captain-marvel","iron-man"]
    inGame tg do
      _ <- withLeader "57040"
      resolve [IdentityMessage (seats tg !! 1) IdentityDefeated]
      _ <- stackEncounterDeck [M.hydraMercenary,M.hydraMercenary]
      card <- encounterCard C.cw56125
      resolve [RevealEncounterCard (player1 tg) card]
      mainSchemeThreat `shouldReturn` 2

  for_ ["56126","56204","57037","57079"] $ \code -> do
    it (show code <> " attacks a hero who reveals it") do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        let p = player1 tg
        _ <- withLeader "57040"
        friendly <- addSecondBoard tg
        before <- leaderHp friendly
        changeForm p
        _ <- stackEncounterDeck [M.hydraMercenary]
        card <- encounterCard (civilWarDef code)
        resolve [RevealEncounterCard p card]
        damageOf p `shouldReturn` 2
        leaderHp friendly `shouldReturn` before

    it (show code <> " attacks the revealing alter-ego's friendly leader without asking a player to defend") do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        let p = player1 tg
        _ <- withLeader "57040"
        friendly <- addSecondBoard tg
        before <- leaderHp friendly
        _ <- stackEncounterDeck [M.hydraMercenary]
        card <- encounterCard (civilWarDef code)
        start [RevealEncounterCard p card]
        leaderHp friendly `shouldReturn` (before - 2)
        damageOf p `shouldReturn` 0
        damageOf (player2 tg) `shouldReturn` 0

  for_ ["56127","56205","57038","57080"] $ \code ->
    it (show code <> " gives the opponent a private top-five search of the receiving team's deck") do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        let p = player1 tg
            q = player2 tg
        leader <- villainIdNow
        opponent <- addSecondBoard tg
        ownCards <- traverse encounterCard [M.hydraMercenary,M.hydraSoldier,M.weaponsRunner,T.hardToKeepDown,S.breakinAndTakin,S.crowdControl]
        otherCards <- traverse encounterCard [M.armoredGuard,M.hydraSoldier,M.hydraBomber]
        messages <- L.runQueueT $ L.capture $ do
          L.createDeck (L.VillainDeckKey leader) ownCards
          L.createDeck (L.VillainDeckKey opponent) otherCards
        resolve messages
        ownBefore <- asYou p getEncounterDeck
        otherBefore <- asYou q getEncounterDeck
        let selected = testHead ownBefore
            outside = last ownBefore
            isCard card = \case CardLabel candidate _ -> candidate == toCardCode card;_ -> False
        card <- encounterCard (civilWarDef code)
        start [RevealEncounterCard p card]
        fmap fst <$> asked `shouldReturn` Just q
        offers (isCard selected) `shouldReturn` True
        offers (isCard outside) `shouldReturn` False
        game <- getGame
        view <- liftIO $ publicGameFor (== p) game
        let visibleChoices = [length choices | Object root <- [view],Just (Object questions) <- [KeyMap.lookup "question" root],Object question <- KeyMap.elems questions,Just (Array choices) <- [KeyMap.lookup "choices" question]]
        liftIO $ visibleChoices `shouldBe` [0]
        choose "the opponent chooses a facedown encounter card" (isCard selected)
        playerIdentityEncounterCards <$> playerAttrs p `shouldReturn` [selected]
        asYou q getEncounterDeck `shouldReturn` otherBefore
        length <$> asYou p getEncounterDeck `shouldReturn` 5

  for_ [("one player",["spider-man","captain-marvel"],2),("two players",["spider-man","she-hulk","captain-marvel","iron-man"],3)] $ \(description,heroes,damage) ->
    it ("Targeted Strike's boost counts the " <> description <> " on the defending team") do
      tg <- competitive heroes
      inGame tg do
        let p = player1 tg
        vid <- withLeader "57040"
        changeForm p
        _ <- stackEncounterDeck [C.cw56127]
        start [VillainMessage vid $ VillainAttacks p]
        pass
        damageOf p `shouldReturn` damage

  it "counts two physical Targeted Strike boosts once each" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let p = player1 tg
      vid <- withLeader "57040"
      changeForm p
      _ <- stackEncounterDeck [C.cw56127,C.cw56127]
      messages <- L.runQueueT $ L.capture $ do
        L.additionalActivationBoost vid p
        L.enemyAttacks vid p
      start messages
      pass
      damageOf p `shouldReturn` 3

  it "Target Acquired cancels Targeted Strike's gained boost icons" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let p = player1 tg
      vid <- withLeader "57040"
      changeForm p
      acquired <- putIntoPlay p "08024"
      _ <- stackEncounterDeck [C.cw56127]
      start [VillainMessage vid $ VillainAttacks p]
      declineOptional
      choose "do not defend" (isLabel "$label.noDefender")
      choose "cancel the boost ability with Target Acquired" (isAbilityOf $ upgradeIdOf acquired)
      pass
      damageOf p `shouldReturn` 1

  it "Northstar cancels Targeted Strike's gained icons while leaving its boost ability intact" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let p = player1 tg
      vid <- withLeader "57040"
      changeForm p
      northstar <- putIntoPlay p "48013"
      _ <- stackEncounterDeck [C.cw56127]
      start [VillainMessage vid $ VillainAttacks p]
      declineOptional
      choose "do not defend" (isLabel "$label.noDefender")
      choose "cancel the icons with Northstar" (isAbilityOf $ allyIdOf northstar)
      pass
      damageOf p `shouldReturn` 1
