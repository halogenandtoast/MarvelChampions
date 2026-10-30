module Marvel.Aspect.MilesMoralesPackSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Minion.Cards qualified as Minions
import Marvel.Support.Cards qualified as Supports
import Marvel.Villain.Types qualified as Villain
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

logistics :: IdentityId -> GameAppT ()
logistics p = do
  support <- putIntoPlay p "01064"
  event <- putInHand p "27043"
  playWith event []
  choose "exhaust a SHIELD card" (isTargetChoice $ supportIdOf support)

assign :: (IsRef c) => c -> Text -> GameAppT ()
assign card destination = do
  choose "select an inspected card" (isTargetChoice card)
  choose "choose its destination" (isLabel destination)

spec :: Spec
spec = describe "Miles Morales pack Justice cards" do
  describe "Monica Chang" do
    for_ ["deck", "hand", "discard"] \zone ->
      it ("puts Surveillance Team into play from the " <> zone <> " and adds counters to all its controller's Teams") do
        tg <- testGame ["spider-man-miles-morales", "spider-man"]
        let p = player1 tg
            other = player2 tg
        inGame tg do
          first <- putIntoPlay p "01064"
          _ <- putIntoPlay other "01064"
          team <- case zone of
            "deck" -> head <$> stackDeck p ["27045"]
            "hand" -> putInHand p "27045"
            _ -> putInDiscard p "27045"
          monica <- putInHand p "27040"
          resources <- traverse (putInHand p) ["01088", "27031"]
          playWith monica resources
          choose "Monica's response" (isAbilityOfCard monica)
          choose "the searched Surveillance Team" (isTargetChoice team)
          pass
          own <- supportsWith "01064"
          reprint <- supportsWith "27045"
          [supportUses t | t <- own <> reprint, supportController t == p] `shouldMatchList` [4, 4]
          [supportUses t | t <- own, supportController t == other] `shouldBe` [3]
          handOf p >>= (`shouldSatisfy` notElem team)
          unDeck . playerIdentityDeck <$> playerAttrs p >>= (`shouldSatisfy` notElem team)
          discardOf p >>= (`shouldSatisfy` notElem team)
          supportIdOf first `shouldSatisfy` (/= supportIdOf team)

    it "still adds a snoop counter when no additional Surveillance Team can be found" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "01064"
        -- Remove the starter deck's Teams from all search zones.
        withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) -> PlayerIdentity a
          { playerIdentityDeck = Deck $ filter ((`notElem` ["01064", "27045"]) . toCardCode) (unDeck $ playerIdentityDeck a)
          , playerIdentityDiscard = Discard [] }
        monica <- putInHand p "27040"
        resources <- traverse (putInHand p) ["01088", "27031"]
        playWith monica resources
        choose "Monica's response" (isAbilityOfCard monica)
        pass
        map supportUses <$> supportsWith "01064" `shouldReturn` [4]

  describe "Spider-Woman" do
    it "reduces her cost by one for each confused enemy, including minions" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        clearHand p
        minion <- addMinion p Minions.hydraSoldier
        vid <- villainIdNow
        resolve [VillainMessage vid (VillainConfused GameRef), MinionMessage minion (MinionConfused GameRef)]
        card <- putInHand p "27041"
        resource <- putInHand p "27031"
        refreshTurn
        offers (isPlay card) `shouldReturn` True
        playWith card [resource]
        pass
        alliesWith "27041" >>= (`shouldSatisfy` ((== 1) . length))

    it "costs three when enemies have no confuse status" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        clearHand p
        card <- putInHand p "27041"
        _ <- putInHand p "27031"
        refreshTurn
        offers (isPlay card) `shouldReturn` False

  describe "Homeland Intervention" do
    for_ [1 :: Int, 3] \n ->
      it ("exhausts " <> show n <> " SHIELD cards and removes twice that much threat") do
        tg <- testGame ["spider-man-miles-morales", "spider-man"]
        let p = player1 tg
            other = player2 tg
        inGame tg do
          clearHand p
          own <- traverse (putIntoPlay p) ["01064", "01080", "01056"]
          opponents <- putIntoPlay other "01064"
          nonShield <- putIntoPlay p "27035"
          placeMainThreat 7
          before <- mainSchemeThreatNow
          event <- putInHand p "27042"
          playWith event []
          offers (isTargetChoice $ supportIdOf opponents) `shouldReturn` False
          offers (isTargetChoice $ supportIdOf nonShield) `shouldReturn` False
          offers (isLabel "$label.done") `shouldReturn` False
          for_ (take n own) \card -> choose "exhaust a SHIELD support" (isTargetChoice $ supportIdOf card)
          when (n < 3) $ choose "stop exhausting cards" (isLabel "$label.done")
          chooseMainScheme
          pass
          mainSchemeThreatNow `shouldReturn` before - 2 * fromIntegral n

    it "requires at least one ready SHIELD card to pay its up-to cost" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        clearHand p
        placeMainThreat 3
        event <- putInHand p "27042"
        refreshTurn
        offers (isPlay event) `shouldReturn` False

  describe "Global Logistics" do
    for_ [False, True] \otherDeck ->
      it ("preserves arbitrary top/bottom/discard order for " <> if otherDeck then "another player's deck" else "your deck") do
        tg <- testGame ["spider-man-miles-morales", "spider-man"]
        let p = player1 tg
            owner = if otherDeck then player2 tg else p
        inGame tg do
          clearHand p
          [a,b,c,d,e] <- stackDeck owner ["27031", "27032", "27033", "27034", "27035"]
          logistics p
          choose "the player deck" (isTargetChoice owner)
          assign c "$label.globalLogisticsTop"
          assign b "$label.globalLogisticsBottom"
          assign a "$label.globalLogisticsDiscard"
          assign d "$label.globalLogisticsTop"
          pass
          deck <- unDeck . playerIdentityDeck <$> playerAttrs owner
          take 3 deck `shouldBe` [c,d,e]
          lastMay deck `shouldBe` Just b
          discardOf owner >>= (`shouldSatisfy` elem a)
          deck `shouldSatisfy` notElem a

    it "can reorder and discard the encounter deck without changing uninspected cards" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        clearHand p
        ([a,b,c,d,e], _) <- setEncounterDeck [Minions.vulture, Minions.shocker, Minions.hydraSoldier, Minions.weaponsRunner, Minions.armoredGuard] []
        logistics p
        choose "encounter deck" (isLabel "$label.encounterDeck")
        assign d "$label.globalLogisticsTop"
        assign b "$label.globalLogisticsBottom"
        assign c "$label.globalLogisticsDiscard"
        assign a "$label.globalLogisticsTop"
        pass
        scenarioEncounterDeckNow `shouldReturn` [d,a,e,b]
        encounterDiscardNow `shouldReturn` [c]

    it "looks at fewer than four cards without prematurely resetting the player's deck" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        clearHand p
        [a,b] <- stackDeck p ["27031", "27032"]
        withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity attrs) ->
          PlayerIdentity attrs {playerIdentityDeck = Deck [a,b], playerIdentityDiscard = Discard []}
        logistics p
        choose "your deck" (isTargetChoice p)
        assign b "$label.globalLogisticsTop"
        assign a "$label.globalLogisticsBottom"
        pass
        unDeck . playerIdentityDeck <$> playerAttrs p `shouldReturn` [b,a]
        dealtTo p `shouldReturn` []

    it "discards an entire short encounter deck before its normal refill" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        clearHand p
        ([a,b], _) <- setEncounterDeck [Minions.vulture, Minions.shocker] []
        logistics p
        choose "encounter deck" (isLabel "$label.encounterDeck")
        assign a "$label.globalLogisticsDiscard"
        assign b "$label.globalLogisticsDiscard"
        pass
        scenarioEncounterDeckNow >>= (`shouldMatchList` [a,b])
        encounterDiscardNow `shouldReturn` []
