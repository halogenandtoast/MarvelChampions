module Marvel.Hero.MilesMoralesSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as Text
import Marvel.Card.Side (Side (A, B))
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Villain.Types qualified as Villain
import Marvel.Window qualified as W
import TestImport

milesGame :: IO TestGame
milesGame = testGame ["spider-man-miles-morales"]

asMiles :: IdentityId -> GameAppT ()
asMiles p = changeForm p >> pass >> clearHand p

isShuffleResponse :: IdentityId -> Choice -> Bool
isShuffleResponse p = \case
  UseAbility ab -> abilitySource ab == toRef p && abilityIndex ab == 1 && ChangeForm `notElem` abilityChoices ab
  _ -> False

spec :: Spec
spec = describe "Miles Morales" do
  describe "identity" do
    it "shuffles a chosen Miles signature card from its own discard after changing to alter-ego" do
      tg <- milesGame
      let p = player1 tg
      inGame tg do
        asMiles p
        signature <- putInDiscard p "27034"
        aspect <- putInDiscard p "27042"
        peter <- putInDiscard p "01005"
        runNow [IdentityMessage p (ChangedToForm B)]
        choose "Miles's response" (isShuffleResponse p)
        offers (isTargetChoice signature) `shouldReturn` True
        offers (isTargetChoice aspect) `shouldReturn` False
        offers (isTargetChoice peter) `shouldReturn` False
        choose "shuffle Web-Shot" (isTargetChoice signature)
        pass
        unDeck . playerIdentityDeck <$> playerAttrs p >>= (`shouldSatisfy` elem signature)
        discardOf p >>= (`shouldSatisfy` (\cards -> aspect `elem` cards && peter `elem` cards && signature `notElem` cards))

    it "does not offer the shuffle response when changing to hero or when no signature card is discarded" do
      tg <- milesGame
      let p = player1 tg
      inGame tg do
        _ <- putInDiscard p "27042"
        changeForm p
        offers (isShuffleResponse p) `shouldReturn` False
        pass
        runNow [IdentityMessage p (ChangedToForm B)]
        offers (isShuffleResponse p) `shouldReturn` False

    it "may decline the response without moving the card" do
      tg <- milesGame
      let p = player1 tg
      inGame tg do
        asMiles p
        card <- putInDiscard p "27039"
        runNow [IdentityMessage p (ChangedToForm B)]
        choose "decline Miles's response" isNoResponse
        discardOf p >>= (`shouldSatisfy` elem card)

  describe "Ganke Lee" do
    it "exhausts and draws a card in alter-ego form" do
      tg <- milesGame
      let p = player1 tg
      inGame tg do
        clearHand p
        ganke <- putIntoPlay p "27035"
        top <- stackDeck p ["27031"]
        choose "Ganke Lee" (isAbilityOfCard ganke)
        handOf p `shouldReturn` top
        map supportExhausted <$> supportsWith "27035" `shouldReturn` [True]
        offers (isAbilityOfCard ganke) `shouldReturn` False

    it "draws before choosing one card to discard in hero form" do
      tg <- milesGame
      let p = player1 tg
      inGame tg do
        asMiles p
        ganke <- putIntoPlay p "27035"
        retained <- putInHand p "27031"
        [drawn] <- stackDeck p ["27034"]
        choose "Ganke Lee" (isAbilityOfCard ganke)
        offers (isTargetChoice drawn) `shouldReturn` True
        choose "discard the card just drawn" (isTargetChoice drawn)
        handOf p `shouldReturn` [retained]
        discardOf p >>= (`shouldSatisfy` elem drawn)

  describe "Jefferson Davis" do
    it "removes one threat from the scheme with the least threat" do
      tg <- milesGame
      let p = player1 tg
      inGame tg do
        jefferson <- putIntoPlay p "27036"
        scheme <- revealNow p Schemes.trackingPrey
        pass
        placeMainThreat 5
        choose "Jefferson Davis" (isAbilityOfCard jefferson)
        offers (isTargetChoice $ SideSchemeId $ unCardId $ ecCardId scheme) `shouldReturn` True
        mid <- mainSchemeIdNow
        offers (isTargetChoice mid) `shouldReturn` False
        choose "Tracking Prey" (isTargetChoice $ SideSchemeId $ unCardId $ ecCardId scheme)
        pass
        sideSchemeThreat (SideSchemeId $ unCardId $ ecCardId scheme) `shouldReturn` 3
        map supportExhausted <$> supportsWith "27036" `shouldReturn` [True]

    it "cannot exhaust Jefferson when a least-threat scheme is empty, even if another has threat" do
      tg <- milesGame
      let p = player1 tg
      inGame tg do
        jefferson <- putIntoPlay p "27036"
        _ <- revealNow p Schemes.trackingPrey
        pass
        offers (isAbilityOfCard jefferson) `shouldReturn` False
        map supportExhausted <$> supportsWith "27036" `shouldReturn` [False]

    it "allows choosing among tied schemes and is restricted to alter-ego" do
      tg <- milesGame
      let p = player1 tg
      inGame tg do
        jefferson <- putIntoPlay p "27036"
        scheme <- revealNow p Schemes.trackingPrey
        pass
        before <- mainSchemeThreatNow
        placeMainThreat (4 - before)
        choose "Jefferson Davis" (isAbilityOfCard jefferson)
        mid <- mainSchemeIdNow
        offers (isTargetChoice mid) `shouldReturn` True
        offers (isTargetChoice $ SideSchemeId $ unCardId $ ecCardId scheme) `shouldReturn` True
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` 3
        resolve [SupportMessage (supportIdOf jefferson) ReadiedSupport]
        changeForm p
        pass
        offers (isAbilityOfCard jefferson) `shouldReturn` False

  describe "Power Within and Defense Mechanism" do
    for_ [("27037", "Power Within", False), ("27038", "Defense Mechanism", True)] \(code, title, camouflage) -> do
      for_ ["attack", "thwart", "defend"] \power ->
        it ("uses " <> title <> " after a basic " <> power <> " and discards it as the cost") do
          tg <- milesGame
          let p = player1 tg
          inGame tg do
            asMiles p
            upgrade <- putIntoPlay p code
            case power of
              "attack" -> choose "basic attack" (isHeroAttack p) >> chooseVillain
              "thwart" -> do
                placeMainThreat 4
                choose "basic thwart" (isHeroThwart p)
                chooseMainScheme
              _ -> do
                _ <- stackEncounterDeck [Minions.vulture]
                vid <- villainIdNow
                start [VillainMessage vid (VillainAttacks p)]
                choose "basic defense" isDefend
            choose (Text.pack title) (isAbilityOfCard upgrade)
            chooseVillain
            pass
            upgradesWith code `shouldReturn` []
            discardOf p >>= (`shouldSatisfy` elem upgrade)
            if camouflage then do
              (.playerIdentityTough) <$> playerAttrs p `shouldReturn` True
              Villain.villainConfused . toAttrs <$> getVillain `shouldReturn` True
            else do
              villainDamageNow `shouldReturn` (if power == "attack" then 4 else 2)
              Villain.villainStunned . toAttrs <$> getVillain `shouldReturn` True

      it (title <> " does not answer another hero's basic power or an alter-ego recovery") do
        tg <- testGame ["spider-man-miles-morales", "spider-man"]
        let p = player1 tg
            other = player2 tg
        inGame tg do
          upgrade <- putIntoPlay p code
          setDamage p 4
          choose "recover" \case UseAbility ab -> Recover `elem` abilityChoices ab; _ -> False
          offers (isAbilityOfCard upgrade) `shouldReturn` False
          resolve [IdentityMessage p (ChangedToForm A)]
          runNow [CheckWindows [W.Window W.After $ W.MadeBasicAttack other]]
          offers (isAbilityOfCard upgrade) `shouldReturn` False

  describe "Web-Shooter" do
    it "spends one web counter and exhausts to generate a wild resource, discarding after three uses" do
      tg <- milesGame
      let p = player1 tg
      inGame tg do
        asMiles p
        shooter <- putIntoPlay p "27039"
        for_ [1 :: Int, 2, 3] \_ -> do
          next <- putInHand p "27039"
          choose "play another Web-Shooter" (isPlay next)
          choose "generate a wild resource" (isAbilityOfCard shooter)
          whenM (offers isFinishPayment) $ choose "finish paying" isFinishPayment
          pass
          offers (isAbilityOfCard shooter) `shouldReturn` False
          runNow [UpgradeMessage (upgradeIdOf shooter) ReadiedUpgrade]
        getsGame (HashMap.member (upgradeIdOf shooter) . gameUpgrades) `shouldReturn` False
        discardOf p >>= (`shouldSatisfy` elem shooter)

    it "cannot supply resources in alter-ego form" do
      tg <- milesGame
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "27039"
        card <- putInHand p "27032"
        refreshTurn
        offers (isPlay card) `shouldReturn` False

  describe "Keeping Secrets" do
    it "allows flipping to alter-ego and exhausting Miles to remove it from the game" do
      tg <- milesGame
      let p = player1 tg
      inGame tg do
        asMiles p
        _ <- revealNow p Obligations.keepingSecrets
        choose "change to alter-ego" (isLabel "$label.changeToB")
        pass
        choose "exhaust Miles" (isLabel "$label.exhaustMilesMoralesRemoveKeepingSecrets")
        pass
        (.playerIdentitySide) <$> playerAttrs p `shouldReturn` B
        isExhaustedNow p `shouldReturn` True
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "27056")

    for_ [["27035", "27036"], ["27035"], ["27036"]] \codes ->
      it ("discards the supports in play without surging: " <> show codes) do
        tg <- milesGame
        let p = player1 tg
        inGame tg do
          supports <- traverse (putIntoPlay p) codes
          _ <- revealNow p Obligations.keepingSecrets
          choose "discard the supports" (isLabel "$label.discardGankeLeeAndJeffersonDavis")
          pass
          discardOf p >>= (`shouldSatisfy` (\cards -> all (`elem` cards) supports))
          encounterDiscardCodes >>= (`shouldSatisfy` elem "27056")
          dealtTo p `shouldReturn` []

    it "surges when neither support was discarded and leaves another player's supports alone" do
      tg <- testGame ["spider-man-miles-morales", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        _ <- putIntoPlay other "27035"
        _ <- putIntoPlay other "27036"
        -- A harmless side scheme makes the mandatory surge deterministic.
        _ <- stackEncounterDeck [Schemes.bombScare]
        _ <- revealNow p Obligations.keepingSecrets
        choose "discard neither support" (isLabel "$label.discardGankeLeeAndJeffersonDavis")
        pass
        supportsWith "27035" >>= (`shouldSatisfy` ((== 1) . length))
        supportsWith "27036" >>= (`shouldSatisfy` ((== 1) . length))
        encounterDiscardCodes >>= (`shouldSatisfy` elem "27056")
        map toCardCode <$> dealtTo p >>= (`shouldSatisfy` elem "01109")
