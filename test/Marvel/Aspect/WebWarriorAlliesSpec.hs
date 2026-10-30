module Marvel.Aspect.WebWarriorAlliesSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Hero.GhostSpiderSpec (ghostSpider, asGhost)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as Villain
import TestImport

printedAbility :: PlayerCard -> Choice -> Bool
printedAbility card = \case
  UseAbility ab -> abilityIndex ab == 1 && abilitySource ab == AllyRef (allyIdOf card)
  _ -> False

spec :: Spec
spec = describe "Sinister Motives Web-Warrior allies" do
  describe "Silk" do
    it "discards a chosen treachery from the encounter deck after being played from hand with another Web-Warrior" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        silk <- putInHand p "27010"
        energy <- putInHand p "01088"
        (deck, _) <- setEncounterDeck [Treacheries.advance, Treacheries.caughtOffGuard, Minions.shocker] []
        playWith silk [energy]
        choose "Silk's response" (printedAbility silk)
        choose "discard Advance" (isTargetChoice $ head deck)
        encounterDiscard >>= (`shouldSatisfy` elem (head deck))
        scenarioEncounterDeckNow >>= (`shouldSatisfy` notElem (head deck))
        mainSchemeThreatNow `shouldReturn` 0

    it "does not trigger when put into play or when no other Web-Warrior is controlled" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        silk <- putInHand p "27010"
        energy <- putInHand p "01088"
        playWith silk [energy]
        offers (printedAbility silk) `shouldReturn` False
        resolve [ReturnToHand $ AllyRef $ allyIdOf silk]
        direct <- putIntoPlay p "27010"
        offers (printedAbility direct) `shouldReturn` False

  describe "Spider-Man (Miles Morales)" do
    it "counts himself as the third controlled Web-Warrior and stuns and confuses one enemy" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        _ <- putIntoPlay p "27023"
        miles <- putInHand p "27011"
        energy <- putInHand p "01088"
        genius <- putInHand p "01089"
        playWith miles [energy, genius]
        choose "Miles' response" (printedAbility miles)
        chooseVillain
        Villain.villainStunned . toAttrs <$> getVillain `shouldReturn` True
        Villain.villainConfused . toAttrs <$> getVillain `shouldReturn` True

    it "does not trigger with fewer than three Web-Warriors or from put-into-play effects" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        miles <- putInHand p "27011"
        energy <- putInHand p "01088"
        genius <- putInHand p "01089"
        playWith miles [energy, genius]
        offers (printedAbility miles) `shouldReturn` False
        resolve [ReturnToHand $ AllyRef $ allyIdOf miles]
        _ <- putIntoPlay p "27023"
        direct <- putIntoPlay p "27011"
        offers (printedAbility direct) `shouldReturn` False

  describe "Spider-UK" do
    it "deals controlled Web-Warrior count damage before the defended attack, stopping a defeated attacker" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        _ <- putIntoPlay p "27023"
        uk <- putIntoPlay p "27012"
        mid <- addMinion p Minions.hydraMercenary
        runNow [MinionMessage mid $ MinionAttacks p]
        choose "defend with Spider-UK" (\case AllyDefend aid _ -> aid == allyIdOf uk; _ -> False)
        choose "Spider-UK's interrupt" (printedAbility uk)
        pass
        minionDamageOf mid `shouldReturn` Nothing
        fmap (map allyDamage) (alliesWith "27012") `shouldReturn` [0]
        damageOf p `shouldReturn` 0

    it "does not trigger merely because an attack is made against the identity" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        uk <- putIntoPlay p "27012"
        _ <- stackEncounterDeck [Treacheries.imTough]
        vid <- villainIdNow
        runNow [VillainMessage vid $ VillainAttacks p]
        offers (printedAbility uk) `shouldReturn` False
        choose "defend with the hero" isDefend
        offers (printedAbility uk) `shouldReturn` False

  describe "Spider-Man (Hobie Brown)" do
    for_ [("discard", RemoveFromPlay), ("return to hand", ReturnToHand), ("remove from game", RemoveFromGame)] $ \(cause, leave) ->
      it ("discards three encounter cards and deals their boost icons before " <> cause) do
        tg <- ghostSpider
        let p = player1 tg
        inGame tg do
          asGhost p
          hobie <- putIntoPlay p "27017"
          _ <- setEncounterDeck [Minions.shocker, Minions.hydraMercenary, Treacheries.imTough, Treacheries.advance] []
          runNow [leave $ AllyRef $ allyIdOf hobie]
          choose "Hobie's interrupt" (printedAbility hobie)
          villainDamageNow `shouldReturn` 3
          pass
          encounterDiscardCodes >>= (`shouldSatisfy` (\codes -> all (`elem` codes) ["01103", "01101", "01105"]))

    it "requires control of a Web-Warrior card to be played" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        hobie <- putInHand p "27017"
        _ <- putInHand p "01088"
        _ <- putInHand p "01089"
        offers (isPlay hobie) `shouldReturn` False
        _ <- putIntoPlay p "27023"
        offers (isPlay hobie) `shouldReturn` True

  describe "Web of Life and Destiny" do
    it "costs zero for a Web-Warrior identity and costs three in alter ego" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        clearHand p
        web <- putInHand p "27023"
        offers (isPlay web) `shouldReturn` False
        changeForm p
        offers (isPlay web) `shouldReturn` True
        playWith web []
        supportsWith "27023" >>= (`shouldSatisfy` ((== 1) . length))

    for_ [("discard", RemoveFromPlay), ("return to hand", ReturnToHand), ("remove from game", RemoveFromGame)] $ \(cause, leave) ->
      it ("lets its controller choose another player to draw after a Web-Warrior ally's " <> cause) do
        tg <- testGame ["ghost-spider", "spider-man"]
        let p = player1 tg
            q = player2 tg
        inGame tg do
          asGhost p
          web <- putIntoPlay p "27023"
          silk <- putIntoPlay q "27010"
          before <- length <$> handOf q
          runNow [leave $ AllyRef $ allyIdOf silk]
          choose "Web of Life and Destiny" (isAbilityOfCard web)
          choose "Spider-Man draws" (isTargetChoice q)
          length <$> handOf q `shouldReturn` (before + 1 + if cause == "return to hand" then 1 else 0)

    it "does not trigger when a non-Web-Warrior ally leaves play" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        web <- putIntoPlay p "27023"
        ally <- putIntoPlay p "01068"
        runNow [RemoveFromPlay $ AllyRef $ allyIdOf ally]
        offers (isAbilityOfCard web) `shouldReturn` False

  describe "Across the Spider-Verse" do
    it "can repeat the whole ability more than once, paying three and exhausting a Web-Warrior each time" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        [silk, uk, hobie] <- stackDiscard p ["27010", "27012", "27017"]
        event <- putInHand p "27018"
        initial <- putInHand p "01088"
        a <- putInHand p "01089"
        b <- putInHand p "27024"
        c <- putInHand p "01090"
        d <- putInHand p "27003"
        playWith event [initial]
        choose "exhaust the hero" (isTargetChoice p)
        choose "retrieve Silk" (isTargetChoice silk)
        choose "choose this player to repeat" (isTargetChoice p)
        choose "pay to repeat" (isLabel "$label.pay")
        payWith [a, b]
        choose "exhaust Silk" (isTargetChoice $ allyIdOf silk)
        choose "retrieve Spider-UK" (isTargetChoice uk)
        choose "choose this player again" (isTargetChoice p)
        choose "pay for another repetition" (isLabel "$label.pay")
        payWith [c, d]
        choose "exhaust Spider-UK" (isTargetChoice $ allyIdOf uk)
        choose "retrieve Hobie" (isTargetChoice hobie)
        choose "choose this player once more" (isTargetChoice p)
        choose "decline to repeat" (isLabel "$label.done")
        fmap (map allyExhausted) (alliesWith "27010") `shouldReturn` [True]
        fmap (map allyExhausted) (alliesWith "27012") `shouldReturn` [True]
        fmap (map allyExhausted) (alliesWith "27017") `shouldReturn` [False]
        discardOf p >>= (`shouldSatisfy` elem event)

    it "requires a ready controlled Web-Warrior card to pay its initial cost" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        _ <- stackDiscard p ["27010"]
        event <- putInHand p "27018"
        _ <- putInHand p "01088"
        resolve [IdentityMessage p ExhaustedIdentity]
        offers (isPlay event) `shouldReturn` False
