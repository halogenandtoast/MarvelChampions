module Marvel.Aspect.JusticeSpec (spec) where

import Marvel.Minion.Cards qualified as Cards
import Marvel.SideScheme.Cards qualified as Cards
import TestImport

spec :: Spec
spec = describe "Justice" do
  describe "Daredevil (01058)" do
    it "deals 1 damage to an enemy after thwarting" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "01058"
        placeMainThreat 3
        threat <- mainSchemeThreatNow
        dd <- allyIn "01058"
        mid <- mainSchemeId
        villain <- getVillain
        choose "thwart with Daredevil" (isAllyThwart dd)
        choose "the target" (isTargetChoice mid)
        mainSchemeThreatNow `shouldReturn` (threat - 2)
        choose "use Daredevil" (isAbilityOf (allyId dd))
        choose "the target" (isTargetChoice (toId villain))
        villainDamageNow `shouldReturn` 1
        (.allyDamage) <$> allyIn "01058" `shouldReturn` 1

  describe "Jessica Jones (01059)" do
    it "gets +1 THW for each side scheme in play" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "01059"
        placeMainThreat 5
        scheme <- encounterCard Cards.breakinAndTakin
        resolve [RevealEncounterCard p scheme]
        threat <- mainSchemeThreatNow
        jj <- allyIn "01059"
        mid <- mainSchemeId
        choose "thwart with Jessica Jones" (isAllyThwart jj)
        choose "the main scheme" (isTargetChoice mid)
        mainSchemeThreatNow `shouldReturn` (threat - 2)

  describe "For Justice! (01060)" do
    it "removes 3 threat" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        forJustice <- putInHand p "01060"
        energy <- putInHand p "01088"
        placeMainThreat 6
        threat <- mainSchemeThreatNow
        offers (isPlay forJustice) `shouldReturn` False
        changeForm p
        playWith forJustice [energy]
        mid <- mainSchemeId
        choose "the target" (isTargetChoice mid)
        mainSchemeThreatNow `shouldReturn` (threat - 3)

    it "removes 4 threat when paid for with a mental resource" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        forJustice <- putInHand p "01060"
        genius <- putInHand p "01089"
        placeMainThreat 6
        threat <- mainSchemeThreatNow
        changeForm p
        playWith forJustice [genius]
        mid <- mainSchemeId
        choose "the target" (isTargetChoice mid)
        mainSchemeThreatNow `shouldReturn` (threat - 4)

  describe "Great Responsibility (01061)" do
    it "takes the threat the villain would place as damage instead" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        changeForm p
        responsibility <- putInHand p "01061"
        threat <- mainSchemeThreatNow
        villain <- getVillain
        -- Rhino schemes for 1
        runNow [VillainMessage (toId villain) VillainSchemed]
        choose "play Great Responsibility" (isPlay responsibility)
        pass
        mainSchemeThreatNow `shouldReturn` threat
        damageOf p `shouldReturn` 1

    it "takes threat a card would place as damage instead" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        changeForm p
        responsibility <- putInHand p "01061"
        threat <- mainSchemeThreatNow
        scheme <- mainSchemeId
        runNow (placeThreatOn (SchemeMainSchemeId scheme) 2)
        choose "play Great Responsibility" (isPlay responsibility)
        pass
        mainSchemeThreatNow `shouldReturn` threat
        damageOf p `shouldReturn` 2

    it "takes it as damage a would-take-damage interrupt can answer" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        -- Cosmic Flight: when you would take damage, discard it to prevent 3
        flightCard <- putInHand p "01017"
        energy <- putInHand p "01088"
        playWith flightCard [energy]
        responsibility <- putInHand p "01061"
        threat <- mainSchemeThreatNow
        scheme <- mainSchemeId
        runNow (placeThreatOn (SchemeMainSchemeId scheme) 2)
        choose "play Great Responsibility" (isPlay responsibility)
        choose "use Cosmic Flight" (isAbilityOfCard flightCard)
        pass
        mainSchemeThreatNow `shouldReturn` threat
        damageOf p `shouldReturn` 0

  describe "The Power of Justice (01062)" do
    it "pays 2 for a Justice card, 1 otherwise" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        team <- putInHand p "01064"
        power <- putInHand p "01062"
        playWith team [power]
        supportsWith "01064" >>= (`shouldSatisfy` ((== 1) . length))
        firstAid <- putInHand p "01086"
        power2 <- putInHand p "01062"
        -- First Aid costs 1 and needs a damaged character
        setDamage p 2
        playWith firstAid [power2]
        choose "heal Peter Parker" (isTargetChoice (IdentityCharacter p))
        damageOf p `shouldReturn` 0

  describe "Interrogation Room (01063)" do
    it "removes 1 threat after you defeat a minion, max 1 per player" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "01063"
        room2 <- putInHand p "01063"
        _ <- putInHand p "01088"
        offers (isPlay room2) `shouldReturn` False
        placeMainThreat 3
        threat <- mainSchemeThreatNow
        changeForm p
        mercenary <- addMinion p Cards.hydraMercenary
        resolve [MinionMessage mercenary (MinionDamaged (toSource p) (toDamage 1 FromAbility))]
        choose "attack" (isHeroAttack p)
        choose "the target" (isTargetChoice mercenary)
        minionDamageOf mercenary `shouldReturn` Nothing
        [room] <- supportsWith "01063"
        choose "use Interrogation Room" (isAbilityOf (supportId room))
        mid <- mainSchemeId
        choose "the target" (isTargetChoice mid)
        mainSchemeThreatNow `shouldReturn` (threat - 1)

    it "does not answer a minion another player defeats" do
      tg <- testGame ["spider-man", "captain-marvel"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        clearHand p
        putIntoPlay p "01063"
        mercenary <- addMinion p Cards.hydraMercenary
        runNow [MinionMessage mercenary (MinionDamaged (toSource other) (toDamage 10 FromAbility))]
        [room] <- supportsWith "01063"
        offers (isAbilityOf (supportId room)) `shouldReturn` False

    it "does not answer a minion your ally defeats: \"you\" is your identity" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "01063"
        _ <- putIntoPlay p "01058"
        mercenary <- addMinion p Cards.hydraMercenary
        runNow [MinionMessage mercenary (MinionDamaged (toSource p) (toDamage 2 FromAbility))]
        daredevil <- allyIn "01058"
        choose "Daredevil attacks" (isAllyAttack daredevil)
        choose "the target" (isTargetChoice mercenary)
        minionDamageOf mercenary `shouldReturn` Nothing
        [room] <- supportsWith "01063"
        offers (isAbilityOf (supportId room)) `shouldReturn` False

  describe "Surveillance Team (01064)" do
    it "has 3 uses, each exhausting it to remove 1 threat" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "01064"
        placeMainThreat 5
        threat <- mainSchemeThreatNow
        [team] <- supportsWith "01064"
        team.supportUses `shouldBe` 3
        mid <- mainSchemeId
        for_ [1 :: Natural .. 3] \n -> do
          resolve [SupportMessage (supportId team) ReadiedSupport]
          choose "use Surveillance Team" (isAbilityOf (supportId team))
          choose "the target" (isTargetChoice mid)
          mainSchemeThreatNow `shouldReturn` (threat - n)
          when (n < 3) $ offers (isAbilityOf (supportId team)) `shouldReturn` False
        supportsWith "01064" `shouldReturn` []

  describe "Heroic Intuition (01065)" do
    it "may be played under another player's control, and is discarded to its owner's pile" do
      tg <- testGame ["spider-man", "captain-marvel"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        clearHand p
        intuition <- putInHand p "01065"
        energy <- putInHand p "01088"
        playOn intuition other [energy]
        upgrades <- upgradesWith "01065"
        map upgradeController upgrades `shouldBe` [other]
        for_ upgrades \u -> runNow [UpgradeMessage (upgradeId u) DiscardUpgrade]
        map toCardCode <$> discardOf p >>= (`shouldSatisfy` elem "01065")
        map toCardCode <$> discardOf other >>= (`shouldSatisfy` notElem "01065")

    it "gives the hero +1 THW, max 1 per player" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        intuition <- putInHand p "01065"
        intuition2 <- putInHand p "01065"
        energy <- putInHand p "01088"
        playWith intuition [energy]
        offers (isPlay intuition2) `shouldReturn` False
        placeMainThreat 5
        threat <- mainSchemeThreatNow
        changeForm p
        mid <- mainSchemeId
        choose "thwart" (isHeroThwart p)
        choose "the target" (isTargetChoice mid)
        -- Spider-Man has 1 THW
        mainSchemeThreatNow `shouldReturn` (threat - 2)
