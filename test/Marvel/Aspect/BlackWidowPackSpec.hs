module Marvel.Aspect.BlackWidowPackSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

blackWidow :: IO TestGame
blackWidow = testGame ["black-widow"]

isMissionPrep :: Choice -> Bool
isMissionPrep = isNamedAbility "Mission Prep"

spec :: Spec
spec = describe "Black Widow's aspect and basic cards" do
  describe "Agent Coulson (08011)" do
    it "searches the deck and the discard pile for a Preparation card" do
      tg <- blackWidow
      let p = player1 tg
      inGame tg do
        clearHand p
        [bite] <- stackDiscard p ["08010"]
        [hook] <- stackDeck p ["08008"]
        coulson <- putInHand p "08011"
        payment <- traverse (putInHand p) ["01088", "01089"]
        playWith coulson payment
        choose "use Agent Coulson" (isAbilityOfCard coulson)
        offers (isTargetChoice hook) `shouldReturn` True
        choose "Widow's Bite" (isTargetChoice bite)
        handOf p `shouldReturn` [bite]
        discardOf p >>= (`shouldSatisfy` notElem bite)

  describe "Quake (08012)" do
    it "deals 2 damage to a minion after it schemes" do
      tg <- blackWidow
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "08012"
        taskmaster <- addMinion p Minions.taskmaster
        start [MinionMessage taskmaster MinionSchemes]
        [quake] <- alliesWith "08012"
        choose "use Quake" (isAbilityOf (allyId quake))
        minionDamageOf taskmaster `shouldReturn` Just 2

  describe "Stealth Strike (08013)" do
    it "removes 2 threat from a scheme when its attack defeats the enemy" do
      tg <- blackWidow
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        placeMainThreat 5
        threat <- mainSchemeThreatNow
        mercenary <- addMinion p Minions.hydraMercenary
        strike <- putInHand p "08013"
        [a, b, c] <- traverse (putInHand p) ["01088", "01089", "01090"]
        playWith strike [a, b, c]
        chooseIf (isTargetChoice (EnemyMinionId mercenary))
        pass
        mid <- mainSchemeId
        chooseIf (isTargetChoice mid)
        minionDamageOf mercenary `shouldReturn` Nothing
        mainSchemeThreatNow `shouldReturn` (threat - 2)

    it "removes no threat when the enemy survives" do
      tg <- blackWidow
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        placeMainThreat 5
        threat <- mainSchemeThreatNow
        strike <- putInHand p "08013"
        [a, b, c] <- traverse (putInHand p) ["01088", "01089", "01090"]
        playWith strike [a, b, c]
        vid <- villainIdNow
        chooseIf (isTargetChoice (EnemyVillainId vid))
        pass
        villainDamageNow `shouldReturn` 4
        mainSchemeThreatNow `shouldReturn` threat

  describe "Counterintelligence (08017)" do
    it "prevents 3 of the threat the villain would place, max 1 per player" do
      tg <- blackWidow
      let p = player1 tg
      inGame tg do
        clearHand p
        ci <- putIntoPlay p "08017"
        second <- putInHand p "08017"
        _ <- putInHand p "01088"
        offers (isPlay second) `shouldReturn` False
        _ <- stackEncounterDeck [Treacheries.theVulturesPlans]
        threat <- mainSchemeThreatNow
        vid <- villainIdNow
        start [VillainMessage vid VillainSchemes]
        choose "use Counterintelligence" (isAbilityOfCard ci)
        pass
        -- Rhino's 1 SCH and 2 boost, less 3
        mainSchemeThreatNow `shouldReturn` threat

  describe "Spycraft (08018)" do
    it "is played only with a Spy character" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        spycraft <- putInHand p "08018"
        _ <- putInHand p "01088"
        offers (isPlay spycraft) `shouldReturn` False
        _ <- putIntoPlay p "01084"
        offers (isPlay spycraft) `shouldReturn` True

    it "cancels an encounter card you reveal and reveals another" do
      tg <- blackWidow
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        spycraft <- putIntoPlay p "08018"
        [next] <- stackEncounterDeck [Minions.hydraMercenary]
        shot <- encounterCard Treacheries.deadlyShot
        runNow [RevealEncounterCard p shot]
        choose "use Spycraft" (isAbilityOfCard spycraft)
        pass
        damageOf p `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` elem "08029")
        getsGame (HashMap.member (minionOf next) . gameMinions) `shouldReturn` True

  describe "Quincarrier (08023)" do
    it "is played only while your identity is an Avenger, and generates a wild resource" do
      tg <- blackWidow
      let p = player1 tg
      inGame tg do
        clearHand p
        carrier <- putInHand p "08023"
        traverse_ (putInHand p) ["01088", "01089", "01090"]
        -- Natasha Romanoff is no Avenger
        offers (isPlay carrier) `shouldReturn` False
        changeForm p
        offers (isPlay carrier) `shouldReturn` True
        clearHand p
        _ <- putIntoPlay p "08023"
        haymaker <- putInHand p "01087"
        _ <- putInHand p "01088"
        offers (isPlay haymaker) `shouldReturn` True

  describe "Target Acquired (08024)" do
    it "cancels the boost ability of a boost card" do
      tg <- blackWidow
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        acquired <- putIntoPlay p "08024"
        _ <- putIntoPlay p "08017"
        _ <- stackEncounterDeck [Minions.taskmaster]
        threat <- mainSchemeThreatNow
        vid <- villainIdNow
        start [VillainMessage vid VillainSchemes]
        choose "use Target Acquired" (isAbilityOfCard acquired)
        pass
        -- Rhino's 1 SCH alone: Taskmaster's boost would have given 1 more
        mainSchemeThreatNow `shouldReturn` (threat + 1)

  describe "Counterattack (08030)" do
    it "deals the damage you take from an enemy attack back to that enemy" do
      tg <- blackWidow
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        counter <- putIntoPlay p "08030"
        _ <- stackEncounterDeck [Treacheries.theVulturesPlans]
        vid <- villainIdNow
        start [VillainMessage vid (VillainAttacks p)]
        choose "no defender" isNoDefender
        choose "use Counterattack" (isAbilityOfCard counter)
        pass
        damageOf p `shouldReturn` 4
        villainDamageNow `shouldReturn` 4

    it "cannot attack the villain while a guard minion is engaged with you" do
      tg <- blackWidow
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        counter <- putIntoPlay p "08030"
        _ <- addMinion p Minions.hydraMercenary
        _ <- stackEncounterDeck [Treacheries.theVulturesPlans]
        vid <- villainIdNow
        start [VillainMessage vid (VillainAttacks p)]
        choose "no defender" isNoDefender
        damageOf p >>= (`shouldSatisfy` (> 0))
        offers (isAbilityOfCard counter) `shouldReturn` False
        villainDamageNow `shouldReturn` 0

  describe "Rapid Response (08031)" do
    it "puts a defeated ally back into play with 1 damage" do
      tg <- blackWidow
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        response <- putIntoPlay p "08031"
        _ <- putIntoPlay p "08011"
        [coulson] <- alliesWith "08011"
        start [AllyMessage (allyId coulson) (AllyDamaged (toSource p) (toDamage 3 FromAbility))]
        choose "use Rapid Response" (isAbilityOfCard response)
        pass
        [coulson'] <- alliesWith "08011"
        coulson'.allyDamage `shouldBe` 1
        discardOf p >>= (`shouldSatisfy` all ((/= "08011") . toCardCode))

  describe "Defensive Stance (08032)" do
    it "prevents 3 of the damage you would take" do
      tg <- blackWidow
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        stance <- putIntoPlay p "08032"
        start (damageIdentity p GameRef (toDamage 4 FromAbility))
        choose "use Defensive Stance" (isAbilityOfCard stance)
        pass
        damageOf p `shouldReturn` 1
        upgradeCodes `shouldReturn` []

  describe "Espionage (08033)" do
    it "draws 2 cards when a surge would resolve" do
      tg <- blackWidow
      let p = player1 tg
      inGame tg do
        clearHand p
        espionage <- putIntoPlay p "08033"
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        start [Surge p]
        choose "use Espionage" (isAbilityOfCard espionage)
        pass
        length <$> handOf p `shouldReturn` 2
        offers isMissionPrep `shouldReturn` False
