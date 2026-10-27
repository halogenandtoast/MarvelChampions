module Marvel.Aspect.StarLordPackSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Minion.Cards qualified as Cards
import Marvel.Phase (Phase (PlayerPhase))
import TestImport

-- | Captain Marvel (Leadership, neither Aerial nor Guardian) in hero form, her hand
-- empty: the Guardians and Aerials are the allies the test puts into play.
asHero :: IdentityId -> GameAppT ()
asHero p = do
  changeForm p
  pass
  clearHand p

damageMinion :: IdentityId -> MinionId -> Natural -> GameAppT ()
damageMinion p m n = resolve [MinionMessage m (MinionDamaged (toSource p) (toDamage n FromAbility))]

useAbilityOf :: Attrs Ally -> GameAppT ()
useAbilityOf a = choose "use the ally's response" (isAbilityOf (allyId a))

-- | The ally's response (ability 1), not its basic attack or thwart.
isResponseOf :: Attrs Ally -> Choice -> Bool
isResponseOf a = \case
  UseAbility ab -> abilitySource ab == toSource (allyId a) && abilityIndex ab == 1
  _ -> False

-- | Play Laser Blaster on the ally, as a player does.
attachLaserBlaster :: IdentityId -> Attrs Ally -> GameAppT ()
attachLaserBlaster p a = do
  laser <- putInHand p "17019"
  energy <- putInHand p "01088"
  playOn laser (allyId a) [energy]

spec :: Spec
spec = describe "Star-Lord pack" do
  describe "Adam Warlock (17011)" do
    it "after attacking, discards at random: a physical card removes 3 threat from a scheme" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "17011"
        adam <- allyIn "17011"
        strength <- putInHand p "01090"
        placeMainThreat 5
        threat <- mainSchemeThreatNow
        choose "attack with Adam Warlock" (isAllyAttack adam)
        chooseVillain
        useAbilityOf adam
        chooseMainScheme
        mainSchemeThreatNow `shouldReturn` (threat - 3)
        handOf p `shouldReturn` []
        discardOf p `shouldReturn` [strength]
        villainDamageNow `shouldReturn` 1

    it "an energy card heals 3 damage from an identity" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "17011"
        adam <- allyIn "17011"
        _ <- putInHand p "01088"
        setDamage p 4
        choose "attack with Adam Warlock" (isAllyAttack adam)
        chooseVillain
        useAbilityOf adam
        chooseIf (isTargetChoice p)
        damageOf p `shouldReturn` 1

    it "after thwarting too: a mental card deals 3 damage to an enemy" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "17011"
        adam <- allyIn "17011"
        _ <- putInHand p "01089"
        placeMainThreat 3
        choose "thwart with Adam Warlock" (isAllyThwart adam)
        chooseMainScheme
        useAbilityOf adam
        chooseVillain
        villainDamageNow `shouldReturn` 3

    it "a wild card chooses one of the three" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "17011"
        adam <- allyIn "17011"
        _ <- putInHand p "01044"
        choose "attack with Adam Warlock" (isAllyAttack adam)
        chooseVillain
        useAbilityOf adam
        offers (isLabel (countLabel "healAnIdentity" 3)) `shouldReturn` True
        offers (isLabel (countLabel "removeThreatFromAScheme" 3)) `shouldReturn` True
        choose "deal damage" (isLabel (countLabel "dealDamageToAnEnemy" 3))
        chooseVillain
        villainDamageNow `shouldReturn` 4

    it "with an empty hand does nothing" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "17011"
        adam <- allyIn "17011"
        threat <- mainSchemeThreatNow
        choose "attack with Adam Warlock" (isAllyAttack adam)
        chooseVillain
        chooseIf (isAbilityOf (allyId adam))
        villainDamageNow `shouldReturn` 1
        mainSchemeThreatNow `shouldReturn` threat
        discardOf p `shouldReturn` []

  describe "Beta Ray Bill (17012)" do
    it "removes 2 threat from the main scheme after he attacks and defeats a minion" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "17012"
        bill <- allyIn "17012"
        placeMainThreat 4
        threat <- mainSchemeThreatNow
        mercenary <- addMinion p Cards.hydraMercenary
        choose "attack with Beta Ray Bill" (isAllyAttack bill)
        choose "the mercenary" (isTargetChoice mercenary)
        minionDamageOf mercenary `shouldReturn` Nothing
        useAbilityOf bill
        chooseIf . isTargetChoice =<< mainSchemeIdNow
        mainSchemeThreatNow `shouldReturn` (threat - 2)

    it "not when the minion survives his attack" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "17012"
        bill <- allyIn "17012"
        -- Whiplash has 4 hit points
        whiplash <- addMinion p Cards.whiplash
        choose "attack with Beta Ray Bill" (isAllyAttack bill)
        choose "Whiplash" (isTargetChoice whiplash)
        minionDamageOf whiplash `shouldReturn` Just 3
        offers (isResponseOf bill) `shouldReturn` False

    it "not when a minion is defeated by something else" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "17012"
        bill <- allyIn "17012"
        mercenary <- addMinion p Cards.hydraMercenary
        damageMinion p mercenary 3
        minionDamageOf mercenary `shouldReturn` Nothing
        offers (isResponseOf bill) `shouldReturn` False

  describe "Yondu (17013)" do
    it "attacks with ranged: a retaliating enemy deals him nothing" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "17013"
        yondu <- allyIn "17013"
        whiplash <- addMinion p Cards.whiplash
        choose "attack with Yondu" (isAllyAttack yondu)
        choose "Whiplash" (isTargetChoice whiplash)
        minionDamageOf whiplash `shouldReturn` Just 1
        (.allyDamage) <$> allyIn "17013" `shouldReturn` 0

  describe "Air Supremacy (17014)" do
    it "deals 3 damage to each of up to X enemies, X the Aerial characters you control" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        -- Adam Warlock and Falcon are Aerial, Maria Hill is not: X is 2
        for_ ["17011", "03011", "01067"] (putIntoPlay p)
        first <- addMinion p Cards.hydraMercenary
        second <- addMinion p Cards.hydraMercenary
        air <- putInHand p "17014"
        energy <- putInHand p "01088"
        playWith air [energy]
        chooseVillain
        choose "the first mercenary" (isTargetChoice first)
        -- two chosen: the damage is dealt, nothing more asked
        offers (isTargetChoice second) `shouldReturn` False
        villainDamageNow `shouldReturn` 3
        minionDamageOf first `shouldReturn` Nothing
        minionDamageOf second `shouldReturn` Just 0

    it "may choose fewer" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        for_ ["17011", "03011"] (putIntoPlay p)
        mercenary <- addMinion p Cards.hydraMercenary
        air <- putInHand p "17014"
        energy <- putInHand p "01088"
        playWith air [energy]
        choose "the mercenary" (isTargetChoice mercenary)
        choose "done" (isLabel "$label.done")
        minionDamageOf mercenary `shouldReturn` Nothing
        villainDamageNow `shouldReturn` 0

  describe "Blaze of Glory (17015)" do
    it "gives each Guardian +2 THW and +2 ATK this phase, then deals each Guardian 1 damage" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        -- Yondu is a Guardian, Maria Hill is not
        putIntoPlay p "17013"
        putIntoPlay p "01067"
        yondu <- allyIn "17013"
        maria <- allyIn "01067"
        blaze <- putInHand p "17015"
        energy <- putInHand p "01088"
        playWith blaze [energy]
        choose "attack with Yondu" (isAllyAttack yondu)
        chooseVillain
        villainDamageNow `shouldReturn` 3
        choose "attack with Maria Hill" (isAllyAttack maria)
        chooseVillain
        villainDamageNow `shouldReturn` 4
        mariaDamage <- (.allyDamage) <$> allyIn "01067"
        -- to the first question after the phase ends: the villain's attack, before it
        -- deals damage
        start [EndPhase PlayerPhase]
        (.allyDamage) <$> allyIn "17013" `shouldReturn` 1
        (.allyDamage) <$> allyIn "01067" `shouldReturn` mariaDamage
        damageOf p `shouldReturn` 0
        getsGame (HashMap.null . gameEffects) `shouldReturn` True

    it "is max 1 per round" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        blaze <- putInHand p "17015"
        energy <- putInHand p "01088"
        playWith blaze [energy]
        again <- putInHand p "17015"
        _ <- putInHand p "01088"
        offers (isPlay again) `shouldReturn` False

  describe "Get Ready (17016)" do
    it "plays as Get Ready (01069): readies an exhausted ally" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        getReady <- putInHand p "17016"
        putIntoPlay p "01067"
        maria <- allyIn "01067"
        choose "attack with Maria Hill" (isAllyAttack maria)
        chooseVillain
        choose "play Get Ready" (isPlay getReady)
        choose "the target" (isTargetChoice (allyId maria))
        (.allyExhausted) <$> allyIn "01067" `shouldReturn` False

  describe "Target Practice (17017)" do
    it "is discarded when an ally with a Weapon attached attacks: +2 ATK for that attack" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "17013"
        yondu <- allyIn "17013"
        attachLaserBlaster p yondu
        practice <- putIntoPlay p "17017"
        choose "attack with Yondu" (isAllyAttack yondu)
        chooseVillain
        choose "use Target Practice" (isAbilityOf (supportIdOf practice))
        -- 1 ATK, +1 from Laser Blaster, +2
        villainDamageNow `shouldReturn` 4
        supportsWith "17017" `shouldReturn` []

    it "not for an ally without a Weapon" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "17013"
        yondu <- allyIn "17013"
        practice <- putIntoPlay p "17017"
        choose "attack with Yondu" (isAllyAttack yondu)
        chooseVillain
        offers (isAbilityOf (supportIdOf practice)) `shouldReturn` False
        villainDamageNow `shouldReturn` 1

  describe "The Power of Leadership (17018)" do
    it "plays as The Power of Leadership (01072): pays 2 for a Leadership card" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        maria <- putInHand p "01067"
        power <- putInHand p "17018"
        playWith maria [power]
        pass
        alliesWith "01067" >>= (`shouldSatisfy` ((== 1) . length))

  describe "Laser Blaster (17019)" do
    it "attaches to a Guardian ally, max 1 per ally" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "01067"
        laser <- putInHand p "17019"
        energy <- putInHand p "01088"
        offers (isPlay laser) `shouldReturn` False
        putIntoPlay p "17013"
        yondu <- allyIn "17013"
        playOn laser (allyId yondu) [energy]
        [blaster] <- upgradesWith "17019"
        blaster.upgradeAttachedAlly `shouldBe` Just (allyId yondu)
        again <- putInHand p "17019"
        _ <- putInHand p "01088"
        offers (isPlay again) `shouldReturn` False

    it "gives +1 ATK and overkill: the excess goes to the villain" do
      tg <- testGame ["captain-marvel"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "17013"
        yondu <- allyIn "17013"
        attachLaserBlaster p yondu
        mercenary <- addMinion p Cards.hydraMercenary
        -- 1 of its 3 hit points left: 2 ATK defeats it with 1 to spare
        damageMinion p mercenary 2
        choose "attack with Yondu" (isAllyAttack yondu)
        choose "the mercenary" (isTargetChoice mercenary)
        minionDamageOf mercenary `shouldReturn` Nothing
        villainDamageNow `shouldReturn` 1
