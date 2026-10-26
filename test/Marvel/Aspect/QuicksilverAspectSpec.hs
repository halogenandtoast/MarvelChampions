module Marvel.Aspect.QuicksilverAspectSpec (spec) where

import Marvel.Minion.Cards qualified as Minions
import Marvel.Villain.Types (villainIsTough)
import TestImport

quicksilver :: IO TestGame
quicksilver = testGame ["quicksilver"]

-- | Quicksilver in hero form, his hand empty.
asHero :: IdentityId -> GameAppT ()
asHero p = do
  changeForm p
  pass
  clearHand p

spec :: Spec
spec = describe "The Quicksilver pack's Aggression, Justice and Leadership cards" do
  describe "Brute Force (14029)" do
    it "gives +1 ATK and piercing to a basic attack, then is discarded" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        force <- putInHand p "14029"
        genius <- putInHand p "01089"
        playWith force [genius]
        v <- villainIdNow
        resolve [GiveStatus (toTarget v) ToughStatus]
        choose "attack" (isHeroAttack p)
        chooseVillain
        villainIsTough <$> getVillain `shouldReturn` False
        villainDamageNow `shouldReturn` 2
        chooseIf (isAbilityOfCard force)
        pass
        upgradeCodes `shouldReturn` []
        map toCardCode <$> discardOf p >>= (`shouldSatisfy` elem "14029")

    it "stays after an attack that is not a basic attack" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p "14029"
        haymaker <- putInHand p "01087"
        energy <- putInHand p "01088"
        playWith haymaker [energy]
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 3
        upgradeCodes `shouldReturn` ["14029"]

  describe "Sense of Justice (14030)" do
    it "generates a [mental] resource for a Thwart event only" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        justice <- putIntoPlay p "14030"
        haymaker <- putInHand p "01087"
        energy <- putInHand p "01088"
        choose "play Haymaker" (isPlay haymaker)
        offers (isAbilityOfCard justice) `shouldReturn` False
        choose "pay" (isPayWith energy)
        chooseIf isFinishPayment
        chooseVillain
        pass
        clearHand p
        placeMainThreat 6
        before <- mainSchemeThreatNow
        forJustice <- putInHand p "01060"
        other <- putInHand p "01087"
        choose "play For Justice!" (isPlay forJustice)
        choose "pay with Sense of Justice" (isAbilityOfCard justice)
        choose "pay" (isPayWith other)
        chooseIf isFinishPayment
        chooseMainScheme
        pass
        -- For Justice!'s 3, and 1 more for the [mental] Sense of Justice paid
        mainSchemeThreatNow `shouldReturn` before - 4
        upgradeIsExhausted (upgradeIdOf justice) `shouldReturn` True

    it "is played under any player's control, one per player" do
      tg <- testGame ["quicksilver", "captain-marvel"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        _ <- putIntoPlay p "14030"
        clearHand p
        justice <- putInHand p "14030"
        genius <- putInHand p "01089"
        playWith justice [genius]
        length <$> upgradesOf p `shouldReturn` 1
        length <$> upgradesOf other `shouldReturn` 1

  describe "United We Stand (14031)" do
    it "heals 1 damage from as many friendly characters as the villain's stage" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        setDamage p 2
        warlock <- putIntoPlay p "14013"
        resolve [AllyMessage (allyIdOf warlock) (AllyDamaged GameRef (toDamage 1 FromAbility))]
        united <- putInHand p "14031"
        playWith united []
        choose "Quicksilver" (isTargetChoice p)
        -- Rhino I: one character only
        offers (isTargetChoice (allyIdOf warlock)) `shouldReturn` False
        damageOf p `shouldReturn` 1
        (.allyDamage) <$> allyIn "14013" `shouldReturn` 1

    it "cannot be played without the Avenger trait" do
      tg <- testGame ["ms-marvel"]
      let p = player1 tg
      inGame tg do
        asHero p
        setDamage p 2
        united <- putInHand p "14031"
        offers (isPlay united) `shouldReturn` False

  describe "Beat 'Em Up (14032)" do
    it "deals 1 damage to the villain and each minion engaged with you, not another player's" do
      tg <- testGame ["quicksilver", "captain-marvel"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        asHero p
        mine <- addMinion p Minions.hydraMercenary
        pass
        theirs <- addMinion other Minions.hydraMercenary
        pass
        beat <- putInHand p "14032"
        energy <- putInHand p "01088"
        playWith beat [energy]
        pass
        villainDamageNow `shouldReturn` 1
        minionDamageOf mine `shouldReturn` Just 1
        minionDamageOf theirs `shouldReturn` Just 0
