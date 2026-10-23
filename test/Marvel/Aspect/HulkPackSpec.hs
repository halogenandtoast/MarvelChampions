module Marvel.Aspect.HulkPackSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Minion.Cards qualified as Minions
import Marvel.Villain.Types qualified as V
import Marvel.Card.Side (Side (A))
import TestImport

hulk :: IO TestGame
hulk = testGame ["hulk"]

-- | Change to Hulk, and keep a hand of just the cards given.
asHulk :: IdentityId -> [CardCode] -> GameAppT [PlayerCard]
asHulk p codes = do
  changeForm p
  pass
  clearHand p
  traverse (putInHand p) codes

spec :: Spec
spec = describe "Hulk pack" do
  describe "Brawn (10011)" do
    it "removes 1 threat from a scheme after he attacks" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "10011"
        placeMainThreat 3
        before <- mainSchemeThreatNow
        brawn <- allyIn "10011"
        vid <- villainIdNow
        choose "attack with Brawn" (isAllyAttack brawn)
        choose "Rhino" (isTargetChoice vid)
        choose "Brawn's response" (isAbilityOf (allyId brawn))
        chooseMainScheme
        mainSchemeThreatNow `shouldReturn` (before - 1)
        villainDamageNow `shouldReturn` 1

  describe "Sentry (10012)" do
    it "deals his controller an encounter card as he enters play" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        clearHand p
        sentry <- putInHand p "10012"
        payment <- traverse (putInHand p) ["01088", "01089"]
        playWith sentry payment
        choose "Sentry's forced response" isAnyAbility
        length . (.playerIdentityEncounterCards) <$> playerAttrs p `shouldReturn` 1

  describe "She-Hulk (10013)" do
    it "gets +1 ATK for each damage on her" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "10013"
        sheHulk <- allyIn "10013"
        resolve [AllyMessage (allyId sheHulk) (AllyDamaged GameRef (toDamage 2 FromAbility))]
        vid <- villainIdNow
        choose "attack with She-Hulk" (isAllyAttack sheHulk)
        choose "Rhino" (isTargetChoice vid)
        villainDamageNow `shouldReturn` 3

  describe "Drop Kick (10014)" do
    it "deals 4 damage, and paid with only [physical] resources stuns and draws a card" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        [kick, strength, _] <- asHulk p ["10014", "01090", "01090"]
        playWith kick [strength]
        whenM (offers isPayWith') $ choose "pay" isPayWith'
        chooseIf isFinishPayment
        vid <- villainIdNow
        choose "Rhino" (isTargetChoice vid)
        villainDamageNow `shouldReturn` 4
        (V.villainStunned . toAttrs <$> getVillain) `shouldReturn` True
        length <$> handOf p `shouldReturn` 1

    it "neither stuns nor draws otherwise" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        [kick, strength, genius] <- asHulk p ["10014", "01090", "01089"]
        playWith kick [strength, genius]
        vid <- villainIdNow
        choose "Rhino" (isTargetChoice vid)
        villainDamageNow `shouldReturn` 4
        (V.villainStunned . toAttrs <$> getVillain) `shouldReturn` False
        length <$> handOf p `shouldReturn` 0

  describe "Toe to Toe (10015)" do
    it "the enemy chosen attacks you, then takes 5 damage" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        [toe, genius] <- asHulk p ["10015", "01089"]
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        playWith toe [genius]
        vid <- villainIdNow
        choose "Rhino" (isTargetChoice vid)
        choose "no defender" isNoDefender
        pass
        damageOf p `shouldReturn` 3
        villainDamageNow `shouldReturn` 5

  describe "\"You'll Pay for That!\" (10016)" do
    it "removes a threat for each damage the villain's attack dealt you" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        [pay, genius] <- asHulk p ["10016", "01089"]
        placeMainThreat 5
        before <- mainSchemeThreatNow
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks p)]
        choose "no defender" isNoDefender
        choose "You'll Pay for That!" (isPlay pay)
        choose "pay" (isPayWith genius)
        chooseIf isFinishPayment
        pass
        chooseMainScheme
        damageOf p `shouldReturn` 3
        mainSchemeThreatNow `shouldReturn` (before - 3)

    it "is played by the player whose hero defended, for the damage they took (RR Defend)" do
      tg <- testGame ["hulk", "captain-marvel"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        runNow [IdentityMessage p (ChangedToForm A), IdentityMessage other (ChangedToForm A)]
        clearHand p
        hulksPay <- putInHand p "10016"
        _ <- putInHand p "01089"
        clearHand other
        pay <- putInHand other "10016"
        genius <- putInHand other "01089"
        placeMainThreat 5
        before <- mainSchemeThreatNow
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks p)]
        choose "ask Captain Marvel to defend" (isTargetChoice other)
        choose "Captain Marvel defends" (isTargetChoice other)
        offers (isPlay hulksPay) `shouldReturn` False
        choose "You'll Pay for That!" (isPlay pay)
        payWith [genius]
        pass
        chooseMainScheme
        damageOf p `shouldReturn` 0
        -- Rhino's 2 ATK and the boost icon, less her 1 DEF
        damageOf other `shouldReturn` 2
        mainSchemeThreatNow `shouldReturn` (before - 2)

    it "is not offered after a minion attacks" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        [pay, _] <- asHulk p ["10016", "01089"]
        mercenary <- addMinion p Minions.hydraMercenary
        runNow [MinionMessage mercenary (MinionAttacks p)]
        choose "no defender" isNoDefender
        offers (isPlay pay) `shouldReturn` False

  describe "Martial Prowess (10018)" do
    it "generates a [physical] resource for an Attack event only" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        _ <- asHulk p []
        prowess <- putIntoPlay p "10018"
        rescue <- putInHand p "10019"
        _ <- putInHand p "01089"
        choose "play To the Rescue!" (isPlay rescue)
        offers (isAbilityOfCard prowess) `shouldReturn` False
        choose "pay" isPayWith'
        chooseIf isFinishPayment
        clearHand p
        blow <- putInHand p "10002"
        choose "play Crushing Blow" (isPlay blow)
        choose "pay with Martial Prowess" (isAbilityOfCard prowess)
        vid <- villainIdNow
        choose "Rhino" (isTargetChoice vid)
        villainDamageNow `shouldReturn` 3

    it "is played under any player's control, one per player" do
      tg <- testGame ["hulk", "captain-marvel"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        _ <- putIntoPlay p "10018"
        clearHand p
        prowess <- putInHand p "10018"
        strength <- putInHand p "01090"
        -- Hulk has one, so only Captain Marvel may take this
        playWith prowess [strength]
        length <$> upgradesOf p `shouldReturn` 1
        length <$> upgradesOf other `shouldReturn` 1

  describe "To the Rescue! (10019)" do
    it "removes 2 threat" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        [rescue, genius] <- asHulk p ["10019", "01089"]
        placeMainThreat 4
        before <- mainSchemeThreatNow
        playWith rescue [genius]
        chooseMainScheme
        mainSchemeThreatNow `shouldReturn` (before - 2)

  describe "Beat Cop (10029)" do
    it "moves threat to itself, then is discarded to deal that much damage to a minion" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        cop <- putIntoPlay p "10029"
        placeMainThreat 4
        before <- mainSchemeThreatNow
        choose "move threat" (isAbilityOfCard cop)
        chooseMainScheme
        mainSchemeThreatNow `shouldReturn` (before - 1)
        map (.supportThreat) <$> supportsWith "10029" `shouldReturn` [1]
        runNow [SupportMessage (supportOf cop) ReadiedSupport]
        mercenary <- addMinion p Minions.hydraMercenary
        choose "discard Beat Cop" \c -> isAbilityOfCard cop c && abilityIndexIs 2 c
        choose "the target" (isTargetChoice mercenary)
        minionDamageOf mercenary `shouldReturn` Just 1
        supportsWith "10029" `shouldReturn` []
        discardOf p >>= (`shouldSatisfy` elem cop)

  describe "Inspiring Presence (10030)" do
    it "heals 1 damage from an ally and readies it, played by an Avenger" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        [presence, genius] <- asHulk p ["10030", "01089"]
        _ <- putIntoPlay p "10011"
        brawn <- allyIn "10011"
        resolve [AllyMessage (allyId brawn) (AllyDamaged GameRef (toDamage 2 FromAbility)), AllyMessage (allyId brawn) ExhaustedAlly]
        playWith presence [genius]
        choose "the target" (isTargetChoice (allyId brawn))
        brawn' <- allyIn "10011"
        brawn'.allyDamage `shouldBe` 1
        brawn'.allyExhausted `shouldBe` False

    it "cannot be played by an identity without the Avenger trait" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        _ <- asHulk p []
        _ <- putIntoPlay p "10011"
        presence <- putInHand p "10030"
        _ <- putInHand p "01089"
        offers (isPlay presence) `shouldReturn` True
        runNow [IdentityMessage p (ChangedToForm B)]
        pass
        offers (isPlay presence) `shouldReturn` False

  describe "Electrostatic Armor (10031)" do
    it "deals 1 damage to the attacker after you defend" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        _ <- asHulk p []
        armor <- putIntoPlay p "10031"
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks p)]
        choose "defend" isDefend
        choose "Electrostatic Armor" (isAbilityOfCard armor)
        pass
        villainDamageNow `shouldReturn` 1

    it "is played under another player's control, answering that player defending" do
      tg <- testGame ["hulk", "captain-marvel"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        runNow [IdentityMessage p (ChangedToForm A), IdentityMessage other (ChangedToForm A)]
        clearHand p
        armor <- putInHand p "10031"
        energy <- putInHand p "01088"
        playOn armor other [energy]
        length <$> upgradesOf other `shouldReturn` 1
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks other)]
        choose "Captain Marvel defends" isDefend
        fmap fst <$> asked `shouldReturn` Just other
        choose "Electrostatic Armor" (isAbilityOfCard armor)
        pass
        villainDamageNow `shouldReturn` 1

  describe "Resourceful (10032)" do
    it "is discarded for a [wild] resource" do
      tg <- hulk
      let p = player1 tg
      inGame tg do
        [rescue] <- asHulk p ["10019"]
        resourceful <- putIntoPlay p "10032"
        _ <- putInHand p "01089"
        placeMainThreat 4
        before <- mainSchemeThreatNow
        choose "play To the Rescue!" (isPlay rescue)
        choose "pay with Resourceful" (isAbilityOfCard resourceful)
        choose "pay" isPayWith'
        chooseIf isFinishPayment
        chooseMainScheme
        mainSchemeThreatNow `shouldReturn` (before - 2)
        upgradeCodes `shouldReturn` []
        getsGame (HashMap.null . gameUpgrades) `shouldReturn` True
 where
  abilityIndexIs n = \case
    UseAbility ab -> abilityIndex ab == n
    _ -> False
  isPayWith' = \case
    PayWithCard _ -> True
    _ -> False
