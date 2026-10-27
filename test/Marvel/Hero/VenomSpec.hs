module Marvel.Hero.VenomSpec (spec) where

import Data.List (sort)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Payment.Types (Payment (NoPayment))
import TestImport

venomGame :: IO TestGame
venomGame = testGame ["venom"]

-- | Venom in hero form, his hand empty.
asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

isSymbioticBond :: Choice -> Bool
isSymbioticBond = isNamedAbility "Symbiotic Bond"

pickVillain :: GameAppT ()
pickVillain = chooseIf . isTargetChoice =<< villainIdNow

pickMainScheme :: GameAppT ()
pickMainScheme = chooseIf . isTargetChoice =<< mainSchemeIdNow

weapons :: [CardCode]
weapons = ["20008", "20010", "20015", "20022"]

codesOf :: [PlayerCard] -> [CardCode]
codesOf = map (toCardCode . getCardDef)

-- | Rhino attacks, a Hydra Mercenary (1 boost icon) his boost card: 3 damage.
rhinoAttacks :: IdentityId -> GameAppT ()
rhinoAttacks p = do
  _ <- stackEncounterDeck [Minions.hydraMercenary]
  vid <- villainIdNow
  runNow [VillainMessage vid (VillainAttacks p)]

spec :: Spec
spec = describe "Venom" do
  describe "Venom (20001a)" do
    it "Symbiotic Bond takes 1 damage to generate a wild resource, once per phase" do
      tg <- venomGame
      let p = player1 tg
      inGame tg do
        asHero p
        pistol <- putInHand p "20010"
        other <- putInHand p "20010"
        choose "play Venom's Pistol" (isPlay pistol)
        choose "pay with Symbiotic Bond" isSymbioticBond
        whenM (offers isFinishPayment) $ choose "finish paying" isFinishPayment
        pass
        length <$> upgradesWith "20010" `shouldReturn` 1
        damageOf p `shouldReturn` 1
        offers (isPlay other) `shouldReturn` False

    it "cannot be used while Venom has a tough status card" do
      tg <- venomGame
      let p = player1 tg
      inGame tg do
        asHero p
        pistol <- putInHand p "20010"
        offers (isPlay pistol) `shouldReturn` True
        resolve [GiveStatus (IdentityRef p) ToughStatus]
        offers (isPlay pistol) `shouldReturn` False

    it "is not Flash Thompson's" do
      tg <- venomGame
      let p = player1 tg
      inGame tg do
        clearHand p
        pistol <- putInHand p "20010"
        offers (isPlay pistol) `shouldReturn` False

  describe "Flash Thompson (20001b)" do
    it "begins the game with a Weapon upgrade in hand" do
      tg <- venomGame
      let p = player1 tg
      inGame tg do
        codesOf <$> handOf p >>= (`shouldSatisfy` any (`elem` weapons))

    it "Armed and Ready discards from the top of the deck until a Weapon upgrade, which goes to hand" do
      tg <- venomGame
      let p = player1 tg
      inGame tg do
        clearHand p
        [strength, genius, pistol] <- stackDeck p ["01090", "01089", "20010"]
        resolve [RanAbility p (IdentityRef p) 1 [] NoPayment]
        handOf p `shouldReturn` [pistol]
        discard <- discardOf p
        discard `shouldSatisfy` \d -> strength `elem` d && genius `elem` d && pistol `notElem` d

    it "Armed and Ready stops as the deck runs out without a Weapon upgrade" do
      tg <- venomGame
      let p = player1 tg
      inGame tg do
        clearHand p
        withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
          PlayerIdentity a {playerIdentityDeck = Deck []}
        cards <- stackDeck p ["01090", "01089"]
        resolve [RanAbility p (IdentityRef p) 1 [] NoPayment]
        handOf p `shouldReturn` []
        -- both discarded: the empty deck then has the discard pile shuffled back in
        discard <- discardOf p
        deck <- unDeck . (.playerIdentityDeck) <$> playerAttrs p
        (discard <> deck) `shouldSatisfy` \d -> all (`elem` d) cards

  describe "Project Rebirth 2.0 (20007)" do
    it "heals 3 damage from Flash Thompson" do
      tg <- venomGame
      let p = player1 tg
      inGame tg do
        rebirth <- putIntoPlay p "20007"
        setDamage p 5
        choose "use Project Rebirth 2.0" (isAbilityOfCard rebirth)
        choose "heal" (isLabelStarting "$label.healYourIdentity")
        damageOf p `shouldReturn` 2
        supportsWith "20007" >>= (`shouldSatisfy` all (.supportExhausted))
        offers (isAbilityOfCard rebirth) `shouldReturn` False

    it "draws 1 card instead, and heal is not offered undamaged" do
      tg <- venomGame
      let p = player1 tg
      inGame tg do
        clearHand p
        rebirth <- putIntoPlay p "20007"
        choose "use Project Rebirth 2.0" (isAbilityOfCard rebirth)
        offers (isLabelStarting "$label.healYourIdentity") `shouldReturn` False
        choose "draw" (isLabelStarting "$label.drawCards")
        length <$> handOf p `shouldReturn` 1

    it "is an alter-ego action" do
      tg <- venomGame
      let p = player1 tg
      inGame tg do
        asHero p
        rebirth <- putIntoPlay p "20007"
        offers (isAbilityOfCard rebirth) `shouldReturn` False

  describe "Multi-Gun (20008)" do
    it "exhausts to deal 2 damage to an enemy" do
      tg <- venomGame
      let p = player1 tg
      inGame tg do
        asHero p
        gun <- putIntoPlay p "20008"
        choose "use Multi-Gun" (isAbilityOfCard gun)
        choose "damage an enemy" (isLabelStarting "$label.dealDamageToAnEnemy")
        pickVillain
        pass
        villainDamageNow `shouldReturn` 2
        upgradeIsExhausted (upgradeIdOf gun) `shouldReturn` True
        offers (isAbilityOfCard gun) `shouldReturn` False

    it "deals 1 damage to each minion engaged with the player chosen" do
      tg <- testGame ["venom", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        asHero p
        _ <- addMinion p Minions.hydraMercenary
        _ <- addMinion other Minions.hydraMercenary
        _ <- addMinion other Minions.hydraMercenary
        gun <- putIntoPlay p "20008"
        choose "use Multi-Gun" (isAbilityOfCard gun)
        choose "damage minions" (isLabelStarting "$label.dealDamageToEachMinionEngagedWithAPlayer")
        choose "the other player" (isTargetChoice other)
        pass
        sort <$> minionDamages `shouldReturn` [0, 1, 1]

    it "does not offer the minion option with no minion engaged" do
      tg <- venomGame
      let p = player1 tg
      inGame tg do
        asHero p
        gun <- putIntoPlay p "20008"
        choose "use Multi-Gun" (isAbilityOfCard gun)
        offers (isLabelStarting "$label.dealDamageToEachMinionEngagedWithAPlayer") `shouldReturn` False

    it "removes 2 threat from a scheme" do
      tg <- venomGame
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 6
        before <- mainSchemeThreatNow
        gun <- putIntoPlay p "20008"
        choose "use Multi-Gun" (isAbilityOfCard gun)
        choose "remove threat" (isLabelStarting "$label.removeThreatFromAScheme")
        pickMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` before - 2

    it "does not offer removing threat with no threat on any scheme" do
      tg <- venomGame
      let p = player1 tg
      inGame tg do
        asHero p
        ms <- mainSchemeIdNow
        threat <- mainSchemeThreatNow
        resolve [MainSchemeMessage ms (MainSchemeThreatRemoved GameRef threat)]
        mainSchemeThreatNow `shouldReturn` 0
        gun <- putIntoPlay p "20008"
        choose "use Multi-Gun" (isAbilityOfCard gun)
        offers (isLabelStarting "$label.removeThreatFromAScheme") `shouldReturn` False

    it "is a hero action" do
      tg <- venomGame
      let p = player1 tg
      inGame tg do
        gun <- putIntoPlay p "20008"
        offers (isAbilityOfCard gun) `shouldReturn` False

  describe "Spider-Sense (20009)" do
    it "draws 1 card when the villain attacks Venom" do
      tg <- venomGame
      let p = player1 tg
      inGame tg do
        asHero p
        sense <- putIntoPlay p "20009"
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks p)]
        choose "use Spider-Sense" (isAbilityOfCard sense)
        length <$> handOf p `shouldReturn` 1

    it "not when a minion attacks" do
      tg <- venomGame
      let p = player1 tg
      inGame tg do
        asHero p
        sense <- putIntoPlay p "20009"
        mid <- addMinion p Minions.hydraMercenary
        runNow [MinionMessage mid (MinionAttacks p)]
        offers (isAbilityOfCard sense) `shouldReturn` False

  describe "Venom's Pistol (20010)" do
    it "exhausts for +1 THW on a basic thwart" do
      tg <- venomGame
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 6
        before <- mainSchemeThreatNow
        pistol <- putIntoPlay p "20010"
        -- the interrupt is to the power's use, before its target is chosen
        choose "thwart" (isHeroThwart p)
        choose "use Venom's Pistol" (isAbilityOfCard pistol)
        pickMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` before - 2
        upgradeIsExhausted (upgradeIdOf pistol) `shouldReturn` True

    it "two Pistols each give +1 ATK on a basic attack" do
      tg <- venomGame
      let p = player1 tg
      inGame tg do
        asHero p
        one <- putIntoPlay p "20010"
        two <- putIntoPlay p "20010"
        choose "attack" (isHeroAttack p)
        choose "use the first Pistol" (isAbilityOfCard one)
        choose "use the second Pistol" (isAbilityOfCard two)
        pickVillain
        pass
        villainDamageNow `shouldReturn` 4

    it "gives +1 DEF as Venom defends" do
      tg <- venomGame
      let p = player1 tg
      inGame tg do
        asHero p
        pistol <- putIntoPlay p "20010"
        rhinoAttacks p
        choose "Venom defends" isDefend
        choose "use Venom's Pistol" (isAbilityOfCard pistol)
        pass
        -- 3 damage against DEF 2 + 1
        damageOf p `shouldReturn` 0
