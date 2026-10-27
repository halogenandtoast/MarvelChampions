module Marvel.Aspect.DraxPackSpec (spec) where

import Control.Monad (replicateM)
import Marvel.Identity (isPlayable)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as V
import TestImport

-- | The player in hero form, their hand empty.
asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

-- | The ability the card numbers 1, not an ally's basic attack or thwart.
isFirstAbilityOf :: (IsRef a) => a -> Choice -> Bool
isFirstAbilityOf a = \case
  UseAbility ab -> abilitySource ab == toSource a && abilityIndex ab == 1
  _ -> False

-- | Decline responses and defending until the choice is offered.
untilOffered :: (HasCallStack) => (Choice -> Bool) -> GameAppT ()
untilOffered p = go (30 :: Int)
 where
  go 0 = error "never offered"
  go n = whenM (not <$> offers p) do
    asked >>= \case
      Just (_, cs) | any isNoResponse cs -> choose "decline" isNoResponse
      Just (_, cs) | any isNoDefender cs -> choose "no defender" isNoDefender
      _ -> error "never offered"
    go (n - 1)

-- | Decline responses and defending, the choice never offered meanwhile.
neverOffered :: (HasCallStack) => (Choice -> Bool) -> GameAppT ()
neverOffered p = go (30 :: Int)
 where
  go 0 = pure ()
  go n = do
    offers p `shouldReturn` False
    asked >>= \case
      Just (_, cs) | any isNoResponse cs -> choose "decline" isNoResponse >> go (n - 1)
      Just (_, cs) | any isNoDefender cs -> choose "no defender" isNoDefender >> go (n - 1)
      _ -> pure ()

villainAttacks :: IdentityId -> GameAppT ()
villainAttacks p = villainIdNow >>= \vid -> runNow [VillainMessage vid (VillainAttacks p)]

chooseVillainIf :: GameAppT ()
chooseVillainIf = villainIdNow >>= chooseIf . isTargetChoice

deckSize :: IdentityId -> GameAppT Int
deckSize p = length . unDeck . (.playerIdentityDeck) <$> playerAttrs p

isTough :: IdentityId -> GameAppT Bool
isTough p = (.playerIdentityTough) <$> playerAttrs p

playable :: IdentityId -> PlayerCard -> GameAppT Bool
playable p card = getPlayer p >>= (`isPlayable` card)

spec :: Spec
spec = describe "Drax pack aspect and basic cards" do
  describe "Martyr — Phyla-Vell (19012)" do
    it "after her attack defeats an enemy and she takes its consequential damage, she may become tough" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "19012"
        martyr <- allyIn "19012"
        regular <- addMinion p Minions.hydraRegular
        choose "Martyr attacks" (isAllyAttack martyr)
        choose "the Hydra Regular" (isTargetChoice regular)
        untilOffered (isFirstAbilityOf (allyId martyr))
        choose "her Response" (isFirstAbilityOf (allyId martyr))
        minionDamageOf regular `shouldReturn` Nothing
        martyr' <- allyIn "19012"
        allyDamage martyr' `shouldBe` 1
        allyTough martyr' `shouldBe` True

    it "does not become tough when her attack defeats no enemy" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "19012"
        martyr <- allyIn "19012"
        merc <- addMinion p Minions.hydraMercenary
        choose "Martyr attacks" (isAllyAttack martyr)
        chooseIf (isTargetChoice merc)
        neverOffered (isFirstAbilityOf (allyId martyr))
        martyr' <- allyIn "19012"
        allyDamage martyr' `shouldBe` 1
        allyTough martyr' `shouldBe` False

  describe "Moondragon — Heather Douglas (19013)" do
    it "is discarded to have a minion attack another enemy of your choice" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        md <- putIntoPlay p "19013"
        offers (isFirstAbilityOf (allyIdOf md)) `shouldReturn` False
        regular <- addMinion p Minions.hydraRegular
        merc <- addMinion p Minions.hydraMercenary
        choose "Moondragon" (isFirstAbilityOf (allyIdOf md))
        choose "the Hydra Regular" (isTargetChoice regular)
        -- another enemy: not the minion attacking
        offers (isTargetChoice regular) `shouldReturn` False
        choose "the Hydra Mercenary" (isTargetChoice merc)
        pass
        -- the Hydra Regular's 2 ATK
        minionDamageOf merc `shouldReturn` Just 2
        alliesWith "19013" `shouldReturn` []
        map toCardCode <$> discardOf p >>= (`shouldSatisfy` elem "19013")

    it "may have the minion attack the villain" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        md <- putIntoPlay p "19013"
        regular <- addMinion p Minions.hydraRegular
        damage <- villainDamageNow
        choose "Moondragon" (isFirstAbilityOf (allyIdOf md))
        chooseIf (isTargetChoice regular)
        chooseVillainIf
        pass
        villainDamageNow `shouldReturn` (damage + 2)

  describe "Deflection (19015)" do
    it "prevents up to 5 of an attack's damage, as much as you choose, discarding that many cards from your deck" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        asHero p
        deflection <- putInHand p "19015"
        energy <- putInHand p "01088"
        -- Rhino's 2 ATK and a boost card with no icons
        _ <- stackEncounterDeck [Treacheries.hardToKeepDown]
        villainAttacks p
        untilOffered (isPlay deflection)
        playWith deflection [energy]
        offers (isLabel (countLabel "preventDamage" 3)) `shouldReturn` False
        deck <- deckSize p
        choose "prevent 1" (isLabel (countLabel "preventDamage" 1))
        pass
        damageOf p `shouldReturn` 1
        deckSize p `shouldReturn` (deck - 1)

    it "prevents and discards no more than is left once another interrupt prevented some" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        asHero p
        parry <- putInHand p "19006"
        deflection <- putInHand p "19015"
        energy <- putInHand p "01088"
        -- Rhino's 2 ATK and 2 boost icons
        _ <- stackEncounterDeck [Treacheries.shadowOfThePast]
        villainAttacks p
        untilOffered (isPlay parry)
        -- double Drax's 1 ATK: 2 of the 4 prevented
        playWith parry []
        untilOffered (isPlay deflection)
        playWith deflection [energy]
        offers (isLabel (countLabel "preventDamage" 3)) `shouldReturn` False
        deck <- deckSize p
        choose "prevent 2" (isLabel (countLabel "preventDamage" 2))
        pass
        damageOf p `shouldReturn` 0
        deckSize p `shouldReturn` (deck - 2)

    it "answers no damage but an attack's" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        asHero p
        deflection <- putInHand p "19015"
        _ <- putInHand p "01088"
        start (damageIdentity p (toSource p) (toDamage 3 FromAbility))
        neverOffered (isPlay deflection)
        damageOf p `shouldReturn` 3

  describe "Hard Knocks (19016)" do
    it "deals 4 damage, and your hero becomes tough when it defeats the enemy" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        asHero p
        regular <- addMinion p Minions.hydraRegular
        knocks <- putInHand p "19016"
        energies <- replicateM 2 (putInHand p "01088")
        playWith knocks energies
        choose "the Hydra Regular" (isTargetChoice regular)
        pass
        minionDamageOf regular `shouldReturn` Nothing
        isTough p `shouldReturn` True

    it "gives no tough status card when the enemy survives" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        asHero p
        knocks <- putInHand p "19016"
        energies <- replicateM 2 (putInHand p "01088")
        damage <- villainDamageNow
        playWith knocks energies
        chooseVillainIf
        pass
        villainDamageNow `shouldReturn` (damage + 4)
        isTough p `shouldReturn` False

  describe "Leading Blow (19017)" do
    it "discards the top encounter card; with no boost icons the attack still deals damage and readies your hero" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        asHero p
        blow <- putInHand p "19017"
        _ <- stackEncounterDeck [Treacheries.hardToKeepDown]
        damage <- villainDamageNow
        choose "attack" (isHeroAttack p)
        choose "Leading Blow" (isPlay blow)
        payWith []
        chooseVillainIf
        pass
        encounterDiscardCodes >>= (`shouldSatisfy` elem (toCardCode Treacheries.hardToKeepDown))
        -- Drax's 1 ATK
        villainDamageNow `shouldReturn` (damage + 1)
        exhaustedNow p `shouldReturn` False

    it "reduces your hero's ATK by its printed boost icons, and does not ready a hero dealing no damage" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        asHero p
        blow <- putInHand p "19017"
        _ <- stackEncounterDeck [Treacheries.shadowOfThePast]
        damage <- villainDamageNow
        choose "attack" (isHeroAttack p)
        choose "Leading Blow" (isPlay blow)
        payWith []
        chooseVillainIf
        pass
        villainDamageNow `shouldReturn` damage
        exhaustedNow p `shouldReturn` True

    it "reduces the ATK every other modifier gives, not one taken before them" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        asHero p
        -- Drax's Knife: ATK 2
        _ <- putIntoPlay p "19008"
        blow <- putInHand p "19017"
        _ <- stackEncounterDeck [Treacheries.shadowOfThePast]
        damage <- villainDamageNow
        choose "attack" (isHeroAttack p)
        choose "Leading Blow" (isPlay blow)
        payWith []
        chooseVillainIf
        pass
        -- 1 + 1 - 2: no damage, and no ready
        villainDamageNow `shouldReturn` damage
        exhaustedNow p `shouldReturn` True

  describe "Subdue (19018)" do
    it "the attacking enemy gets -3 ATK for that attack, after its boost" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        asHero p
        subdue <- putInHand p "19018"
        resource <- putInHand p "01089"
        -- Rhino's 2 ATK and 2 boost icons, less 3
        _ <- stackEncounterDeck [Treacheries.shadowOfThePast]
        villainAttacks p
        untilOffered (isPlay subdue)
        playWith subdue [resource]
        pass
        damageOf p `shouldReturn` 1

  describe "Gamora (19020)" do
    it "is played only by a Guardian identity" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        clearHand p
        gamora <- putInHand p "19020"
        _ <- replicateM 2 (putInHand p "01088")
        -- Drax's alter-ego is an Outlaw
        playable p gamora `shouldReturn` False
        changeForm p >> pass
        playable p gamora `shouldReturn` True

    it "is not played by an identity without the Guardian trait" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        gamora <- putInHand p "19020"
        _ <- replicateM 2 (putInHand p "01088")
        playable p gamora `shouldReturn` False

    it "after she attacks, discards from your deck until an event and adds it to your hand" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        asHero p
        gamora <- putIntoPlay p "19020"
        [strength, haymaker] <- stackDeck p ["01090", "01087"]
        ga <- allyIn "19020"
        choose "Gamora attacks" (isAllyAttack ga)
        chooseVillainIf
        untilOffered (isFirstAbilityOf (allyIdOf gamora))
        choose "Gamora" (isFirstAbilityOf (allyIdOf gamora))
        handOf p `shouldReturn` [haymaker]
        discardOf p >>= (`shouldSatisfy` elem strength)

    it "after she thwarts, too" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 3
        gamora <- putIntoPlay p "19020"
        [_, haymaker] <- stackDeck p ["01090", "01087"]
        ga <- allyIn "19020"
        choose "Gamora thwarts" (isAllyThwart ga)
        mainSchemeIdNow >>= chooseIf . isTargetChoice
        untilOffered (isFirstAbilityOf (allyIdOf gamora))
        choose "Gamora" (isFirstAbilityOf (allyIdOf gamora))
        handOf p `shouldReturn` [haymaker]

    it "is a Hero Response: not used in alter-ego form" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        clearHand p
        gamora <- putIntoPlay p "19020"
        ga <- allyIn "19020"
        choose "Gamora attacks" (isAllyAttack ga)
        chooseVillainIf
        neverOffered (isFirstAbilityOf (allyIdOf gamora))

  describe "\"Bring It!\" (19030)" do
    it "draws 1 card for each minion engaged with you, max 1 per phase" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- addMinion p Minions.hydraRegular
        _ <- addMinion p Minions.hydraRegular
        clearHand p
        bringIt <- putInHand p "19030"
        again <- putInHand p "19030"
        playWith bringIt []
        length <$> handOf p `shouldReturn` 3
        offers (isPlay again) `shouldReturn` False

    it "draws nothing with no minion engaged" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        asHero p
        bringIt <- putInHand p "19030"
        playWith bringIt []
        handOf p `shouldReturn` []

  describe "\"Think Fast!\" (19031)" do
    it "deals you 1 damage and confuses the villain" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        asHero p
        thinkFast <- putInHand p "19031"
        resource <- putInHand p "01089"
        playWith thinkFast [resource]
        pass
        damageOf p `shouldReturn` 1
        field V.VillainConfused <$> getVillain `shouldReturn` True

    it "is played only by a Guardian identity" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        asHero p
        thinkFast <- putInHand p "19031"
        _ <- putInHand p "01089"
        playable p thinkFast `shouldReturn` False

  describe "Regroup (19032)" do
    it "returns an ally defeated by an enemy attack to its owner's hand" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        clearHand p
        regroup <- putIntoPlay p "19032"
        md <- putIntoPlay p "19013"
        -- Rhino's 2 ATK and a boost card with no icons: Moondragon's 2 hit points
        _ <- stackEncounterDeck [Treacheries.hardToKeepDown]
        villainAttacks p
        let defends = \case
              AllyDefend a _ -> a == allyIdOf md
              _ -> False
        untilOffered defends
        choose "Moondragon defends" defends
        untilOffered (isFirstAbilityOf (supportIdOf regroup))
        choose "Regroup" (isFirstAbilityOf (supportIdOf regroup))
        pass
        alliesWith "19013" `shouldReturn` []
        map toCardCode <$> handOf p `shouldReturn` ["19013"]
        map toCardCode <$> discardOf p >>= (`shouldSatisfy` notElem "19013")

    it "does not answer an ally defeated by anything but an attack" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        clearHand p
        regroup <- putIntoPlay p "19032"
        md <- putIntoPlay p "19013"
        start [AllyMessage (allyIdOf md) (AllyDamaged (toSource p) (toDamage 2 FromAbility))]
        neverOffered (isFirstAbilityOf (supportIdOf regroup))
        alliesWith "19013" `shouldReturn` []
        handOf p `shouldReturn` []

    it "is discarded when the round ends" do
      tg <- testGame ["drax"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "19032"
        resolve [EndRound]
        supportsWith "19032" `shouldReturn` []
