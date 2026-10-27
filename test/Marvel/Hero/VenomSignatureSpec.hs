module Marvel.Hero.VenomSignatureSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Minion.Cards qualified as Minions
import Marvel.Villain.Types qualified as V
import TestImport

venom :: IO TestGame
venom = testGame ["venom"]

-- | Venom in hero form, his hand empty.
asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

behindEnemyLines, graspingTendrils, lockedAndLoaded, runAndGun, savageAttack :: CardCode
behindEnemyLines = "20002"
graspingTendrils = "20003"
lockedAndLoaded = "20004"
runAndGun = "20005"
savageAttack = "20006"

-- basic resources: 2 [energy], 2 [mental], 2 [physical]
energyCard, geniusCard, strengthCard :: CardCode
energyCard = "01088"
geniusCard = "01089"
strengthCard = "01090"

venomsPistol, multiGun, spiderSense, plasmaPistol, haymaker, counterPunch :: CardCode
venomsPistol = "20010"
multiGun = "20008"
spiderSense = "20009"
plasmaPistol = "20022"
haymaker = "01087"
counterPunch = "01077"

villainConfusedNow, villainStunnedNow :: GameAppT Bool
villainConfusedNow = V.villainConfused . toAttrs <$> getVillain
villainStunnedNow = V.villainStunned . toAttrs <$> getVillain

minionConfusedNow :: MinionId -> GameAppT Bool
minionConfusedNow m = getsGame (maybe False (minionConfused . toAttrs) . HashMap.lookup m . gameMinions)

deckOf :: IdentityId -> GameAppT [PlayerCard]
deckOf p = unDeck . (.playerIdentityDeck) <$> playerAttrs p

-- | The villain attacks the player, a hydra mercenary on the encounter deck to boost it.
villainAttacks :: IdentityId -> GameAppT EncounterCard
villainAttacks p = do
  [boost] <- stackEncounterDeck [Minions.hydraMercenary]
  vid <- villainIdNow
  runNow [VillainMessage vid (VillainAttacks p)]
  pure boost

spec :: Spec
spec = describe "Venom's signature events" do
  describe "Behind Enemy Lines (20002)" do
    it "removes 3 threat from a scheme, and paid using only [mental], confuses an enemy" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 5
        before <- mainSchemeThreatNow
        card <- putInHand p behindEnemyLines
        genius <- putInHand p geniusCard
        playWith card [genius]
        chooseMainScheme
        chooseVillain
        mainSchemeThreatNow `shouldReturn` (before - 3)
        villainConfusedNow `shouldReturn` True

    it "may confuse a minion" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 5
        mercenary <- addMinion p Minions.hydraMercenary
        card <- putInHand p behindEnemyLines
        other <- putInHand p behindEnemyLines
        genius <- putInHand p geniusCard
        pass
        playWith card [other, genius]
        chooseMainScheme
        choose "the mercenary" (isTargetChoice mercenary)
        minionConfusedNow mercenary `shouldReturn` True
        villainConfusedNow `shouldReturn` False

    it "a wild resource declared [mental] is still only [mental]" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 5
        card <- putInHand p behindEnemyLines
        other <- putInHand p behindEnemyLines
        wild <- putInHand p multiGun
        playDeclaring card [other, wild] Mental
        chooseMainScheme
        chooseVillain
        villainConfusedNow `shouldReturn` True

    it "paid with any other resource, confuses no one" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 5
        before <- mainSchemeThreatNow
        card <- putInHand p behindEnemyLines
        other <- putInHand p behindEnemyLines
        energy <- putInHand p haymaker
        playWith card [other, energy]
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` (before - 3)
        villainConfusedNow `shouldReturn` False

    it "is a hero action" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        clearHand p
        placeMainThreat 5
        card <- putInHand p behindEnemyLines
        _ <- putInHand p geniusCard
        offers (isPlay card) `shouldReturn` False

  describe "Grasping Tendrils (20003)" do
    it "cancels the villain's attack against you: no boost, no damage, and you defended it" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        tendrils <- putInHand p graspingTendrils
        strength <- putInHand p strengthCard
        punch <- putInHand p counterPunch
        boost <- villainAttacks p
        playWith tendrils [strength]
        -- an "after your hero defends" response may answer it
        offers (isPlay punch) `shouldReturn` True
        pass
        damageOf p `shouldReturn` 0
        -- the attack was cancelled as it was initiated: no boost card was dealt
        map ecCardId . take 1 <$> scenarioEncounterDeckNow `shouldReturn` [ecCardId boost]
        -- paid using only [physical]: the villain is stunned
        villainStunnedNow `shouldReturn` True

    it "paid with any other resource, cancels the attack but does not stun the villain" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        tendrils <- putInHand p graspingTendrils
        genius <- putInHand p geniusCard
        _ <- villainAttacks p
        playWith tendrils [genius]
        pass
        damageOf p `shouldReturn` 0
        villainStunnedNow `shouldReturn` False

    it "is not offered when a minion attacks you" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        mercenary <- addMinion p Minions.hydraMercenary
        tendrils <- putInHand p graspingTendrils
        _ <- putInHand p strengthCard
        runNow [MinionMessage mercenary (MinionAttacks p)]
        offers (isPlay tendrils) `shouldReturn` False

    it "is not offered when the villain attacks another player" do
      tg <- testGame ["venom", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        asHero p
        tendrils <- putInHand p graspingTendrils
        _ <- putInHand p strengthCard
        _ <- villainAttacks other
        offers (isPlay tendrils) `shouldReturn` False

    it "is not offered in alter-ego form" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        clearHand p
        tendrils <- putInHand p graspingTendrils
        _ <- putInHand p strengthCard
        _ <- villainAttacks p
        offers (isPlay tendrils) `shouldReturn` False

  describe "Locked and Loaded (20004)" do
    it "searches your deck for a Weapon upgrade and adds it to your hand, in either form" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        clearHand p
        [sense, pistol] <- stackDeck p [spiderSense, venomsPistol]
        before <- length <$> deckOf p
        card <- putInHand p lockedAndLoaded
        playWith card []
        -- an upgrade that is no Weapon is not found
        offers (isTargetChoice sense) `shouldReturn` False
        choose "take the pistol" (isTargetChoice pistol)
        handOf p `shouldReturn` [pistol]
        length <$> deckOf p `shouldReturn` (before - 1)

  describe "Run and Gun (20005)" do
    it "readies Venom and each Weapon upgrade you control, and nothing else" do
      tg <- testGame ["venom", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        asHero p
        pistol <- upgradeIdOf <$> putIntoPlay p venomsPistol
        plasma <- upgradeIdOf <$> putIntoPlay p plasmaPistol
        sense <- upgradeIdOf <$> putIntoPlay p spiderSense
        theirs <- upgradeIdOf <$> putIntoPlay other plasmaPistol
        resolve $
          IdentityMessage p ExhaustedIdentity
            : [UpgradeMessage u ExhaustedUpgrade | u <- [pistol, plasma, sense, theirs]]
        card <- putInHand p runAndGun
        energy <- putInHand p energyCard
        punch <- putInHand p haymaker
        playWith card [energy, punch]
        pass
        isExhaustedNow p `shouldReturn` False
        upgradeIsExhausted pistol `shouldReturn` False
        upgradeIsExhausted plasma `shouldReturn` False
        upgradeIsExhausted sense `shouldReturn` True
        upgradeIsExhausted theirs `shouldReturn` True

    it "is a hero action" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        clearHand p
        card <- putInHand p runAndGun
        _ <- putInHand p energyCard
        _ <- putInHand p haymaker
        offers (isPlay card) `shouldReturn` False

  describe "Savage Attack (20006)" do
    it "deals 5 damage to an enemy" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        card <- putInHand p savageAttack
        strength <- putInHand p strengthCard
        playWith card [strength]
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 5

    it "paid using only [energy], the attack gains overkill" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        mercenary <- addMinion p Minions.hydraMercenary
        card <- putInHand p savageAttack
        energy <- putInHand p energyCard
        pass
        playWith card [energy]
        choose "the mercenary" (isTargetChoice mercenary)
        pass
        minionDamageOf mercenary `shouldReturn` Nothing
        -- 3 hit points, so 2 excess damage reaches the villain
        villainDamageNow `shouldReturn` 2

    it "paid with any other resource, has no overkill" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        asHero p
        mercenary <- addMinion p Minions.hydraMercenary
        card <- putInHand p savageAttack
        other <- putInHand p savageAttack
        energy <- putInHand p haymaker
        pass
        playWith card [other, energy]
        choose "the mercenary" (isTargetChoice mercenary)
        pass
        minionDamageOf mercenary `shouldReturn` Nothing
        villainDamageNow `shouldReturn` 0

    it "is a hero action" do
      tg <- venom
      let p = player1 tg
      inGame tg do
        clearHand p
        card <- putInHand p savageAttack
        _ <- putInHand p strengthCard
        offers (isPlay card) `shouldReturn` False
