module Marvel.Hero.PhoenixSignatureSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query
import TestImport

asPhoenix :: IdentityId -> GameAppT ()
asPhoenix you = changeForm you >> pass >> clearHand you

-- Resolve the permanent upgrade's mandatory response before continuing an event.
settlePhoenix :: GameAppT ()
settlePhoenix = do
  pass
  forces <- (<>) <$> upgradesWith "34002a" <*> upgradesWith "34002b"
  offered <- filterM (offers . isAbilityOf . upgradeId) forces
  for_ (headMay offered) $ \force -> do
    choose "forced Phoenix Force flip" (isAbilityOf $ upgradeId force)
    settlePhoenix

forceId :: IdentityId -> GameAppT UpgradeId
forceId you = selectJust (UpgradeNamed "Phoenix Force" <> UpgradeControlledBy (IdentityWithId you))

powerOf :: IdentityId -> GameAppT Natural
powerOf you = do
  uid <- forceId you
  getsGame (upgradeUses . toAttrs . fromMaybe (error "Phoenix Force missing") . HashMap.lookup uid . gameUpgrades)

unleash :: IdentityId -> GameAppT ()
unleash you = do
  uid <- forceId you
  resolve $ replicate 4 (UpgradeMessage uid SpendUpgradeUse)

playSignature :: IdentityId -> CardCode -> GameAppT ()
playSignature you code = do
  card <- putInHand you code
  payment <- traverse (putInHand you) ["01088", "01089"]
  playWith card payment

spec :: Spec
spec = describe "PhoenixSignature" do
  describe "Telekinetic Attack" do
    it "deals seven damage while Restrained" do
      tg <- testGame ["phoenix"]
      inGame tg do
        let you = player1 tg
        asPhoenix you
        playSignature you "34010"
        chooseVillain
        settlePhoenix
        villainDamageNow `shouldReturn` 7

    it "deals nine damage while Unleashed" do
      tg <- testGame ["phoenix"]
      inGame tg do
        let you = player1 tg
        asPhoenix you
        unleash you
        playSignature you "34010"
        chooseVillain
        settlePhoenix
        villainDamageNow `shouldReturn` 9

    it "does not overkill a minion while Restrained" do
      tg <- testGame ["phoenix"]
      inGame tg do
        let you = player1 tg
        asPhoenix you
        mid <- addMinion you Minions.shocker
        playSignature you "34010"
        choose "Shocker" (isTargetChoice mid)
        settlePhoenix
        minionDamageOf mid `shouldReturn` Nothing
        villainDamageNow `shouldReturn` 0

    it "overkills a minion for six excess damage while Unleashed" do
      tg <- testGame ["phoenix"]
      inGame tg do
        let you = player1 tg
        asPhoenix you
        unleash you
        mid <- addMinion you Minions.shocker
        playSignature you "34010"
        choose "Shocker" (isTargetChoice mid)
        settlePhoenix
        minionDamageOf mid `shouldReturn` Nothing
        villainDamageNow `shouldReturn` 6

    it "is an attack and is replaced by stun" do
      tg <- testGame ["phoenix"]
      inGame tg do
        let you = player1 tg
        asPhoenix you
        resolve [GiveStatus (IdentityRef you) StunnedStatus]
        playSignature you "34010"
        settlePhoenix
        villainDamageNow `shouldReturn` 0
        isStunned you `shouldReturn` False

    it "cannot be played in alter-ego form" do
      tg <- testGame ["phoenix"]
      inGame tg do
        let you = player1 tg
        card <- putInHand you "34010"
        _ <- putInHand you "01088"
        _ <- putInHand you "01089"
        offers (isPlay card) `shouldReturn` False

  describe "Psychic Blast" do
    it "damages the villain but not engaged minions while Restrained" do
      tg <- testGame ["phoenix"]
      inGame tg do
        let you = player1 tg
        asPhoenix you
        mid <- addMinion you Minions.shocker
        playSignature you "34011"
        settlePhoenix
        villainDamageNow `shouldReturn` 4
        minionDamageOf mid `shouldReturn` Just 0

    it "damages all minions engaged with its player while Unleashed" do
      tg <- testGame ["phoenix", "spider-man"]
      inGame tg do
        let you = player1 tg
            other = player2 tg
        asPhoenix you
        unleash you
        one <- addMinion you Minions.hydraSoldier
        two <- addMinion you Minions.hydraSoldier
        theirs <- addMinion other Minions.hydraSoldier
        playSignature you "34011"
        settlePhoenix
        villainDamageNow `shouldReturn` 4
        minionDamageOf one `shouldReturn` Nothing
        minionDamageOf two `shouldReturn` Nothing
        minionDamageOf theirs `shouldReturn` Just 0

    it "is not an attack, so stun and guard do not stop its villain damage" do
      tg <- testGame ["phoenix"]
      inGame tg do
        let you = player1 tg
        asPhoenix you
        _ <- addMinion you Minions.hydraSoldier
        resolve [GiveStatus (IdentityRef you) StunnedStatus]
        playSignature you "34011"
        settlePhoenix
        villainDamageNow `shouldReturn` 4
        isStunned you `shouldReturn` True

    it "cannot be played in alter-ego form" do
      tg <- testGame ["phoenix"]
      inGame tg do
        card <- putInHand (player1 tg) "34011"
        _ <- putInHand (player1 tg) "01088"
        offers (isPlay card) `shouldReturn` False

  describe "Telepathic Trickery" do
    it "removes four threat and does not give statuses while Restrained" do
      tg <- testGame ["phoenix"]
      inGame tg do
        let you = player1 tg
        asPhoenix you
        placeMainThreat 5
        before <- mainSchemeThreatNow
        playSignature you "34012"
        chooseMainScheme
        settlePhoenix
        mainSchemeThreatNow `shouldReturn` (before - 4)
        vid <- villainIdNow
        selectAny (StunnedEnemy <> EnemyWithId (EnemyVillainId vid)) `shouldReturn` False
        selectAny (ConfusedEnemy <> EnemyWithId (EnemyVillainId vid)) `shouldReturn` False

    it "removes threat then stuns and confuses an enemy while Unleashed" do
      tg <- testGame ["phoenix"]
      inGame tg do
        let you = player1 tg
        asPhoenix you
        unleash you
        placeMainThreat 5
        before <- mainSchemeThreatNow
        playSignature you "34012"
        chooseMainScheme
        chooseVillain
        settlePhoenix
        mainSchemeThreatNow `shouldReturn` (before - 4)
        vid <- villainIdNow
        selectAny (StunnedEnemy <> EnemyWithId (EnemyVillainId vid)) `shouldReturn` True
        selectAny (ConfusedEnemy <> EnemyWithId (EnemyVillainId vid)) `shouldReturn` True

    it "can stun and confuse while Unleashed even when there is no threat to remove" do
      tg <- testGame ["phoenix"]
      inGame tg do
        let you = player1 tg
        asPhoenix you
        unleash you
        mid <- mainSchemeIdNow
        resolve [MainSchemeMessage mid (MainSchemeThreatRemoved GameRef 100)]
        playSignature you "34012"
        chooseVillain
        settlePhoenix
        vid <- villainIdNow
        selectAny (StunnedEnemy <> EnemyWithId (EnemyVillainId vid)) `shouldReturn` True
        selectAny (ConfusedEnemy <> EnemyWithId (EnemyVillainId vid)) `shouldReturn` True

    it "is a thwart and confusion replaces both its threat removal and statuses" do
      tg <- testGame ["phoenix"]
      inGame tg do
        let you = player1 tg
        asPhoenix you
        unleash you
        placeMainThreat 5
        before <- mainSchemeThreatNow
        resolve [GiveStatus (IdentityRef you) ConfusedStatus]
        playSignature you "34012"
        settlePhoenix
        mainSchemeThreatNow `shouldReturn` before
        isConfused you `shouldReturn` False
        vid <- villainIdNow
        selectAny (StunnedEnemy <> EnemyWithId (EnemyVillainId vid)) `shouldReturn` False

    it "a confused Restrained Phoenix can pay for Trickery with zero threat to remove only her confusion" do
      tg <- testGame ["phoenix"]
      inGame tg do
        let you = player1 tg
        asPhoenix you
        main <- mainSchemeIdNow
        resolve [MainSchemeMessage main (MainSchemeThreatRemoved GameRef 100)]
        resolve [GiveStatus (IdentityRef you) ConfusedStatus]
        trickery <- putInHand you "34012"
        energy <- putInHand you "01088"
        offers (isPlay trickery) `shouldReturn` True
        playWith trickery [energy]
        settlePhoenix
        isConfused you `shouldReturn` False
        mainSchemeThreatNow `shouldReturn` 0
        powerOf you `shouldReturn` 4
        upgradeCodes >>= (`shouldSatisfy` elem "34002a")
        vid <- villainIdNow
        selectAny (StunnedEnemy <> EnemyWithId (EnemyVillainId vid)) `shouldReturn` False
        selectAny (ConfusedEnemy <> EnemyWithId (EnemyVillainId vid)) `shouldReturn` False
        (map toCardCode <$> discardOf you) >>= (`shouldSatisfy` elem "34012")
        (map toCardCode <$> discardOf you) >>= (`shouldSatisfy` elem "01088")
        offers isEndTurn `shouldReturn` True

  describe "Phoenix Firebird" do
    it "can place two power counters while Restrained" do
      tg <- testGame ["phoenix"]
      inGame tg do
        let you = player1 tg
        asPhoenix you
        playSignature you "34013"
        choose "place power" (isLabel "$label.phoenixFirebirdPower")
        settlePhoenix
        powerOf you `shouldReturn` 6

    it "can spend a power counter to ready Phoenix" do
      tg <- testGame ["phoenix"]
      inGame tg do
        let you = player1 tg
        asPhoenix you
        resolve [IdentityMessage you ExhaustedIdentity]
        playSignature you "34013"
        choose "ready Phoenix" (isLabel "$label.phoenixFirebirdReady")
        settlePhoenix
        powerOf you `shouldReturn` 3
        isExhaustedPlayer you `shouldReturn` False

    it "does not offer readying if Phoenix is already ready" do
      tg <- testGame ["phoenix"]
      inGame tg do
        let you = player1 tg
        asPhoenix you
        playSignature you "34013"
        offers (isLabel "$label.phoenixFirebirdReady") `shouldReturn` False
        choose "place power" (isLabel "$label.phoenixFirebirdPower")
        settlePhoenix

    it "cannot pay the readying cost with no power counters" do
      tg <- testGame ["phoenix"]
      inGame tg do
        let you = player1 tg
        asPhoenix you
        unleash you
        resolve [IdentityMessage you ExhaustedIdentity]
        playSignature you "34013"
        offers (isLabel "$label.phoenixFirebirdReady") `shouldReturn` False
        choose "place power" (isLabel "$label.phoenixFirebirdPower")
        settlePhoenix
        powerOf you `shouldReturn` 2
        isExhaustedPlayer you `shouldReturn` True

    it "adding two counters twice while Unleashed restrains Phoenix Force at four" do
      tg <- testGame ["phoenix"]
      inGame tg do
        let you = player1 tg
        asPhoenix you
        unleash you
        for_ [1 :: Int, 2] $ \_ -> do
          playSignature you "34013"
          choose "place power" (isLabel "$label.phoenixFirebirdPower")
          settlePhoenix
        powerOf you `shouldReturn` 4
        upgradeCodes >>= (`shouldSatisfy` elem "34002a")

    it "spending the last counter to ready Phoenix unleashes Phoenix Force" do
      tg <- testGame ["phoenix"]
      inGame tg do
        let you = player1 tg
        asPhoenix you
        uid <- forceId you
        resolve $ replicate 3 (UpgradeMessage uid SpendUpgradeUse)
        resolve [IdentityMessage you ExhaustedIdentity]
        playSignature you "34013"
        choose "ready Phoenix" (isLabel "$label.phoenixFirebirdReady")
        settlePhoenix
        powerOf you `shouldReturn` 0
        isExhaustedPlayer you `shouldReturn` False
        upgradeCodes >>= (`shouldSatisfy` elem "34002b")
