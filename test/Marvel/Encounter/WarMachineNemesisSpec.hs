module Marvel.Encounter.WarMachineNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Card.Side (Side (A))
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero p = resolve [IdentityMessage p (ChangedToForm A)] >> clearHand p

setAmmo :: IdentityId -> Natural -> GameAppT ()
setAmmo p n = withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) -> PlayerIdentity a {playerIdentityCounters = n}

ammoOf :: IdentityId -> GameAppT Natural
ammoOf p = playerIdentityCounters . toAttrs <$> getPlayer p

wasDealt :: IdentityId -> EncounterCard -> GameAppT Bool
wasDealt p c = any ((== ecCardId c) . ecCardId) <$> dealtTo p

startLaserBoost :: IdentityId -> GameAppT ()
startLaserBoost p = do
  _ <- stackEncounterDeck [Treacheries.laserStrike]
  vid <- villainIdNow
  runNow [VillainMessage vid (VillainAttacks p)]

spec :: Spec
spec = describe "War Machine nemesis and obligation" do
  describe "Equipment Malfunction (23028)" do
    it "lets ready James Rhodes exhaust to remove it from the game without discarding or surging" do
      tg <- testGame ["war-machine"]
      let p = player1 tg
      inGame tg do
        [next] <- stackEncounterDeck [Minions.hydraMercenary]
        card <- revealNow p Obligations.equipmentMalfunction
        choose "exhaust James Rhodes" (isLabel "$label.exhaustJamesRhodesRemoveEquipmentMalfunction")
        pass
        playerIdentityExhausted . toAttrs <$> getPlayer p `shouldReturn` True
        encounterDiscard >>= (`shouldSatisfy` notElem card)
        wasDealt p next `shouldReturn` False

    it "may stay in hero form, removes all five ammo and does not surge" do
      tg <- testGame ["war-machine"]
      let p = player1 tg
      inGame tg do
        asHero p
        setAmmo p 5
        [next] <- stackEncounterDeck [Minions.hydraMercenary]
        card <- revealNow p Obligations.equipmentMalfunction
        choose "stay in hero form" (isLabel "$label.doNotFlipToAlterEgo")
        pass
        ammoOf p `shouldReturn` 0
        playerIdentityExhausted . toAttrs <$> getPlayer p `shouldReturn` False
        encounterDiscard >>= (`shouldSatisfy` elem card)
        wasDealt p next `shouldReturn` False

    for_ [0, 1, 2] \n -> it ("surges after removing " <> show n <> " ammo") do
      tg <- testGame ["war-machine"]
      let p = player1 tg
      inGame tg do
        asHero p
        setAmmo p n
        [next] <- stackEncounterDeck [Minions.hydraMercenary]
        _ <- revealNow p Obligations.equipmentMalfunction
        choose "stay in hero form" (isLabel "$label.doNotFlipToAlterEgo")
        pass
        ammoOf p `shouldReturn` 0
        wasDealt p next `shouldReturn` True

    it "does not surge when exactly three ammo are removed" do
      tg <- testGame ["war-machine"]
      let p = player1 tg
      inGame tg do
        asHero p
        setAmmo p 3
        [next] <- stackEncounterDeck [Minions.hydraMercenary]
        _ <- revealNow p Obligations.equipmentMalfunction
        choose "stay in hero form" (isLabel "$label.doNotFlipToAlterEgo")
        pass
        ammoOf p `shouldReturn` 0
        wasDealt p next `shouldReturn` False

    it "changing to alter-ego resolves the forced ammo removal before its discard choice" do
      tg <- testGame ["war-machine"]
      let p = player1 tg
      inGame tg do
        asHero p
        setAmmo p 5
        [next] <- stackEncounterDeck [Minions.hydraMercenary]
        _ <- revealNow p Obligations.equipmentMalfunction
        choose "flip" (isLabel "$label.changeToB")
        choose "resolve James Rhodes's forced response" (\case UseAbility ab -> abilitySource ab == toRef p && abilityIndex ab == 3; _ -> False)
        choose "discard with no ammo remaining" (isLabel "$label.removeAllAmmoDiscardEquipmentMalfunction")
        pass
        ammoOf p `shouldReturn` 0
        wasDealt p next `shouldReturn` True

    it "does not offer the exhaustion option when James Rhodes is already exhausted" do
      tg <- testGame ["war-machine"]
      let p = player1 tg
      inGame tg do
        resolve [IdentityMessage p ExhaustedIdentity]
        [next] <- stackEncounterDeck [Minions.hydraMercenary]
        _ <- revealNow p Obligations.equipmentMalfunction
        offers (isLabel "$label.exhaustJamesRhodesRemoveEquipmentMalfunction") `shouldReturn` False
        pass
        wasDealt p next `shouldReturn` True

    it "is given to James Rhodes even when another player reveals it" do
      tg <- testGame ["war-machine", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        asHero p
        _ <- revealNow other Obligations.equipmentMalfunction
        fmap fst <$> asked `shouldReturn` Just p
        choose "stay hero" (isLabel "$label.doNotFlipToAlterEgo")
        pass
        ammoOf p `shouldReturn` 0
        playerIdentityExhausted . toAttrs <$> getPlayer other `shouldReturn` False

  describe "Living Laser (23029)" do
    it "quickstrikes the hero for two damage immediately on entering play" do
      tg <- testGame ["war-machine"]
      let p = player1 tg
      inGame tg do
        asHero p
        card <- revealNow p Minions.livingLaser
        choose "take quickstrike" isNoDefender
        pass
        damageOf p `shouldReturn` 2
        minionDamageOf (minionOf card) `shouldReturn` Just 0

    it "does not quickstrike an alter-ego" do
      tg <- testGame ["war-machine"]
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Minions.livingLaser
        pass
        damageOf p `shouldReturn` 0

    it "pierces tough on its quickstrike attack" do
      tg <- testGame ["war-machine"]
      let p = player1 tg
      inGame tg do
        asHero p
        resolve [GiveStatus (toRef p) ToughStatus]
        _ <- revealNow p Minions.livingLaser
        choose "take quickstrike" isNoDefender
        pass
        damageOf p `shouldReturn` 2
        playerIdentityTough . toAttrs <$> getPlayer p `shouldReturn` False

    it "pierces a defending ally's tough status on subsequent attacks" do
      tg <- testGame ["war-machine"]
      let p = player1 tg
      inGame tg do
        laser <- addMinion p Minions.livingLaser
        ally <- putIntoPlay p "01068"
        asHero p
        resolve [GiveStatus (toRef $ allyIdOf ally) ToughStatus]
        runNow [MinionMessage laser (MinionAttacks p)]
        choose "Vision defends" (\case AllyDefend aid _ -> aid == allyIdOf ally; _ -> False)
        pass
        (.allyDamage) <$> allyIn "01068" `shouldReturn` 2
        (.allyTough) <$> allyIn "01068" `shouldReturn` False
        damageOf p `shouldReturn` 0

    it "has four hit points and schemes for one threat" do
      tg <- testGame ["war-machine"]
      let p = player1 tg
      inGame tg do
        laser <- addMinion p Minions.livingLaser
        before <- mainSchemeThreatNow
        resolve [MinionMessage laser MinionSchemes]
        mainSchemeThreatNow `shouldReturn` (before + 1)
        resolve [MinionMessage laser (MinionDamaged GameRef $ toDamage 3 FromAbility)]
        minionDamageOf laser `shouldReturn` Just 3
        resolve [MinionMessage laser (MinionDamaged GameRef $ toDamage 1 FromAbility)]
        minionDamageOf laser `shouldReturn` Nothing

  describe "Deadly Light Show (23030)" do
    for_ [(1, 4), (2, 5)] \(count, expected) -> it ("has fixed three plus hinder with " <> show count <> " players") do
      tg <- testGame (take count ["war-machine", "spider-man"])
      let p = player1 tg
      inGame tg do
        scheme <- revealNow p SideSchemes.deadlyLightShow
        pass
        sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` expected

    it "has crisis and blocks thwarting the main scheme while present" do
      tg <- testGame ["war-machine"]
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 3
        scheme <- revealNow p SideSchemes.deadlyLightShow
        pass
        choose "thwart" (isHeroThwart p)
        offers (isTargetChoice $ sideSchemeOf scheme) `shouldReturn` True
        main <- mainSchemeIdNow
        offers (isTargetChoice main) `shouldReturn` False

    it "deals one nonattack damage to every identity when defeated" do
      tg <- testGame ["war-machine", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        asHero p
        scheme <- revealNow p SideSchemes.deadlyLightShow
        pass
        runNow [SideSchemeMessage (sideSchemeOf scheme) (SideSchemeThwarted (toSource p) 5)]
        pass
        damageOf p `shouldReturn` 1
        damageOf other `shouldReturn` 1
        encounterDiscard >>= (`shouldSatisfy` elem scheme)

    it "opens the normal would-take-damage interrupt window for its defeat damage" do
      tg <- testGame ["war-machine"]
      let p = player1 tg
      inGame tg do
        asHero p
        event <- putInHand p "14015"
        energy <- putInHand p "01088"
        scheme <- revealNow p SideSchemes.deadlyLightShow
        pass
        runNow [SideSchemeMessage (sideSchemeOf scheme) (SideSchemeThwarted (toSource p) 4)]
        playWith event [energy]
        pass
        damageOf p `shouldReturn` 0

  describe "Laser Strike (23031)" do
    it "discards an upgrade controlled by its revealing player, not a support or another player's upgrade" do
      tg <- testGame ["war-machine", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        gun <- putIntoPlay p "23005"
        support <- putIntoPlay p "01092"
        otherPlayersCard <- putIntoPlay other "01008"
        _ <- revealNow p Treacheries.laserStrike
        offers (isTargetChoice $ supportIdOf support) `shouldReturn` False
        offers (isTargetChoice $ upgradeIdOf otherPlayersCard) `shouldReturn` False
        choose "discard Gauntlet Gun" (isTargetChoice $ upgradeIdOf gun)
        pass
        discardOf p >>= (`shouldSatisfy` elem gun)
        upgradesWith "01008" >>= (`shouldSatisfy` notNull)
        supportsWith "01092" >>= (`shouldSatisfy` notNull)

    it "surges without an upgrade even if the revealing player controls a support" do
      tg <- testGame ["war-machine"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "01092"
        [next] <- stackEncounterDeck [Minions.hydraMercenary]
        _ <- revealNow p Treacheries.laserStrike
        pass
        wasDealt p next `shouldReturn` True

    it "its undefended attack boost discards an upgrade without surging" do
      tg <- testGame ["war-machine"]
      let p = player1 tg
      inGame tg do
        asHero p
        gun <- putIntoPlay p "23005"
        startLaserBoost p
        choose "do not defend" isNoDefender
        choose "discard Gun" (isTargetChoice $ upgradeIdOf gun)
        pass
        discardOf p >>= (`shouldSatisfy` elem gun)
        damageOf p `shouldReturn` 3
        dealtTo p `shouldReturn` []

    it "its boost does nothing to upgrades during a defended attack" do
      tg <- testGame ["war-machine"]
      let p = player1 tg
      inGame tg do
        asHero p
        gun <- putIntoPlay p "23005"
        startLaserBoost p
        choose "defend" isDefend
        pass
        upgradesWith "23005" >>= (`shouldSatisfy` notNull)
        discardOf p >>= (`shouldSatisfy` notElem gun)
        damageOf p `shouldReturn` 1

    it "its undefended boost with no upgrade does not gain surge" do
      tg <- testGame ["war-machine"]
      let p = player1 tg
      inGame tg do
        asHero p
        startLaserBoost p
        choose "do not defend" isNoDefender
        pass
        dealtTo p `shouldReturn` []
        damageOf p `shouldReturn` 3
