module Marvel.Aspect.MagikPackSpec (spec) where

import Marvel.Status
import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

asMagik :: IdentityId -> GameAppT ()
asMagik p = changeForm p >> pass >> clearHand p

paying :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
paying p code codes = do
  card <- putInHand p code
  payments <- traverse (putInHand p) codes
  playWith card payments
  pure card

isAbilityOfIndex :: IsRef a => a -> Natural -> Choice -> Bool
isAbilityOfIndex target n = \case
  UseAbility ab -> abilitySource ab == toSource target && abilityIndex ab == n
  _ -> False

spec :: Spec
spec = describe "Magik pack" do
  it "Goldballs discards up to three cards for that attack's bonus only" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      asMagik p
      goldballs <- putIntoPlay p "45041"
      _ <- stackDeck p ["01086", "01087", "01093"]
      choose "Goldballs attacks" (isAbilityOfIndex (allyIdOf goldballs) 301)
      chooseVillain
      choose "Goldballs's interrupt" (isAbilityOfIndex (allyIdOf goldballs) 1)
      choose "discard three cards" (isLabel $ countLabel "discardTopOfDeck" 3)
      pass
      villainDamageNow `shouldReturn` 4
      resolve [AllyMessage (allyIdOf goldballs) ReadiedAlly]
      choose "attack again" (isAbilityOfIndex (allyIdOf goldballs) 301)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 5

  it "Tempus cancels a villain scheme and deals her controller an encounter card" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      tempus <- putIntoPlay p "45042"
      clearHand p
      before <- mainSchemeThreatNow
      dealt <- length . playerIdentityEncounterCards <$> playerAttrs p
      v <- villainIdNow
      runNow [VillainMessage v (VillainSchemesAgainst p)]
      choose "Tempus's interrupt" (isAbilityOfCard tempus)
      pass
      mainSchemeThreatNow `shouldReturn` before
      length . playerIdentityEncounterCards <$> playerAttrs p `shouldReturn` dealt + 1
      alliesWith "45042" `shouldReturn` []

  it "Tempus cannot be played by an identity without X-Men" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      card <- putInHand p "45042"
      _ <- putInHand p "01088"
      offers (isPlay card) `shouldReturn` False

  it "Blood Rage exhausts and costs one damage to draw after a basic attack defeats an enemy" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      asMagik p
      rage <- putIntoPlay p "45043"
      enemy <- addMinion p Minions.hydraRegular
      choose "make a basic attack" (isHeroAttack p)
      choose "defeat the minion" (isTargetChoice enemy)
      choose "Blood Rage" (isAbilityOfCard rage)
      pass
      damageOf p `shouldReturn` 1
      length <$> handOf p `shouldReturn` 1
      upgradeIsExhausted (upgradeIdOf rage) `shouldReturn` True

  it "Blood Rage does not answer defeats by attack events" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      asMagik p
      rage <- putIntoPlay p "45043"
      enemy <- addMinion p Minions.hydraRegular
      _ <- paying p "01087" ["01088"]
      choose "defeat with an event" (isTargetChoice enemy)
      offers (isAbilityOfCard rage) `shouldReturn` False
      damageOf p `shouldReturn` 0

  it "Test the Defense counts attack events and discharges at five counters" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      asMagik p
      upgrade <- putIntoPlay p "45044"
      resolve [UpgradeMessage (upgradeIdOf upgrade) (AddUpgradeUses 4)]
      _ <- paying p "01087" ["01088"]
      chooseVillain
      choose "fifth test counter" (isAbilityOfCard upgrade)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 8
      upgradesWith "45044" `shouldReturn` []

  it "Test the Defense does not answer a non-attack event" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      asMagik p
      upgrade <- putIntoPlay p "45044"
      _ <- paying p "12033" []
      offers (isAbilityOfCard upgrade) `shouldReturn` False
      upgradeUses <$> (testHead <$> upgradesWith "45044") `shouldReturn` 0

  for_ [(5, 0), (6, 6)] \(damage, overkill) ->
    it ("Full-Body Charge at " <> show damage <> " damage uses the strict half-HP threshold") do
      tg <- testGame ["magik"]
      inGame tg do
        let p = player1 tg
        asMagik p
        setDamage p damage
        enemy <- addMinion p Minions.hydraRegular
        _ <- paying p "45045" ["01088", "01089"]
        choose "charge the minion" (isTargetChoice enemy)
        pass
        villainDamageNow `shouldReturn` overkill

  it "Triage heals an X-Men identity after entering play" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      asMagik p
      setDamage p 3
      triage <- putInHand p "45048"
      payment <- putInHand p "01088"
      playWith triage [payment]
      choose "Triage's response" (isAbilityOfCard triage)
      choose "heal Magik" (isTargetChoice p)
      damageOf p `shouldReturn` 1

  it "Triage can heal a damaged X-Men ally but not a non-X-Men character" do
    tg <- testGame ["magik", "spider-man"]
    inGame tg do
      let p = player1 tg
          other = player2 tg
      asMagik p
      colossus <- putIntoPlay p "45031"
      resolve [RemoveStatus (AllyRef $ allyIdOf colossus) ToughStatus, AllyMessage (allyIdOf colossus) (AllyDamaged GameRef $ toDamage 2 FromAbility)]
      setDamage other 3
      triage <- putInHand p "45048"
      payment <- putInHand p "01088"
      playWith triage [payment]
      choose "Triage" (isAbilityOfCard triage)
      offers (isTargetChoice other) `shouldReturn` False
      choose "heal Colossus" (isTargetChoice $ allyIdOf colossus)
      allyDamage <$> allyIn "45031" `shouldReturn` 0

  it "Stepford Cuckoos cancels a treachery, spends a counter, and reveals a replacement" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      asMagik p
      cuckoos <- putIntoPlay p "45049"
      _ <- stackEncounterDeck [Treacheries.assault]
      before <- mainSchemeThreatNow
      canceled <- revealNow p Treacheries.advance
      choose "cancel the treachery" (isAbilityOfCard cuckoos)
      pass
      mainSchemeThreatNow `shouldReturn` before
      [support] <- supportsWith "45049"
      supportUses support `shouldBe` 2
      supportExhausted support `shouldBe` True
      discard <- getEncounterDiscard
      discard `shouldSatisfy` elem canceled

  it "Bloodgem exhausts and takes two damage to generate a wild resource" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      asMagik p
      gem <- putIntoPlay p "45050"
      attack <- putInHand p "01087"
      payment <- putInHand p "01086"
      choose "play Haymaker" (isPlay attack)
      choose "Bloodgem resource" (isAbilityOfCard gem)
      payWith [payment]
      chooseVillain
      pass
      damageOf p `shouldReturn` 2
      upgradeIsExhausted (upgradeIdOf gem) `shouldReturn` True
      villainDamageNow `shouldReturn` 3

  for_ ["45050", "45051"] \code ->
    it ("requires the Mystic trait for " <> show code) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        changeForm p
        card <- putInHand p code
        _ <- putInHand p "01088"
        offers (isPlay card) `shouldReturn` False

  for_ [("$label.magikBasicSpellHeal", 0), ("$label.magikBasicSpellThwart", 1), ("$label.magikBasicSpellDamage", 2)] \(mode, effect) ->
    it ("Basic Spell resolves its chosen mode " <> show mode) do
      tg <- testGame ["magik"]
      inGame tg do
        let p = player1 tg
        asMagik p
        setDamage p 4
        placeMainThreat 5
        before <- mainSchemeThreatNow
        _ <- paying p "45051" ["01088"]
        choose "spell mode" (isLabel mode)
        case effect :: Int of
          0 -> do
            choose "heal identity" (isTargetChoice p)
            damageOf p `shouldReturn` 1
          1 -> do
            chooseMainScheme
            mainSchemeThreatNow `shouldReturn` before - 3
          _ -> do
            chooseVillain
            villainDamageNow `shouldReturn` 3
