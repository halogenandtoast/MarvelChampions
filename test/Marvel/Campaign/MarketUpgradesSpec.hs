-- | The Market's upgrades (The Galaxy's Most Wanted), the Milano Mods: each played as
-- a player would, with and without the Milano under their control.
module Marvel.Campaign.MarketUpgradesSpec (spec) where

import Marvel.Damage
import Marvel.EncounterSet (EncounterSet (ShipCommand))
import Marvel.Identity (getModifiedCost)
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as SideSchemes
import TestImport

-- | Spider-Man against Rhino with Ship Command: the Milano in play under his control.
withMilano :: IO TestGame
withMilano = do
  (app, ran, logged) <- dealtGameLogged (ScenarioSetup "01094" [ShipCommand] Normal) [starter "spider-man"] 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  inGame tg toFirstTurn
  pure tg

-- | Spider-Man against Rhino, no Milano in play.
withoutMilano :: IO TestGame
withoutMilano = testGame ["spider-man"]

-- | The player in hero form, their hand empty.
asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

-- | The player in hero form with the Milano mod in play, the Milano as the game has it.
heroWith :: TestGame -> CardCode -> GameAppT PlayerCard
heroWith tg code = do
  let p = player1 tg
  asHero p
  putIntoPlay p code

costOf :: IdentityId -> PlayerCard -> GameAppT Int
costOf p card = do
  pl <- getPlayer p
  getModifiedCost pl card Nothing

spec :: Spec
spec = describe "The Market upgrades" do
  it "the test tables: Ship Command gives the first player the Milano, Rhino alone does not" do
    tg <- withMilano
    tg' <- withoutMilano
    inGame tg (controlsMilano (player1 tg)) `shouldReturn` True
    inGame tg' (controlsMilano (player1 tg')) `shouldReturn` False

  armorPlating
  heavyCannon
  hyperThrusters
  reactorCore
  cargoHold
  mountedLaser
  navigationColumn
  targetingScreen

armorPlating :: Spec
armorPlating = describe "Armor Plating (16162)" do
  it "exhausts to prevent 1 of the damage an identity would take" do
    tg <- withoutMilano
    let p = player1 tg
    inGame tg do
      plating <- heroWith tg "16162"
      runNow (damageIdentity p GameRef (toDamage 4 FromAbility))
      choose "Armor Plating" (isAbilityOfCard plating)
      pass
      damageOf p `shouldReturn` 3
      upgradeIsExhausted (upgradeIdOf plating) `shouldReturn` True

  it "prevents 2 instead when you control the Milano" do
    tg <- withMilano
    let p = player1 tg
    inGame tg do
      plating <- heroWith tg "16162"
      runNow (damageIdentity p GameRef (toDamage 4 FromAbility))
      choose "Armor Plating" (isAbilityOfCard plating)
      pass
      damageOf p `shouldReturn` 2

  it "answers damage to another player's identity" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let p1 = player1 tg
        p2 = player2 tg
    inGame tg do
      plating <- heroWith tg "16162"
      runNow (damageIdentity p2 GameRef (toDamage 4 FromAbility))
      choose "Armor Plating" (isAbilityOfCard plating)
      pass
      damageOf p2 `shouldReturn` 3
      damageOf p1 `shouldReturn` 0

  it "is a hero interrupt: not offered in alter-ego form, nor once exhausted" do
    tg <- withoutMilano
    let p = player1 tg
    inGame tg do
      plating <- putIntoPlay p "16162"
      runNow (damageIdentity p GameRef (toDamage 2 FromAbility))
      offers (isAbilityOfCard plating) `shouldReturn` False
      pass
      changeForm p
      pass
      runNow [UpgradeMessage (upgradeIdOf plating) ExhaustedUpgrade]
      runNow (damageIdentity p GameRef (toDamage 2 FromAbility))
      offers (isAbilityOfCard plating) `shouldReturn` False

heavyCannon :: Spec
heavyCannon = describe "Heavy Cannon (16163)" do
  it "exhausts to deal 1 damage to each enemy, not as an attack" do
    tg <- withoutMilano
    let p = player1 tg
    inGame tg do
      cannon <- heroWith tg "16163"
      m <- addMinion p Minions.hydraMercenary
      choose "Heavy Cannon" (isAbilityOfCard cannon)
      villainDamageNow `shouldReturn` 1
      minionDamageOf m `shouldReturn` Just 1
      upgradeIsExhausted (upgradeIdOf cannon) `shouldReturn` True
      offers (isAbilityOfCard cannon) `shouldReturn` False

  it "deals 1 additional damage to the villain when you control the Milano" do
    tg <- withMilano
    let p = player1 tg
    inGame tg do
      cannon <- heroWith tg "16163"
      m <- addMinion p Minions.hydraMercenary
      choose "Heavy Cannon" (isAbilityOfCard cannon)
      villainDamageNow `shouldReturn` 2
      minionDamageOf m `shouldReturn` Just 1

  it "is a hero action: not offered in alter-ego form" do
    tg <- withoutMilano
    let p = player1 tg
    inGame tg do
      cannon <- putIntoPlay p "16163"
      offers (isAbilityOfCard cannon) `shouldReturn` False

hyperThrusters :: Spec
hyperThrusters = describe "Hyper Thrusters (16164)" do
  it "exhausts to remove 1 threat from each scheme" do
    tg <- withoutMilano
    let p = player1 tg
    inGame tg do
      thrusters <- heroWith tg "16164"
      placeMainThreat 3
      side <- sideSchemeOf <$> revealNow p SideSchemes.breakinAndTakin
      main <- mainSchemeThreatNow
      sideBefore <- sideSchemeThreat side
      choose "Hyper Thrusters" (isAbilityOfCard thrusters)
      mainSchemeThreatNow `shouldReturn` (main - 1)
      sideSchemeThreat side `shouldReturn` (sideBefore - 1)
      upgradeIsExhausted (upgradeIdOf thrusters) `shouldReturn` True

  it "removes 1 additional threat from the main scheme when you control the Milano" do
    tg <- withMilano
    let p = player1 tg
    inGame tg do
      thrusters <- heroWith tg "16164"
      placeMainThreat 3
      side <- sideSchemeOf <$> revealNow p SideSchemes.breakinAndTakin
      main <- mainSchemeThreatNow
      sideBefore <- sideSchemeThreat side
      choose "Hyper Thrusters" (isAbilityOfCard thrusters)
      mainSchemeThreatNow `shouldReturn` (main - 2)
      sideSchemeThreat side `shouldReturn` (sideBefore - 1)

  it "is not a thwart: patrol does not keep it off the main scheme" do
    tg <- withoutMilano
    let p = player1 tg
    inGame tg do
      thrusters <- heroWith tg "16164"
      placeMainThreat 3
      _ <- addMinion p Minions.headhuntersHenchman
      main <- mainSchemeThreatNow
      choose "Hyper Thrusters" (isAbilityOfCard thrusters)
      mainSchemeThreatNow `shouldReturn` (main - 1)

reactorCore :: Spec
reactorCore = describe "Reactor Core (16165)" do
  it "exhausts and discards the top 2 cards of the deck to make the next event this turn cost 1 less" do
    tg <- withoutMilano
    let p = player1 tg
    inGame tg do
      core <- heroWith tg "16165"
      kick <- putInHand p "01005"
      other <- putInHand p "01005"
      shooter <- putInHand p "01008"
      costOf p kick `shouldReturn` 3
      discarded <- length <$> discardOf p
      choose "Reactor Core" (isAbilityOfCard core)
      (subtract discarded . length <$> discardOf p) `shouldReturn` 2
      upgradeIsExhausted (upgradeIdOf core) `shouldReturn` True
      costOf p kick `shouldReturn` 2
      -- events only
      costOf p shooter `shouldReturn` 1
      -- Strength pays 2: the reduced Swinging Web Kick
      strength <- putInHand p "01090"
      playWith kick [strength]
      villain <- getVillain
      choose "kick Rhino" (isTargetChoice (toId villain))
      villainDamageNow `shouldReturn` 8
      costOf p other `shouldReturn` 3

  it "discards only the top card when you control the Milano" do
    tg <- withMilano
    let p = player1 tg
    inGame tg do
      core <- heroWith tg "16165"
      kick <- putInHand p "01005"
      discarded <- length <$> discardOf p
      choose "Reactor Core" (isAbilityOfCard core)
      (subtract discarded . length <$> discardOf p) `shouldReturn` 1
      costOf p kick `shouldReturn` 2

  it "ends at the end of the turn" do
    tg <- withoutMilano
    let p = player1 tg
    inGame tg do
      core <- heroWith tg "16165"
      kick <- putInHand p "01005"
      choose "Reactor Core" (isAbilityOfCard core)
      costOf p kick `shouldReturn` 2
      endTurn
      costOf p kick `shouldReturn` 3

  it "is a hero action: not offered in alter-ego form" do
    tg <- withoutMilano
    let p = player1 tg
    inGame tg do
      core <- putIntoPlay p "16165"
      offers (isAbilityOfCard core) `shouldReturn` False

cargoHold :: Spec
cargoHold = describe "Cargo Hold (16170)" do
  it "exhausts to heal 1 damage from a friendly character" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let p1 = player1 tg
        p2 = player2 tg
    inGame tg do
      hold <- heroWith tg "16170"
      setDamage p1 2
      setDamage p2 2
      choose "Cargo Hold" (isAbilityOfCard hold)
      choose "heal Captain Marvel" (isTargetChoice p2)
      damageOf p2 `shouldReturn` 1
      damageOf p1 `shouldReturn` 2
      upgradeIsExhausted (upgradeIdOf hold) `shouldReturn` True

  it "heals 1 damage from your identity too when you control the Milano" do
    tg <- withMilano
    let p = player1 tg
    inGame tg do
      hold <- heroWith tg "16170"
      setDamage p 3
      choose "Cargo Hold" (isAbilityOfCard hold)
      -- the only damaged friendly character: the one choice is taken without asking
      damageOf p `shouldReturn` 1

  it "needs a damaged friendly character" do
    tg <- withMilano
    let p = player1 tg
    inGame tg do
      hold <- heroWith tg "16170"
      offers (isAbilityOfCard hold) `shouldReturn` False
      setDamage p 1
      offers (isAbilityOfCard hold) `shouldReturn` True

mountedLaser :: Spec
mountedLaser = describe "Mounted Laser (16171)" do
  it "exhausts to deal 2 damage to an enemy" do
    tg <- withoutMilano
    let p = player1 tg
    inGame tg do
      laser <- heroWith tg "16171"
      m <- addMinion p Minions.hydraMercenary
      vid <- villainIdNow
      choose "Mounted Laser" (isAbilityOfCard laser)
      offers (isTargetChoice m) `shouldReturn` True
      choose "fire at Rhino" (isTargetChoice vid)
      villainDamageNow `shouldReturn` 2
      upgradeIsExhausted (upgradeIdOf laser) `shouldReturn` True

  it "deals 3 instead when you control the Milano" do
    tg <- withMilano
    inGame tg do
      laser <- heroWith tg "16171"
      vid <- villainIdNow
      choose "Mounted Laser" (isAbilityOfCard laser)
      choose "fire at Rhino" (isTargetChoice vid)
      villainDamageNow `shouldReturn` 3

  it "is a hero action: not offered in alter-ego form" do
    tg <- withoutMilano
    let p = player1 tg
    inGame tg do
      laser <- putIntoPlay p "16171"
      offers (isAbilityOfCard laser) `shouldReturn` False

navigationColumn :: Spec
navigationColumn = describe "Navigation Column (16172)" do
  it "exhausts and discards a chosen card from hand to draw 1" do
    tg <- withoutMilano
    let p = player1 tg
    inGame tg do
      column <- heroWith tg "16172"
      junk <- putInHand p "01088"
      [top] <- stackDeck p ["01089"]
      choose "Navigation Column" (isAbilityOfCard column)
      choose "discard" (isTargetChoice junk)
      discardOf p >>= (`shouldSatisfy` elem junk)
      handOf p `shouldReturn` [top]
      upgradeIsExhausted (upgradeIdOf column) `shouldReturn` True

  it "needs a card in hand without the Milano" do
    tg <- withoutMilano
    inGame tg do
      column <- heroWith tg "16172"
      offers (isAbilityOfCard column) `shouldReturn` False

  it "discards the top card of the deck instead when you control the Milano, the hand kept" do
    tg <- withMilano
    let p = player1 tg
    inGame tg do
      column <- heroWith tg "16172"
      [top, next] <- stackDeck p ["01088", "01089"]
      choose "Navigation Column" (isAbilityOfCard column)
      discardOf p >>= (`shouldSatisfy` elem top)
      handOf p `shouldReturn` [next]

targetingScreen :: Spec
targetingScreen = describe "Targeting Screen (16173)" do
  it "exhausts to remove 2 threat from a scheme" do
    tg <- withoutMilano
    inGame tg do
      screen <- heroWith tg "16173"
      placeMainThreat 3
      main <- mainSchemeThreatNow
      m <- mainSchemeIdNow
      choose "Targeting Screen" (isAbilityOfCard screen)
      choose "the main scheme" (isTargetChoice m)
      mainSchemeThreatNow `shouldReturn` (main - 2)
      upgradeIsExhausted (upgradeIdOf screen) `shouldReturn` True

  it "removes 3 instead when you control the Milano" do
    tg <- withMilano
    inGame tg do
      screen <- heroWith tg "16173"
      placeMainThreat 3
      main <- mainSchemeThreatNow
      m <- mainSchemeIdNow
      choose "Targeting Screen" (isAbilityOfCard screen)
      choose "the main scheme" (isTargetChoice m)
      mainSchemeThreatNow `shouldReturn` (main - 3)

  it "is not a thwart: patrol does not keep it off the main scheme" do
    tg <- withoutMilano
    let p = player1 tg
    inGame tg do
      screen <- heroWith tg "16173"
      placeMainThreat 3
      _ <- addMinion p Minions.headhuntersHenchman
      main <- mainSchemeThreatNow
      m <- mainSchemeIdNow
      choose "Targeting Screen" (isAbilityOfCard screen)
      choose "the main scheme" (isTargetChoice m)
      mainSchemeThreatNow `shouldReturn` (main - 2)
