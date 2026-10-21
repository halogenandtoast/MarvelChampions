module Marvel.Hero.SpiderManSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import TestImport

spec :: Spec
spec = describe "Spider-Man" do
  it "Swinging Web Kick deals 8 damage to an enemy" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearHand p
      kick <- putInHand p "01005"
      strength <- putInHand p "01090"
      energy <- putInHand p "01088"
      changeForm p
      playWith kick [strength, energy]
      villain <- getVillain
      choose "kick Rhino" (isTargetChoice (toId villain))
      villainDamageNow `shouldReturn` 8

  it "Peter Parker generates a mental resource once a round" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearHand p
      shooter <- putInHand p "01008"
      choose "play Web-Shooter" (isPlay shooter)
      choose "pay with Scientist" (isAbilityOf p)
      upgrades <- getsGame (map getCardDef . HashMap.elems . gameUpgrades)
      map toCardCode upgrades `shouldBe` ["01008"]
      -- once a round: another card cannot be paid for with it
      kick <- putInHand p "01005"
      offers (isPlay kick) `shouldReturn` False

  it "Spider-Sense draws a card when the villain attacks him" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      endTurn
      -- the end of the player phase draws up to hand size first
      drawnUp <- length <$> handOf p
      choose "use Spider-Sense" (isAbilityOf p)
      after <- length <$> handOf p
      after `shouldBe` drawnUp + 1

  it "Aunt May heals Peter Parker 4" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearHand p
      aunt <- putInHand p "01006"
      energy <- putInHand p "01088"
      playWith aunt [energy]
      withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) -> PlayerIdentity a {playerIdentityDamage = 6}
      choose "use Aunt May" \case
        UseAbility ab -> abilitySource ab /= toSource p
        _ -> False
      damageOf p `shouldReturn` 2
