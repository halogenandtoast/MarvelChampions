module Marvel.Rules.DeadpoolAspectIntegrationSpec (spec) where

import TestImport

agility :: Choice -> Bool
agility = isNamedAbility "\"Superhuman Agility\""

spec :: Spec
spec = describe "Deadpool fifth aspect integration" do
  it "gives Spider-Woman an independent agility trigger for a Pool card" do
    tg <- testGame ["spider-woman"]
    inGame tg do
      let you = player1 tg
      changeForm you
      pass
      clearHand you
      cutupper <- putInHand you "44018"
      payment <- traverse (putInHand you) ["01088", "01090"]
      playWith cutupper payment
      offers agility `shouldReturn` True
      choose "Superhuman Agility for Pool" agility
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 5
      choose "basic attack with increased ATK" (isHeroAttack you)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 7

  it "lets Finesse generate a resource for a Pool card" do
    tg <- testGame ["spider-woman"]
    inGame tg do
      let you = player1 tg
      changeForm you
      pass
      clearHand you
      finesse <- putIntoPlay you "04033"
      healing <- putInHand you "44029"
      energy <- putInHand you "01088"
      choose "play Healing Factor" (isPlay healing)
      offers (isAbilityOfCard finesse) `shouldReturn` True
      choose "generate a wild resource" (isAbilityOfCard finesse)
      payWith [energy]
      pass
      codes <- map toCardCode <$> handOf you
      ("44029" `elem` codes) `shouldBe` False

  it "lets Jessica Drew's Apartment find a Pool card" do
    tg <- testGame ["spider-woman"]
    inGame tg do
      let you = player1 tg
      clearHand you
      apartment <- putIntoPlay you "04034"
      cards <- stackDeck you ["44018", "01088", "01089", "01090", "01088"]
      choose "search with the Apartment" (isAbilityOfCard apartment)
      choose "take the Pool card" (isTargetChoice $ fromMaybe (error "no stacked Pool card") $ headMay cards)
      pass
      codes <- map toCardCode <$> handOf you
      ("44018" `elem` codes) `shouldBe` True
