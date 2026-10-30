module Marvel.Rules.DashPowerSpec (spec) where

import TestImport

spec :: Spec
spec = describe "Dash powers versus zero powers" do
  it "does not let Hulk use his printed dash thwart, including while confused" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      pass
      _ <- putIntoPlay p "01050"
      placeMainThreat 3
      hulk <- allyIn "01050"
      offers (isAllyThwart hulk) `shouldReturn` False
      resolve [GiveStatus (AllyRef $ allyId hulk) ConfusedStatus]
      offers (isAllyThwart hulk) `shouldReturn` False
