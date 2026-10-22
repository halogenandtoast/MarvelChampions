module Marvel.Rules.RecoverSpec (spec) where

import TestImport

isRecover :: Choice -> Bool
isRecover = \case
  UseAbility ab -> Recover `elem` abilityChoices ab
  _ -> False

spec :: Spec
spec = describe "recovering" do
  it "is not offered to an alter-ego with no damage to heal" do
    tg <- testGame ["spider-man"]
    inGame tg do
      offers isRecover `shouldReturn` False

  it "heals the alter-ego's REC" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      setDamage p 5
      choose "recover" isRecover
      damageOf p `shouldReturn` 2
