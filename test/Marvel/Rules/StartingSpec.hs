module Marvel.Rules.StartingSpec (spec) where

import TestImport

spec :: Spec
spec = describe "Starting cards and hand size" do
  it "counts a Starting card already held toward hand size" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      clearHand p
      starting <- putInHand p "61034"
      _ <- stackDeck p ["01089", "01088", "01090", "01089", "01088", "01090"]
      resolve [IdentityMessage p DrawOrDiscardToHandLimit]
      hand <- handOf p
      length hand `shouldBe` 6
      hand `shouldSatisfy` elem starting

  it "counts a Starting card drawn while filling the hand" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      clearHand p
      _ <- stackDeck p ["61034", "01089", "01088", "01090", "01089", "01088", "01090"]
      resolve [IdentityMessage p DrawToHandLimit]
      hand <- handOf p
      length hand `shouldBe` 6
      hand `shouldSatisfy` any ((== "61034") . toCardCode)
