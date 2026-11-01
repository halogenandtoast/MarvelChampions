module Marvel.Rules.PrintedCostSpec (spec) where

import Marvel.ActiveCost.Types (ActiveCost (..), ActiveCostTarget (ForContinuation))
import Marvel.Cost (Cost (PrintedResourceCost), canAffordCost)
import TestImport

spec :: Spec
spec = describe "printed resource costs" do
  it "cannot replace printed energy or mental icons with printed wild icons" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
        cost = PrintedResourceCost [Energy, Mental, Physical]
    inGame tg do
      clearHand p
      _ <- putInHand p "27008"
      _ <- putInHand p "27024"
      canAffordCost p GameRef cost `shouldReturn` False
      _ <- putInHand p "01089"
      canAffordCost p GameRef cost `shouldReturn` False
      _ <- putInHand p "01088"
      canAffordCost p GameRef cost `shouldReturn` True

  it "does not count an alter-ego's resource ability as a printed mental icon" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearHand p
      _ <- putInHand p "01088"
      _ <- putInHand p "01090"
      canAffordCost p GameRef (PrintedResourceCost [Energy, Mental, Physical]) `shouldReturn` False

  it "pays with native printed icons, wastes surplus generation, then resumes the real continuation" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearHand p
      e <- putInHand p "01088"
      m <- putInHand p "01089"
      f <- putInHand p "01090"
      resolve [IdentityMessage p ExhaustedIdentity]
      acid <- getRandom
      runNow [SetActiveCost $ ActiveCost acid p (ForContinuation GameRef [IdentityMessage p ReadiedIdentity])
        (PrintedResourceCost [Energy, Mental, Physical]) NoPayment Nothing [] Nothing]
      payWith [e, m, f]
      exhaustedNow p `shouldReturn` False
      discardOf p >>= (`shouldSatisfy` (\cards -> all (`elem` cards) [e, m, f]))
