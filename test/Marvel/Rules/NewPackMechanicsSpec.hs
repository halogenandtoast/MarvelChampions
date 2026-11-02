module Marvel.Rules.NewPackMechanicsSpec (spec) where

import Marvel.Damage
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Payment
import Marvel.ActiveCost (actualResourceAllocations)
import Marvel.Cost.Types
import Marvel.Villain.Types qualified as Villain
import Marvel.Modifier.Types
import Marvel.Status
import TestImport
import Test.Hspec qualified as Hspec

spec :: Spec
spec = describe "New hero pack engine mechanics" do
  it "unpreventable damage leaves tough in place" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      resolve [GiveStatus (IdentityRef p) ToughStatus]
      resolve $ liftedTestDamage p
      damageOf p `shouldReturn` 2
      (.playerIdentityTough) <$> playerAttrs p `shouldReturn` True
  it "damage prevention leaves an unpreventable queued instance intact" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      withGame_ $ questionL .~ mempty
      pushAll [IdentityMessage p $ IdentityDamaged GameRef $ withUnpreventable $ toDamage 2 FromAbility]
      Lifted.preventDamage p 5
      runGame
      damageOf p `shouldReturn` 2
  Hspec.it "tough cannot prevent unpreventable damage" do
    preventedByTough (withUnpreventable $ toDamage 2 FromAbility) `Hspec.shouldBe` False
  Hspec.it "unpreventable damage ignores enemy damage reduction and attack caps" do
    let damage = withUnpreventable $ toDamage 5 FromAttack
    damageAmount (Villain.reducedDamage [DamageReduction 3, AttackDamageReduction 2, AttackDamageCap 1] damage) `Hspec.shouldBe` 5
  Hspec.it "overpayment retains all deliberately generated resources" do
    actualResourceAllocations (ResourceCost Nothing <> MayOverpay) [Physical, Energy, Energy] `Hspec.shouldBe` [[Physical, Energy, Energy]]
  Hspec.it "overpaid icons are independent of base payment icons" do
    overpaidResources (ActualResourcesPayment [Physical, Energy] <> OverpaidResourcesPayment [Energy]) `Hspec.shouldBe` [Energy]

-- Resolve damage through its normal interrupt window.
liftedTestDamage :: IdentityId -> [Message]
liftedTestDamage p = [IdentityMessage p $ IdentityDamaged GameRef $ withUnpreventable $ toDamage 2 FromAbility]
