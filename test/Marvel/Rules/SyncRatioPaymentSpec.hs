module Marvel.Rules.SyncRatioPaymentSpec (spec) where

import Data.UUID qualified as UUID
import Marvel.Payment.Types
import TestImport
import Test.Hspec qualified as Hspec

spec :: Spec
spec = describe "Sync Ratio payment provenance" do
  it "distinguishes typed resources spent from differently typed surplus generation" do
    let payment = ResourcePayment Mental <> ResourcePayment Mental
          <> SyncRatioPayment 1 <> ResourcePayment Physical
    syncRatioResources payment `Hspec.shouldBe` 1
    syncRatioSpendOptions [Mental, Mental] payment `Hspec.shouldBe` [0]
    syncRatioSpendOptions [Mental, Physical] payment `Hspec.shouldBe` [1]

  it "offers both origins when ordinary and Sync Ratio resources of the same type are interchangeable" do
    let payment = ResourcePayment Energy <> ResourcePayment Energy
          <> SyncRatioPayment 1 <> ResourcePayment Energy
    syncRatioSpendOptions [Energy, Energy] payment `Hspec.shouldBe` [0, 1]

  it "combines independent per-type origin choices into a contiguous feasible range" do
    let payment = SyncRatioPayment 2 <> ResourcePayment Physical <> ResourcePayment Physical
          <> ResourcePayment Physical <> SyncRatioPayment 1 <> ResourcePayment Mental
          <> ResourcePayment Mental
    syncRatioSpendOptions [Physical, Physical, Mental] payment `Hspec.shouldBe` [1, 2, 3]
    syncRatioSpendOptions [Physical, Physical, Energy] payment `Hspec.shouldBe` []

  it "ignores non-resource metadata between a marker and its resource receipts" do
    let payment = SyncRatioPayment 1 <> PrintedResourcesPayment [Mental, Mental]
          <> ExhaustPayment <> WildPayment Energy <> ResourcePayment Mental
          <> ActualResourcesPayment [Energy] <> SyncRatioSpentPayment 1
    generatedResourcesWithSyncRatio payment `Hspec.shouldBe` [(Energy, True), (Mental, False)]
    syncRatioResourcesPaid payment `Hspec.shouldBe` 1
    syncRatioResources payment `Hspec.shouldBe` 1

  it "preserves origin as contributed wild resources are attributed and declared" do
    let contributor = IdentityId UUID.nil
        raw = ResourcePayment Mental <> SyncRatioPayment 1 <> WildPayment Wild
        attributed = attributeWilds 0 contributor raw
        declared = declareWild 0 Energy attributed
    generatedResourcesWithSyncRatio declared `Hspec.shouldBe` [(Mental, False), (Energy, True)]
    syncRatioSpendOptions [Mental] declared `Hspec.shouldBe` [0]
    syncRatioSpendOptions [Energy] declared `Hspec.shouldBe` [1]

  it "defaults to no Sync Ratio resources paid and does not infer spending from generation" do
    let ordinary = ResourcePayment Energy <> ActualResourcesPayment [Energy]
        generated = ordinary <> SyncRatioPayment 1 <> WildPayment Wild
    syncRatioResourcesPaid ordinary `Hspec.shouldBe` 0
    syncRatioResourcesPaid generated `Hspec.shouldBe` 0
    syncRatioSpendOptions [Energy] ordinary `Hspec.shouldBe` [0]
    syncRatioSpendOptions [] generated `Hspec.shouldBe` [0]

  it "keeps every doubled wild with its original Sync Ratio generation" do
    let doubled = ResourcePayment Wild <> ResourcePayment Wild
        payment = normalizeSyncRatioExpansions
          [(SyncRatioPayment 1, SyncRatioPayment 1), (ResourcePayment Wild, doubled), (ResourcePayment Energy, ResourcePayment Energy)]
    syncRatioResources payment `Hspec.shouldBe` 2
    generatedResourcesWithSyncRatio payment `Hspec.shouldBe` [(Wild, True), (Wild, True), (Energy, False)]
    syncRatioSpendOptions [Wild, Wild] payment `Hspec.shouldBe` [2]
