module Marvel.Rules.ActualResourcesSpec (spec) where

import Marvel.ActiveCost (actualResourceAllocations, allocationLabel)
import Marvel.Ally.Cards qualified as Allies
import Marvel.Cost.Types
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Matchers.Types
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Payment
import TestImport
import Test.Hspec qualified as Hspec

newtype CostReduction = CostReduction (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON)
instance RunMessage CostReduction where
  runMessage msg (CostReduction a) = CostReduction <$> runMessage msg a

reduceEros :: IdentityId -> GameAppT ()
reduceEros p = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (CostReduction EffectAttrs
    { effectId = eid, effectCardCode = "01039", effectSource = GameRef
    , effectMatcher = IdentityEntity (IdentityWithId p)
    , effectModifiers = [ResourceCostReductionFor (CardIs Allies.eros) 1]
    , effectEnds = Nothing })

ownAbility :: PlayerCard -> Choice -> Bool
ownAbility card = \case
  UseAbility a -> abilitySource a == toRef (allyIdOf card) && abilityIndex a == 1
  _ -> False

confusedNow :: MinionId -> GameAppT Bool
confusedNow mid = getsGame (maybe False (minionConfused . toAttrs) . lookup mid . gameMinions)

spec :: Spec
spec = describe "actual resources spent" do
  it "limits homogeneous overgeneration to the final reduced cost, so Eros only confuses once" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      reduceEros p
      first <- addMinion p Minions.hydraRegular
      second <- addMinion p Minions.hydraRegular
      eros <- putInHand p "22011"
      genius <- putInHand p "01089"
      playWith eros [genius]
      choose "Eros response" (ownAbility eros)
      choose "confuse the first minion" (isTargetChoice first)
      pass
      confusedNow first `shouldReturn` True
      confusedNow second `shouldReturn` False
      discardOf p >>= (`shouldSatisfy` elem genius)

  for_ [([Mental, Mental], 2 :: Int), ([Physical, Mental], 1)] \(spent, expected) ->
    it ("lets Eros choose the satisfying actual icon allocation " <> show spent) do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        first <- addMinion p Minions.hydraRegular
        second <- addMinion p Minions.hydraRegular
        eros <- putInHand p "22011"
        physical <- putInHand p "01054"
        genius <- putInHand p "01089"
        choose "play Eros" (isPlay eros)
        choose "generate physical" (isPayWith physical)
        choose "generate two mental" (isPayWith genius)
        whenM (offers isFinishPayment) $ choose "finish generating" isFinishPayment
        choose "choose actual icons" (isLabel $ allocationLabel spent)
        choose "Eros response" (ownAbility eros)
        choose "confuse first" (isTargetChoice first)
        when (expected == 2) $ choose "confuse second" (isTargetChoice second)
        pass
        confusedNow first `shouldReturn` True
        confusedNow second `shouldReturn` (expected == 2)
        discardOf p >>= (`shouldSatisfy` (\cards -> physical `elem` cards && genius `elem` cards))

  it "preserves typed requirements while choosing which excess icons are wasted" do
    let cost = ResourceCost (Just Physical) <> ResourceCost Nothing
    actualResourceAllocations cost [Physical, Mental, Mental]
      `Hspec.shouldBe` [[Physical, Mental]]

  it "preserves different-resource requirements" do
    actualResourceAllocations (DifferentResourcesCost 2) [Physical, Physical, Mental]
      `Hspec.shouldBe` [[Physical, Mental]]

  it "does not truncate dynamic or explicit overpayment receipts" do
    let generated = [Physical, Mental, Mental]
    actualResourceAllocations (DynamicResourceCost Nothing) generated `Hspec.shouldBe` [generated]
    actualResourceAllocations (ResourceCost Nothing <> MayOverpay) generated `Hspec.shouldBe` [generated]

  it "uses an empty actual spend for a free cost while preserving raw payment provenance" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let payment = ResourcePayment Mental <> ResourcePayment Mental <> ActualResourcesPayment []
      paymentResources payment `shouldReturn` []
      paidWith Mental payment `shouldReturn` False
      payments payment `shouldSatisfy` elem (ResourcePayment Mental)
