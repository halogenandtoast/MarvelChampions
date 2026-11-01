module Marvel.Rules.PrintedXSpec (spec) where

import Marvel.Cost.Types
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Matchers.Types
import Marvel.Modifier
import Marvel.Payment.Types
import TestImport

newtype CostModifiers = CostModifiers (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON)
instance RunMessage CostModifiers where
  runMessage msg (CostModifiers a) = CostModifiers <$> runMessage msg a

costModifiers :: IdentityId -> [Modifier] -> GameAppT ()
costModifiers p ms = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (CostModifiers EffectAttrs
    { effectId = eid, effectCardCode = "01039", effectSource = GameRef
    , effectMatcher = IdentityEntity (IdentityWithId p)
    , effectModifiers = ms, effectEnds = Nothing })

xAttack :: IdentityId -> GameAppT PlayerCard
xAttack p = do
  original <- putInHand p "01005"
  let card = original {pcCardDef = (getCardDef original)
        { cdCost = Just (-1), cdAbilityCost = PrintedXCost 1 (UpgradeControlledBy $ IdentityWithId p) }}
  withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
    PlayerIdentity a {playerIdentityHand = Hand (card : filter (/= original) (unHand a.playerIdentityHand))}
  pure card

chooseX :: Natural -> GameAppT ()
chooseX n = choose "declare X" (isLabel $ countLabel "chooseXAmount" n)

spec :: Spec
spec = describe "printed X resource costs" do
  it "chooses X before paying and does not cap affordable X by effect targets" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      card <- xAttack p
      _ <- putInHand p "01088"
      _ <- putInHand p "01089"
      choose "play" (isPlay card)
      offers (isLabel $ countLabel "chooseXAmount" 4) `shouldReturn` True
      offers (isLabel $ countLabel "chooseXAmount" 0) `shouldReturn` False
      chooseX 3
      paymentX <$> getCurrentPayment `shouldReturn` 3

  it "retains declared X after a cost reduction" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      card <- xAttack p
      energy <- putInHand p "01088"
      costModifiers p [ResourceCostReduction 1]
      choose "play" (isPlay card)
      chooseX 3
      paymentX <$> getCurrentPayment `shouldReturn` 3
      choose "pay the reduced two-resource cost" (isPayWith energy)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 8

  it "adds additional costs after X is chosen" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      card <- xAttack p
      energy <- putInHand p "01088"
      genius <- putInHand p "01089"
      costModifiers p [AdditionalResourceCost AnyCard 2]
      choose "play" (isPlay card)
      offers (isLabel $ countLabel "chooseXAmount" 3) `shouldReturn` False
      chooseX 2
      paymentX <$> getCurrentPayment `shouldReturn` 2
      choose "pay first two" (isPayWith energy)
      offers isFinishPayment `shouldReturn` False
      choose "pay remaining two" (isPayWith genius)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 8

  it "offers a meaningful positive X when ignoring its resource cost" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      _ <- putIntoPlay p "01007"
      _ <- putIntoPlay p "01007"
      clearHand p
      card <- xAttack p
      costModifiers p [IgnoresResourceCost]
      choose "play ignoring cost" (isPlay card)
      offers (isLabel $ countLabel "chooseXAmount" 2) `shouldReturn` True
      offers (isLabel $ countLabel "chooseXAmount" 3) `shouldReturn` False
      chooseX 2
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 8
