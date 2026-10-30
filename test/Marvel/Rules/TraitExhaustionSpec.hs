module Marvel.Rules.TraitExhaustionSpec (spec) where

import Marvel.ActiveCost.Types
import Marvel.Cost (canAffordCost, readyTraitCardCandidates)
import Marvel.Cost.Types
import Marvel.Payment.Types
import Marvel.Trait (Trait (Avenger, Shield, Tech))
import TestImport

spec :: Spec
spec = describe "exhausting controlled cards with a trait" do
  it "includes identities, allies, supports and upgrades using their own refs" do
    tg <- testGame ["captain-marvel", "spider-man"]
    let p = player1 tg
        other = player2 tg
    inGame tg do
      changeForm p
      ally <- putIntoPlay p "01068"
      support <- putIntoPlay p "01064"
      upgrade <- putIntoPlay p "01008"
      _ <- putIntoPlay other "01064"
      readyTraitCardCandidates p Avenger `shouldReturn` [IdentityRef p, AllyRef $ allyIdOf ally]
      readyTraitCardCandidates p Shield `shouldReturn` [SupportRef $ supportIdOf support]
      readyTraitCardCandidates p Tech `shouldReturn` [UpgradeRef $ upgradeIdOf upgrade]

  it "excludes exhausted cards and cards another player controls" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let p = player1 tg
        other = player2 tg
    inGame tg do
      own <- putIntoPlay p "01064"
      _ <- putIntoPlay other "01064"
      canAffordCost p GameRef (ExhaustCardsWithTraitCost Shield 1 1) `shouldReturn` True
      resolve [SupportMessage (supportIdOf own) ExhaustedSupport]
      readyTraitCardCandidates p Shield `shouldReturn` []
      canAffordCost p GameRef (ExhaustCardsWithTraitCost Shield 1 1) `shouldReturn` False
      canAffordCost other GameRef (ExhaustCardsWithTraitCost Shield 1 1) `shouldReturn` True

  it "allows zero optional exhaustions and rejects impossible bounds" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      canAffordCost p GameRef (ExhaustCardsWithTraitCost Shield 0 3) `shouldReturn` True
      canAffordCost p GameRef (ExhaustCardsWithTraitCost Shield 1 0) `shouldReturn` False
      _ <- putIntoPlay p "01064"
      canAffordCost p GameRef (ExhaustCardsWithTraitCost Shield 2 3) `shouldReturn` False

  it "requires distinct cards for repeated costs and the ability's own exhaustion" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      own <- putIntoPlay p "01064"
      let cost = ExhaustCardsWithTraitCost Shield 1 1
      canAffordCost p GameRef (cost <> cost) `shouldReturn` False
      canAffordCost p (SupportRef $ supportIdOf own) (ExhaustCost <> cost) `shouldReturn` False
      _ <- putIntoPlay p "01064"
      canAffordCost p GameRef (cost <> cost) `shouldReturn` True
      canAffordCost p (SupportRef $ supportIdOf own) (ExhaustCost <> cost) `shouldReturn` True

  it "lets the payer stop without exhausting a card" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      own <- putIntoPlay p "01064"
      acid <- getRandom
      let cost = ExhaustCardsWithTraitCost Shield 0 3
      withGame_ $ activeCostL .~ [ActiveCost acid p ForTreachery cost NoPayment Nothing [] Nothing]
      runNow [PayCost acid cost]
      choose "exhaust no cards" (isLabel "$label.done")
      paymentExhaustedCards <$> getCurrentPayment `shouldReturn` []
      readyTraitCardCandidates p Shield `shouldReturn` [SupportRef $ supportIdOf own]

  it "records selected cards once and stops when the upper bound is reached" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      first <- putIntoPlay p "01064"
      second <- putIntoPlay p "01064"
      third <- putIntoPlay p "01064"
      acid <- getRandom
      let cost = ExhaustCardsWithTraitCost Shield 1 2
      withGame_ $ activeCostL .~ [ActiveCost acid p ForTreachery cost NoPayment Nothing [] Nothing]
      runNow [PayCost acid cost]
      offers (isLabel "$label.done") `shouldReturn` False
      choose "exhaust the first support" (isTargetChoice $ supportIdOf first)
      offers (isTargetChoice $ supportIdOf first) `shouldReturn` False
      offers (isLabel "$label.done") `shouldReturn` True
      choose "exhaust the second support" (isTargetChoice $ supportIdOf second)
      paymentExhaustedCards <$> getCurrentPayment
        `shouldReturn` [SupportRef $ supportIdOf first, SupportRef $ supportIdOf second]
      readyTraitCardCandidates p Shield `shouldReturn` [SupportRef $ supportIdOf third]
      offers (isTargetChoice $ supportIdOf third) `shouldReturn` False
