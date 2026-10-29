module Marvel.Aspect.VisionMeditationSpec (spec) where

import Marvel.Identity (getModifiedCost)
import TestImport

spec :: Spec
spec = describe "Vision Meditation" do
  it "exhausts the alter ego and plays a three-cost card for no resources" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      clearHand p
      meditation <- putInHand p "26036"
      ally <- putInHand p "01083"
      playWith meditation []
      playerIdentityExhausted . toAttrs <$> getPlayer p `shouldReturn` True
      choose "play Mockingbird at a three-resource reduction" (isTargetChoice ally)
      pass
      alliesWith "01083" >>= (`shouldSatisfy` (not . null))

  it "does not offer Meditation when its discount is still insufficient" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      clearHand p
      meditation <- putInHand p "26036"
      original <- putInHand p "01084"
      let expensive = original {pcCardDef = (getCardDef original) {cdCost = Just 8}}
      withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
        PlayerIdentity a {playerIdentityHand = Hand [meditation, expensive]}
      offers (isPlay meditation) `shouldReturn` False

  it "does not count Meditation or the chosen card as resources for that play" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      clearHand p
      meditation <- putInHand p "26036"
      _ <- putInHand p "01084"
      offers (isPlay meditation) `shouldReturn` False

  it "checks target legality after exhausting and excludes itself as a target" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      clearHand p
      meditation <- putInHand p "26036"
      _ <- putInHand p "26036"
      offers (isPlay meditation) `shouldReturn` False
      playerIdentityExhausted . toAttrs <$> getPlayer p `shouldReturn` False

  it "respects uniqueness and cannot be played in hero form" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      _ <- putIntoPlay p "01083"
      clearHand p
      meditation <- putInHand p "26036"
      _ <- putInHand p "01083"
      offers (isPlay meditation) `shouldReturn` False
      _ <- putInHand p "01092"
      changeForm p
      offers (isPlay meditation) `shouldReturn` False

  it "requires and pays a target's remaining resource cost normally" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      clearHand p
      meditation <- putInHand p "26036"
      ally <- putInHand p "01084"
      energy <- putInHand p "01088"
      playWith meditation []
      choose "play Nick Fury" (isTargetChoice ally)
      offers isFinishPayment `shouldReturn` False
      choose "pay the remaining one-resource cost" (isPayWith energy)
      pass
      alliesWith "01084" >>= (`shouldSatisfy` (not . null))

  it "does not leak its reduction into later cards" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      clearHand p
      meditation <- putInHand p "26036"
      ally <- putInHand p "01083"
      carrier <- putInHand p "01092"
      playWith meditation []
      choose "play Mockingbird" (isTargetChoice ally)
      pass
      player <- getPlayer p
      getModifiedCost player carrier Nothing `shouldReturn` 3
      offers (isPlay carrier) `shouldReturn` False

  it "removes the exact-card discount when its nested payment is aborted" do
    tg <- testGame ["black-panther"]
    let p = player1 tg
    inGame tg do
      clearHand p
      meditation <- putInHand p "26036"
      ally <- putInHand p "01084"
      _ <- putInHand p "01088"
      playWith meditation []
      choose "begin paying for Nick Fury" (isTargetChoice ally)
      Just acid <- getActiveCost
      runNow [AbortPayment acid]
      player <- getPlayer p
      getModifiedCost player ally Nothing `shouldReturn` 4
      handOf p >>= (`shouldSatisfy` elem ally)
