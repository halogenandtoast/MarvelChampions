module Marvel.Rules.AllianceSpec (spec) where

import Data.HashSet qualified as HashSet
import Marvel.Card.Side (Side (A))
import Marvel.Cost (canAffordAllianceCost)
import Marvel.Cost.Types
import Marvel.Keyword (Keyword (Alliance))
import Marvel.Trait (Trait (Avenger, Guardian))
import TestImport

-- A normal attack with alliance added isolates the keyword from any one pack.
allianceAttack :: IdentityId -> GameAppT PlayerCard
allianceAttack p = do
  original <- putInHand p "01005"
  let card = original {pcCardDef = (getCardDef original) {cdKeywords = HashSet.singleton Alliance}}
  withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
    PlayerIdentity a {playerIdentityHand = Hand (card : filter (/= original) (unHand a.playerIdentityHand))}
  pure card

spec :: Spec
spec = describe "Alliance" do
  it "another player chooses their own resources; the original player resolves the attack" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let p = player1 tg
        other = player2 tg
    inGame tg do
      changeForm p
      clearHand p
      clearHand other
      card <- allianceAttack p
      energy <- putInHand other "01088"
      genius <- putInHand other "01089"
      refreshTurn
      offers (isPlay card) `shouldReturn` True
      choose "play an alliance attack" (isPlay card)
      choose "invite Captain Marvel" (isTargetChoice other)
      fmap fst <$> asked `shouldReturn` Just other
      choose "contribute Energy" (isPayWith energy)
      fmap fst <$> asked `shouldReturn` Just p
      choose "invite her again" (isTargetChoice other)
      choose "contribute Genius" (isPayWith genius)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 8
      discardOf other >>= (`shouldSatisfy` (\cs -> energy `elem` cs && genius `elem` cs))
      discardOf p >>= (`shouldSatisfy` any ((== pcCardId card) . pcCardId))

  it "a contributor may pass without spending a card" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let p = player1 tg
        other = player2 tg
    inGame tg do
      changeForm p
      clearHand p
      clearHand other
      card <- allianceAttack p
      energy <- putInHand other "01088"
      _ <- putInHand other "01089"
      refreshTurn
      choose "play" (isPlay card)
      choose "invite" (isTargetChoice other)
      choose "pass" (isLabel "$label.doneContributing")
      fmap fst <$> asked `shouldReturn` Just p
      handOf other >>= (`shouldSatisfy` elem energy)
      offers (isTargetChoice other) `shouldReturn` True

  it "Peril prevents other players from contributing" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let p = player1 tg
        other = player2 tg
    inGame tg do
      changeForm p
      clearHand p
      clearHand other
      card <- allianceAttack p
      _ <- putInHand other "01088"
      _ <- putInHand other "01089"
      resolve [SetPeril (Just p)]
      refreshTurn
      offers (isPlay card) `shouldReturn` False
      allianceCostPayers p (Just card) `shouldReturn` [p]

  it "players contribute distinct characters to non-resource costs and choose their own exhaustion" do
    tg <- testGame ["spider-man", "captain-marvel", "rocket-raccoon"]
    let p = player1 tg
        avenger = player2 tg
        guardian = tg.seats !! 2
    inGame tg do
      resolve [IdentityMessage i (ChangedToForm A) | i <- [p, avenger, guardian]]
      resolve [IdentityMessage p ExhaustedIdentity]
      _ <- putIntoPlay avenger "01068"
      clearHand p
      original <- allianceAttack p
      let card = original {pcCardDef = (getCardDef original)
            { cdCost = Just 0
            , cdAbilityCost = ExhaustCharacterWithTraitCost Avenger <> ExhaustCharacterWithTraitCost Guardian
            }}
      withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
        PlayerIdentity a {playerIdentityHand = Hand [card]}
      choose "play the alliance card" (isPlay card)
      fmap fst <$> asked `shouldReturn` Just avenger
      choose "Captain Marvel agrees" (isLabel "$label.contributeToCost")
      fmap fst <$> asked `shouldReturn` Just guardian
      choose "Rocket agrees" (isLabel "$label.contributeToCost")
      fmap fst <$> asked `shouldReturn` Just avenger
      choose "Captain Marvel contributes herself" (isTargetChoice avenger)
      -- Rocket has only himself available, so his already-approved exhaustion is automatic.
      chooseVillain
      pass
      playerIdentityExhausted . toAttrs <$> getPlayer avenger `shouldReturn` True
      playerIdentityExhausted . toAttrs <$> getPlayer guardian `shouldReturn` True
      villainDamageNow `shouldReturn` 8

  it "a sole contributor can refuse after another agrees without exhausting anyone" do
    tg <- testGame ["spider-man", "captain-marvel", "rocket-raccoon"]
    let p = player1 tg
        avenger = player2 tg
        guardian = tg.seats !! 2
    inGame tg do
      resolve [IdentityMessage i (ChangedToForm A) | i <- [p, avenger, guardian]]
      resolve [IdentityMessage p ExhaustedIdentity]
      clearHand p
      clearHand avenger
      original <- allianceAttack p
      let card = original {pcCardDef = (getCardDef original)
            { cdCost = Just 1
            , cdAbilityCost = ExhaustCharacterWithTraitCost Avenger <> ExhaustCharacterWithTraitCost Guardian
            }}
      withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
        PlayerIdentity a {playerIdentityHand = Hand [card]}
      energy <- putInHand avenger "01088"
      choose "play" (isPlay card)
      choose "invite resource help" (isTargetChoice avenger)
      choose "contribute resources" (isPayWith energy)
      fmap fst <$> asked `shouldReturn` Just avenger
      choose "approve Captain Marvel's exhaustion" (isLabel "$label.contributeToCost")
      fmap fst <$> asked `shouldReturn` Just guardian
      choose "Rocket refuses" (isLabel "$label.declineContribution")
      playerIdentityExhausted . toAttrs <$> getPlayer avenger `shouldReturn` False
      playerIdentityExhausted . toAttrs <$> getPlayer guardian `shouldReturn` False
      handOf p >>= (`shouldSatisfy` elem card)
      handOf avenger >>= (`shouldSatisfy` elem energy)
      villainDamageNow `shouldReturn` 0

  it "exhaustion costs require distinct characters even when a character has both traits" do
    tg <- testGame ["star-lord"]
    let p = player1 tg
    inGame tg do
      changeForm p
      -- Star-Lord gives allies Guardian; Vision is an Avenger.
      _ <- putIntoPlay p "01068"
      -- Star-Lord himself is also a ready Guardian, so exhaust him first.
      resolve [IdentityMessage p ExhaustedIdentity]
      canAffordAllianceCost [p] GameRef
        (ExhaustCharacterWithTraitCost Avenger <> ExhaustCharacterWithTraitCost Guardian)
        `shouldReturn` False
