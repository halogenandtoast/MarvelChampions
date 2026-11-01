module Marvel.Rules.PlayerSideSchemesSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Card (toCard)
import Marvel.SideScheme.Types (SideScheme)
import Marvel.SideScheme.Types qualified as SideScheme
import TestImport

schemes :: GameAppT [SideScheme]
schemes = getsGame (HashMap.elems . gameSideSchemes)

spec :: Spec
spec = describe "player side schemes" do
  it "plays from hand with per-player threat and retains its physical card and owner" do
    tg <- testGame ["spider-man", "captain-marvel"]
    inGame tg do
      let p = player1 tg
      clearHand p
      card <- putInHand p "40020"
      energy <- putInHand p "01088"
      playWith card [energy]
      board <- schemes
      length board `shouldBe` 1
      let scheme = testHead board
      SideScheme.sideSchemeThreat (toAttrs scheme) `shouldBe` 4
      toCard scheme `shouldBe` PlayerCard card

  it "the first player discards an excess scheme without defeating it" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      first <- putIntoPlay p "40020"
      second <- putInHand p "40054"
      playWith second []
      choose "discard the older scheme" (isTargetChoice $ SideSchemeId $ unCardId $ pcCardId first)
      map (toCardCode . getCardDef) <$> schemes `shouldReturn` ["40054"]
      map toCardCode <$> discardOf p `shouldReturn` ["40020"]
      getPlayerVictoryDisplay `shouldReturn` []

  it "victory keeps the actual player card out of both discard piles" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      card <- putIntoPlay p "40020"
      resolve [AddToVictoryDisplay $ SideSchemeRef $ SideSchemeId $ unCardId $ pcCardId card]
      getPlayerVictoryDisplay `shouldReturn` [card]
      discardOf p `shouldReturn` []
      encounterDiscard `shouldReturn` []
      sideSchemesInVictoryDisplay `shouldReturn` 1

  it "the campaign scheme does not consume the player side scheme limit" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      _ <- putIntoPlay p "40190a"
      _ <- putIntoPlay p "40020"
      length <$> schemes `shouldReturn` 2

  it "three starting players may keep two player side schemes" do
    tg <- testGame ["spider-man", "captain-marvel", "iron-man"]
    inGame tg do
      _ <- putIntoPlay (player1 tg) "40020"
      _ <- putIntoPlay (player2 tg) "40054"
      length <$> schemes `shouldReturn` 2
