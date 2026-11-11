module Marvel.Rules.PlayerCardTargetsSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Criteria (Criteria (NoCriteria))
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero p = do
  changeForm p
  pass
  clearHand p
  sides <- getsGame (HashMap.keys . gameSideSchemes)
  resolve [RemoveFromPlay $ SideSchemeRef sid | sid <- sides]

fund :: IdentityId -> GameAppT ()
fund p = traverse_ (putInHand p) ["01088", "01088", "01088", "01088"]

spec :: Spec
spec = describe "Player card target restrictions" do
  for_ [("captain-marvel", "01012"), ("hulk", "10004"), ("spider-man", "10019"), ("doctor-strange", "09003"), ("shadowcat", "32039"), ("wolverine", "35011"), ("cable", "40003")] $ \(hero, code) ->
    it ("requires removable threat for " <> show (code :: CardCode) <> " but permits a confused thwart attempt") do
      tg <- testGame [hero]
      inGame tg do
        let p = player1 tg
        asHero p
        event <- putInHand p code
        fund p
        mainSchemeThreatNow `shouldReturn` 0
        offers (isPlay event) `shouldReturn` False
        resolve [IdentityMessage p IdentityConfused]
        offers (isPlay event) `shouldReturn` True
        resolve [IdentityMessage p IdentityRemoveConfused]
        placeMainThreat 3
        offers (isPlay event) `shouldReturn` True

  it "discards only one of a steady hero's two stun cards" do
    tg <- testGame ["drax"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "31024"
      resolve [IdentityMessage p IdentityStunned, IdentityMessage p IdentityStunned]
      before <- playerAttrs p
      before.playerIdentityAdditionalStun `shouldBe` True
      event <- putInHand p "19021"
      resource <- putInHand p "01089"
      playWith event [resource]
      a <- playerAttrs p
      a.playerIdentityStunned `shouldBe` True
      a.playerIdentityAdditionalStun `shouldBe` False

  it "requires main-scheme threat for Running Interference" do
    tg <- testGame ["wasp"]
    inGame tg do
      let p = player1 tg
      asHero p
      event <- putInHand p "13031"
      fund p
      offers (isPlay event) `shouldReturn` False
      placeMainThreat 2
      offers (isPlay event) `shouldReturn` True

  it "does not offer Invulnerability when the hero already has tough" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      asHero p
      event <- putInHand p "06021"
      fund p
      offers (isPlay event) `shouldReturn` True
      resolve [IdentityMessage p IdentityBecomeTough]
      offers (isPlay event) `shouldReturn` False

  it "still permits Invulnerability when Colossus can hold another tough card" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      asHero p
      event <- putInHand p "06021"
      fund p
      offers (isPlay event) `shouldReturn` True

  it "requires an exhausted Tech upgrade for Reload" do
    tg <- testGame ["rocket-raccoon"]
    inGame tg do
      let p = player1 tg
      asHero p
      event <- putInHand p "16031"
      fund p
      offers (isPlay event) `shouldReturn` False
      tech <- putIntoPlay p "16038"
      offers (isPlay event) `shouldReturn` False
      resolve [UpgradeMessage (upgradeIdOf tech) ExhaustedUpgrade]
      offers (isPlay event) `shouldReturn` True

  it "requires Venom or a Weapon to be readyable for Run and Gun" do
    tg <- testGame ["venom"]
    inGame tg do
      let p = player1 tg
      asHero p
      event <- putInHand p "20005"
      fund p
      offers (isPlay event) `shouldReturn` False
      resolve [IdentityMessage p ExhaustedIdentity]
      offers (isPlay event) `shouldReturn` True

  it "does not offer a saved Athletic Conditioning with obsolete unrestricted criteria" do
    tg <- testGame ["drax"]
    inGame tg do
      let p = player1 tg
      asHero p
      current <- putInHand p "19021"
      let legacy = current {pcCardDef = (getCardDef current) {cdCriteria = NoCriteria}}
      withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
        PlayerIdentity a {playerIdentityHand = Hand $ legacy : filter ((/= pcCardId current) . pcCardId) (unHand a.playerIdentityHand)}
      fund p
      offers (isPlay legacy) `shouldReturn` False
      resolve [IdentityMessage p IdentityStunned]
      offers (isPlay legacy) `shouldReturn` True

  it "discards a single confuse card from a steady Asgardian with Godlike Stamina" do
    tg <- testGame ["thor"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "31024"
      resolve [IdentityMessage p IdentityConfused]
      event <- putInHand p "25024"
      playWith event []
      choose "discard confused" (isLabel "$label.confusedStatus")
      (.playerIdentityConfused) <$> playerAttrs p >>= (`shouldBe` False)
