module Marvel.Hero.NickFurySpec (spec) where
import Marvel.AgentsOfShield.PlayerHelpers
import Marvel.Message.Lifted qualified as L
import TestImport

spec :: Spec
spec = describe "Nick Fury" do
  it "sets up Assault, with ten hit points and six alter ego hand size" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      hitPointsOf p `shouldReturn` 10
      handSizeOf p `shouldReturn` 6
      isStealth p `shouldReturn` False
      suitForm p >>= (`shouldSatisfy` isJust)

  it "changes suit without spending the normal identity form change" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      choose "Infiltrate" \case UseAbility a -> abilitySource a == IdentityRef p && abilityIndex a == 1; _ -> False
      isStealth p `shouldReturn` True
      changeForm p
      pass
      handSizeOf p `shouldReturn` 5
      isStealth p `shouldReturn` True

  it "keeps suit threat across suit and identity form changes" do
    tg <- testGame ["nick-fury"]
    inGame tg do
      let p = player1 tg
      L.runQueueT (L.capture $ placeSuitThreat p 3) >>= start
      pass
      L.runQueueT (L.capture $ changeSuit p True) >>= start
      pass
      suitThreat p `shouldReturn` 3
      changeForm p
      pass
      suitThreat p `shouldReturn` 3
