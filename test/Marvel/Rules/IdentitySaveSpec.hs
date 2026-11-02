module Marvel.Rules.IdentitySaveSpec (spec) where

import Data.UUID qualified as UUID
import Marvel.AlterEgo (lookupAlterEgoByCardCode)
import Marvel.Hero (lookupHeroByCardCode)
import Test.Hspec qualified as Hspec
import TestImport

spec :: Spec
spec = describe "identity saves" do
  for_ ["01001", "56001", "56029", "60001", "60037"] $ \code -> do
    it ("reloads both identity forms: " <> show code) do
      let you = IdentityId UUID.nil
          hero = lookupHeroByCardCode (CardCode $ code <> "a") you
          alterEgo = lookupAlterEgoByCardCode (CardCode $ code <> "b") you
      fromJSON (toJSON hero) `Hspec.shouldBe` Success hero
      fromJSON (toJSON alterEgo) `Hspec.shouldBe` Success alterEgo

  it "reloads a Daredevil game at the main scheme setup prompt and after setup" do
    (app, _) <- dealtGameBeforeSetup id (ScenarioSetup "01094" [] Normal) [starter "daredevil"] 1 (const $ pure ())
    runGameApp app do
      before <- getGame
      case fromJSON @Game (toJSON before) of
        Error err -> liftIO $ expectationFailure err
        Success restored -> do
          toJSON restored `shouldBe` toJSON before
          liftIO $ writeIORef app.appGame restored
          pressMainSchemeSetup
          toFirstTurn
          after <- getGame
          case fromJSON @Game (toJSON after) of
            Error err -> liftIO $ expectationFailure err
            Success reloaded -> toJSON reloaded `shouldBe` toJSON after
