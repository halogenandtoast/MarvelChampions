module Marvel.PublicSpec (spec) where

import Control.Monad (replicateM_)
import Data.ByteString.Lazy qualified as BL
import Marvel.Game.Public (ChoiceView (..), describeChoice, owedFor, publicGame)
import Marvel.Resource.Types (Resource (..))
import System.Environment (lookupEnv)
import TestImport

spec :: Spec
spec = describe "the game as the client sees it" do
  it "describes a dealt table, and is written out when SAMPLE_VIEW names a file" do
    let setup = fromMaybe (error "no setup") (recommendedSetup "01094")
    (app, _) <- dealtGame setup [starter "spider-man", starter "captain-marvel"] 3
    -- the player order, then a little way into the first turn
    runGameApp app $ replicateM_ 3 (openQuestion >>= traverse_ (\(ident, _) -> answerQuestion ident 0))
    view <- publicGame =<< runGameApp app getGame
    lookupEnv "SAMPLE_VIEW" >>= traverse_ \path -> BL.writeFile path (encode view)
    case view of
      Object _ -> pure ()
      _ -> expectationFailure "the view is not an object"

  it "says what a cost still needs once resources are paid" $ id @(IO ()) do
    owedFor [Just Physical, Nothing, Nothing] [] `shouldBe` [Just Physical, Nothing, Nothing]
    owedFor [Just Physical, Nothing, Nothing] [Energy] `shouldBe` [Just Physical, Nothing]
    owedFor [Just Physical, Nothing] [Wild, Mental] `shouldBe` []
    owedFor [Nothing] [Physical, Mental] `shouldBe` []

  it "names a choice by what it does to what it is offered on" do
    tg <- testGame ["spider-man", "she-hulk"]
    inGame tg do
      [p1, p2] <- players
      let damage = damageIdentity p1 GameRef (toDamage 1 FromTreachery)
      (describeChoice p1 (TargetLabel (IdentityRef p1) [Run damage])).kind `shouldBe` "damage"
      -- the player chosen is not what is damaged
      (describeChoice p1 (TargetLabel (IdentityRef p2) [Run damage])).kind `shouldBe` "target"
