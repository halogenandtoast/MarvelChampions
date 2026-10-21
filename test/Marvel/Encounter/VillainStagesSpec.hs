module Marvel.Encounter.VillainStagesSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Card.Code
import Marvel.Card.Def (getCardDef)
import Marvel.Damage
import Marvel.Entity
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.Villain.Types (Villain, villainIsTough)
import Marvel.Villain.Types qualified as Villain
import TestImport

theVillain :: GameAppT Villain
theVillain =
  getsGame (HashMap.elems . gameVillains) >>= \case
    [v] -> pure v
    vs -> error $ "expected one villain, found " <> show (length vs)

-- | Damage enough to defeat any stage.
defeat :: Villain -> Message
defeat v = VillainMessage (toId v) $ VillainDamaged GameRef (toDamage 1000 FromAbility)

finished :: GameAppT (Maybe FinishedStatus)
finished = getsGame $ \g -> case gameState g of
  Finished status -> Just status
  _ -> Nothing

rhino :: Difficulty -> IO GameApp
rhino difficulty = do
  let setup = (fromMaybe (error "no setup") (recommendedSetup "01094")) {difficulty}
  (app, _) <- dealtGame setup [starter "spider-man"] 1
  runGameApp app toFirstTurn
  pure app

spec :: Spec
spec = describe "villain stages" do
  it "standard: Rhino I advances to Rhino II, and defeating Rhino II wins" do
    app <- rhino Normal
    runGameApp app do
      v1 <- theVillain
      liftIO $ toCardCode (getCardDef v1) `shouldBe` "01094"
      resolve [defeat v1]
      v2 <- theVillain
      liftIO $ toCardCode (getCardDef v2) `shouldBe` "01095"
      liftIO . (`shouldBe` 2) . Villain.villainStage $ toAttrs v2
      -- Rhino II's When Revealed finds Breakin' & Takin'
      sideSchemes <- getsGame (map getCardDef . HashMap.elems . gameSideSchemes)
      liftIO $ sideSchemes `shouldSatisfy` elem Cards.breakinAndTakin
      finished >>= liftIO . (`shouldBe` Nothing)
      resolve [defeat v2]
      finished >>= liftIO . (`shouldBe` Just Won)

  it "expert: the villain starts at Rhino II, whose When Revealed resolves at setup, and Rhino III is the last stage" do
    app <- rhino Expert
    runGameApp app do
      v2 <- theVillain
      liftIO $ toCardCode (getCardDef v2) `shouldBe` "01095"
      sideSchemes <- getsGame (map getCardDef . HashMap.elems . gameSideSchemes)
      liftIO $ sideSchemes `shouldSatisfy` elem Cards.breakinAndTakin
      resolve [defeat v2]
      v3 <- theVillain
      liftIO $ toCardCode (getCardDef v3) `shouldBe` "01096"
      -- toughness: the first damage only takes the tough status card
      liftIO $ villainIsTough v3 `shouldBe` True
      resolve [defeat v3]
      finished >>= liftIO . (`shouldBe` Nothing)
      theVillain >>= resolve . pure . defeat
      finished >>= liftIO . (`shouldBe` Just Won)
