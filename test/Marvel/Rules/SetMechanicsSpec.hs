-- | The engine mechanics the Green Goblin, Wrecking Crew, Rise of Red Skull and Once and
-- Future Kang sets build on, each as the rules have it.
module Marvel.Rules.SetMechanicsSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Hp
import Marvel.MainScheme.Types (Field (MainSchemeCounters))
import Marvel.Villain.Types (flipVillainTo, hpL, maxHpL, stageL, stunnedL, toVillainAttrs)
import Marvel.Villain.Villains.Rhino (rhino2)
import TestImport

spec :: Spec
spec = describe "set mechanics" do
  it "counters are placed on and removed from the main scheme" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- mainSchemeId
      resolve [PlaceCounters (toTarget mid) 3, RemoveCounters (toTarget mid) 1]
      counters <- getsGame (map (field MainSchemeCounters) . HashMap.elems . gameMainSchemes)
      liftIO $ counters `shouldBe` [2]

  it "a flipped villain keeps its damage, stage and status cards, its card turned over" do
    tg <- testGame ["spider-man"]
    inGame tg do
      v <- getVillain
      let attrs = toAttrs v & hpL .~ HP 5 & stunnedL .~ True
          flipped = toVillainAttrs (flipVillainTo rhino2 attrs)
      liftIO do
        unHp (flipped ^. hpL) `shouldBe` 5
        unHp (flipped ^. maxHpL) `shouldBe` unHp (attrs ^. maxHpL)
        flipped ^. stunnedL `shouldBe` True
        flipped ^. stageL `shouldBe` 1
        toCardCode (getCardDef flipped) `shouldBe` "01095"
