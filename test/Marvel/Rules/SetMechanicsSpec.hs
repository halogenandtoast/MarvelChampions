-- | The engine mechanics the Green Goblin, Wrecking Crew, Rise of Red Skull and Once and
-- Future Kang sets build on, each as the rules have it.
module Marvel.Rules.SetMechanicsSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.MainScheme.Types (Field (MainSchemeCounters))
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
