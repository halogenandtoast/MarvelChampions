module Marvel.Rules.AbilityWindowSnapshotSpec (spec) where

import Data.Aeson (Result (..), fromJSON, toJSON, Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Cost.Types (Cost (NoCost))
import Marvel.Payment.Types (Payment (NoPayment))
import Marvel.ActiveCost.Types
import Marvel.Choice qualified as Choice
import Marvel.Environment.Environments.SmVenom (bellTowers)
import Marvel.Window qualified as W
import TestImport

spec :: Spec
spec = describe "ability triggering window snapshots" do
  it "gives old and source-aware handlers their own triggering payload from one occurrence" do
    tg <- testGameAgainst "27073" ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      plating <- putIntoPlay you "16162"
      eid <- fromMaybe (error "Bell Tower missing") . headMay <$> bellTowers
      resolve [PlaceCounters (EnvironmentRef eid) 3, RemoveCounters (EnvironmentRef eid) 2]
      vid <- villainIdNow
      let damage = toDamage 3 FromAttack
          old = W.IdentityTakeDamage you damage
          sourced = W.IdentityTakeDamageFrom you (VillainRef vid) damage
      withGame_ $ windowsL .~ [[W.Window W.When old, W.Window W.When sourced]]
      abilities <- getAbilitiesFor you
      let findAbility source index = fromMaybe (error "ability missing") $ headMay [a | a <- abilities, abilitySource a == source, abilityIndex a == index]
          snapshot ability = Choice.choiceMessages you (UseAbility ability) >>= \case
            [SetActiveCost cost] -> pure $ activeCostAbilityWindows cost
            _ -> error "expected ability payment"
      snapshot (findAbility (UpgradeRef $ upgradeIdOf plating) 1) `shouldReturn` Just [old]
      snapshot (findAbility (EnvironmentRef eid) 2) `shouldReturn` Just [sourced]
  it "loads an old payment without a triggering-window snapshot" do
    tg <- testGame ["spider-man"]
    inGame tg do
      acid <- getRandom
      let cost = ActiveCost acid (player1 tg) ForTreachery NoCost NoPayment Nothing [] Nothing
          older = case toJSON cost of Object o -> Object $ KeyMap.delete "activeCostAbilityWindows" o; _ -> error "cost JSON"
      case fromJSON older of
        Success loaded -> liftIO $ activeCostAbilityWindows loaded `shouldBe` Nothing
        Error err -> error err
