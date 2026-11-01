module Marvel.Rules.MutantGenesisMechanicsSpec (spec) where

import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Identity.Types qualified as Identity
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Status
import TestImport

countTough :: IdentityId -> GameAppT Natural
countTough p = do
  a <- playerAttrs p
  pure $ (if a.playerIdentityTough then 1 else 0) + a.playerIdentityAdditionalTough

spec :: Spec
spec = describe "Mutant Genesis engine mechanics" do
  it "retains both tough cards through zero damage, including zero piercing damage" do
    tg <- testGame ["colossus"]
    let p = player1 tg
    inGame tg do
      changeForm p
      pass
      resolve [GiveStatus (IdentityRef p) ToughStatus, GiveStatus (IdentityRef p) ToughStatus]
      countTough p `shouldReturn` 2
      resolve [IdentityMessage p $ IdentityDamaged GameRef $ toDamage 0 FromAbility]
      resolve [IdentityMessage p $ IdentityDamaged GameRef $ withPiercing $ toDamage 0 FromAbility]
      countTough p `shouldReturn` 2
      damageOf p `shouldReturn` 0

  it "piercing clears both tough cards and deals damage as a single instance" do
    tg <- testGame ["colossus"]
    let p = player1 tg
    inGame tg do
      changeForm p
      pass
      resolve [GiveStatus (IdentityRef p) ToughStatus, GiveStatus (IdentityRef p) ToughStatus]
      resolve [IdentityMessage p $ IdentityDamaged GameRef $ withPiercing $ toDamage 3 FromAbility]
      countTough p `shouldReturn` 0
      damageOf p `shouldReturn` 3

  it "does not remove an existing additional tough merely by changing to alter ego" do
    tg <- testGame ["colossus"]
    let p = player1 tg
    inGame tg do
      changeForm p
      pass
      resolve [GiveStatus (IdentityRef p) ToughStatus, GiveStatus (IdentityRef p) ToughStatus]
      resolve [IdentityMessage p $ ChangedToForm B]
      resolve [GiveStatus (IdentityRef p) ToughStatus]
      countTough p `shouldReturn` 2

  it "loads identities saved before additional tough statuses were introduced" do
    tg <- testGame ["spider-man"]
    inGame tg do
      a <- playerAttrs (player1 tg)
      let old = case toJSON a of
            Object o -> Object $ KeyMap.delete "additionalTough" o
            other -> other
      case fromJSON old of
        Error err -> fail err
        Success restored -> Identity.playerIdentityAdditionalTough restored `shouldBe` 0

  it "places, reads and removes counters through a scheme ID wrapper" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- mainSchemeIdNow
      messages <- Lifted.runQueueT $ Lifted.capture $ Lifted.placeCounters (SchemeMainSchemeId mid) 2
      resolve messages
      getCountersOn mid `shouldReturn` 2
      getCountersOn (SchemeMainSchemeId mid) `shouldReturn` 2
      removed <- Lifted.runQueueT $ Lifted.capture $ Lifted.removeCounters (SchemeMainSchemeId mid) 1
      resolve removed
      getCountersOn mid `shouldReturn` 1
