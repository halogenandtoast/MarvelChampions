module Marvel.Rules.SteadyIdentitySpec (spec) where

import Data.Aeson (Result (..), Value (Object), fromJSON, toJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability (HasAbilities)
import Marvel.Identity (identityIsStunned, identityIsConfused)
import Marvel.Keyword (Keyword (Steady))
import Marvel.Modifier (Modifier (KeywordModifier), HasModifiersFor (..))
import Marvel.Status (StatusCard (StunnedStatus))
import Marvel.Upgrade.Types (Upgrade (..), IsUpgrade (..))
import TestImport

data SteadyIdentityProbe = SteadyIdentityProbe (Attrs Upgrade) IdentityId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, HasAbilities)
instance IsUpgrade SteadyIdentityProbe where
  toUpgradeAttrs (SteadyIdentityProbe attrs _) = attrs
instance HasModifiersFor SteadyIdentityProbe where
  getModifiersFor _ target (SteadyIdentityProbe _ who) = pure [KeywordModifier Steady | target == toRef who]
instance RunMessage SteadyIdentityProbe where
  runMessage msg (SteadyIdentityProbe attrs who) = (`SteadyIdentityProbe` who) <$> runMessage msg attrs

steadyIdentity :: IdentityId -> GameAppT UpgradeId
steadyIdentity who = do
  card <- putIntoPlay who "01008"
  let uid = upgradeIdOf card
  attrs <- getsGame (toAttrs . fromMaybe (error "missing probe") . HashMap.lookup uid . gameUpgrades)
  withGame_ $ entitiesL . upgradesL . at uid ?~ Upgrade (SteadyIdentityProbe attrs who)
  pure uid

spec :: Spec
spec = describe "Steady identities" do
  it "requires two status cards and removes both when an attack is canceled" do
    tg <- testGame ["spider-man"]
    let who = player1 tg
    inGame tg do
      changeForm who
      pass
      _ <- steadyIdentity who
      resolve [IdentityMessage who IdentityStunned]
      (identityIsStunned =<< getIdentity who) `shouldReturn` False
      (.playerIdentityStunned) <$> playerAttrs who `shouldReturn` True
      resolve [IdentityMessage who IdentityStunned]
      (identityIsStunned =<< getIdentity who) `shouldReturn` True
      event <- putInHand who "01005"
      energy <- putInHand who "01088"
      mental <- putInHand who "01007"
      playWith event [energy, mental]
      pass
      villainDamageNow `shouldReturn` 0
      attrs <- playerAttrs who
      (playerIdentityStunned attrs, playerIdentityAdditionalStun attrs) `shouldBe` (False, False)

  it "removes one card for an ordinary removal and keeps one effective card when steady is lost" do
    tg <- testGame ["spider-man"]
    let who = player1 tg
    inGame tg do
      uid <- steadyIdentity who
      resolve [IdentityMessage who IdentityStunned, IdentityMessage who IdentityStunned]
      resolve [RemoveStatus (toRef who) StunnedStatus]
      attrs <- playerAttrs who
      (playerIdentityStunned attrs, playerIdentityAdditionalStun attrs) `shouldBe` (True, False)
      (identityIsStunned =<< getIdentity who) `shouldReturn` False
      resolve [RemoveFromPlay $ toRef uid]
      (identityIsStunned =<< getIdentity who) `shouldReturn` True

  it "similarly requires two confuse cards and loads older saves with neither extra status" do
    tg <- testGame ["spider-man"]
    let who = player1 tg
    inGame tg do
      _ <- steadyIdentity who
      resolve [IdentityMessage who IdentityConfused]
      (identityIsConfused =<< getIdentity who) `shouldReturn` False
      resolve [IdentityMessage who IdentityConfused]
      (identityIsConfused =<< getIdentity who) `shouldReturn` True
      attrs <- playerAttrs who
      let old = case toJSON attrs of
            Object o -> Object $ KeyMap.delete "additionalStun" $ KeyMap.delete "additionalConfuse" o
            value -> value
      case fromJSON old of
        Error err -> fail err
        Success loaded -> do
          playerIdentityAdditionalStun loaded `shouldBe` False
          playerIdentityAdditionalConfuse loaded `shouldBe` False
