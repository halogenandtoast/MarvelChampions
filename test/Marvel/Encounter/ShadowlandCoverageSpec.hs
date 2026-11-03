module Marvel.Encounter.ShadowlandCoverageSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment qualified as Attachment
import Marvel.Environment qualified as Environment
import Marvel.MainScheme qualified as MainScheme
import Marvel.Minion qualified as Minion
import Marvel.Obligation qualified as Obligation
import Marvel.Shadowland.Cards qualified as Cards
import Marvel.SideScheme qualified as SideScheme
import Marvel.Treachery qualified as Treachery
import Marvel.Upgrade qualified as Upgrade
import Marvel.Villain qualified as Villain
import TestImport
import Test.Hspec qualified as H

-- Main-scheme setup/story faces use their paired in-play B face. Every other
-- printed encounter face must have a concrete runtime builder in its own registry.
spec :: Spec
spec = describe "Shadowland encounter coverage" do
  for_ Cards.streetDefinitions $ \def ->
    it ("has a runtime builder for " <> show (toCardCode def)) do
      let code = toCardCode def
          present = case cdCardType def of
            AttachmentType -> HashMap.member code Attachment.allAttachments
            EnvironmentType -> HashMap.member code Environment.allEnvironments
            MainSchemeType -> HashMap.member code MainScheme.allMainSchemes || HashMap.member (flipCardCode code) MainScheme.allMainSchemes
            MinionType -> HashMap.member code Minion.allMinions
            ObligationType -> HashMap.member code Obligation.allObligations
            SideSchemeType -> HashMap.member code SideScheme.allSideSchemes
            TreacheryType -> HashMap.member code Treachery.allTreacheries
            UpgradeType -> HashMap.member code Upgrade.allUpgrades
            VillainType -> HashMap.member code Villain.allVillains
            _ -> False
      present `H.shouldBe` True
