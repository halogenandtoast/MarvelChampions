module Marvel.Hero.MilesMoralesSignatureSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability (HasAbilities)
import Marvel.Card.Side (Side (A, B))
import Marvel.Keyword (Keyword (Steady))
import Marvel.Matchers hiding (ExhaustedIdentity, ExhaustedAlly)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier (Modifier (KeywordModifier), HasModifiersFor (..))
import Marvel.Upgrade.Types (Upgrade (..), IsUpgrade (..))
import Marvel.Villain.Types qualified as Villain
import TestImport

asMiles :: IdentityId -> GameAppT ()
asMiles p = changeForm p >> pass >> clearHand p

playPaying :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
playPaying p code resourceCodes = do
  card <- putInHand p code
  resources <- traverse (putInHand p) resourceCodes
  playWith card resources
  pure card

-- A keyword modifier isolates the status-card distinction without depending on a
-- particular scenario: Steady needs two cards to be stunned, but one is still a card.
data SteadyProbe = SteadyProbe (Attrs Upgrade) Target
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, HasAbilities)
instance IsUpgrade SteadyProbe where
  toUpgradeAttrs (SteadyProbe attrs _) = attrs
instance HasModifiersFor SteadyProbe where
  getModifiersFor _ target (SteadyProbe _ affected) = pure [KeywordModifier Steady | target == affected]
instance RunMessage SteadyProbe where
  runMessage msg (SteadyProbe attrs affected) = (`SteadyProbe` affected) <$> runMessage msg attrs

steadyVillain :: IdentityId -> GameAppT ()
steadyVillain p = do
  card <- putIntoPlay p "01008"
  attrs <- getsGame (toAttrs . fromMaybe (error "missing probe") . HashMap.lookup (upgradeIdOf card) . gameUpgrades)
  vid <- villainIdNow
  withGame_ $ entitiesL . upgradesL . at (upgradeIdOf card) ?~ Upgrade (SteadyProbe attrs $ toRef vid)

spec :: Spec
spec = describe "Miles Morales signature events" do
  describe "Arachnobatics" do
    for_ [(False, False, 2), (True, False, 5), (False, True, 5), (True, True, 8)] \(stunned, confused, amount) ->
      it ("deals " <> show amount <> " damage for the enemy's status cards") do
        tg <- testGame ["spider-man-miles-morales"]
        let p = player1 tg
        inGame tg do
          asMiles p
          vid <- villainIdNow
          resolve $ [VillainMessage vid (VillainStunned GameRef) | stunned]
            <> [VillainMessage vid (VillainConfused GameRef) | confused]
          _ <- playPaying p "27031" ["27031"]
          chooseVillain
          pass
          villainDamageNow `shouldReturn` amount

    it "counts one status card on a Steady enemy even though the enemy is not stunned yet" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        asMiles p
        steadyVillain p
        vid <- villainIdNow
        resolve [VillainMessage vid (VillainStunned GameRef), VillainMessage vid (VillainConfused GameRef)]
        stunned <- Villain.isStunned . toAttrs =<< getVillain
        stunned `shouldBe` False
        _ <- playPaying p "27031" ["27031"]
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 8

    it "is a hero action" do
      tg <- testGame ["spider-man-miles-morales"]
      inGame tg do
        card <- putInHand (player1 tg) "27031"
        _ <- putInHand (player1 tg) "01089"
        refreshTurn
        offers (isPlay card) `shouldReturn` False

  describe "Double Life" do
    for_ [(A, B), (B, A)] \(from, to) ->
      it ("changes from " <> show from <> " to " <> show to <> " and readies with a physical resource") do
        tg <- testGame ["spider-man-miles-morales"]
        let p = player1 tg
        inGame tg do
          resolve [IdentityMessage p (ChangedToForm from), IdentityMessage p ExhaustedIdentity]
          pass
          clearHand p
          _ <- playPaying p "27032" ["27039"]
          pass
          isExhaustedNow p `shouldReturn` False
          (.playerIdentitySide) <$> playerAttrs p `shouldReturn` to

    it "changes form without readying when no physical resource was spent" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        asMiles p
        resolve [IdentityMessage p ExhaustedIdentity]
        _ <- playPaying p "27032" ["27031"]
        pass
        isExhaustedNow p `shouldReturn` True
        (.playerIdentitySide) <$> playerAttrs p `shouldReturn` B

    it "cannot be played a second time in the same round" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- playPaying p "27032" ["27039"]
        pass
        another <- putInHand p "27032"
        _ <- putInHand p "27039"
        refreshTurn
        offers (isPlay another) `shouldReturn` False

  describe "Swing In" do
    it "removes four threat and resolves camouflage when a mental resource was paid" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        asMiles p
        placeMainThreat 5
        before <- mainSchemeThreatNow
        _ <- playPaying p "27033" ["01089"]
        chooseMainScheme
        chooseVillain
        pass
        mainSchemeThreatNow `shouldReturn` before - 4
        (.playerIdentityTough) <$> playerAttrs p `shouldReturn` True
        Villain.villainConfused . toAttrs <$> getVillain `shouldReturn` True

    it "does not resolve camouflage without a mental resource" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        asMiles p
        placeMainThreat 5
        _ <- playPaying p "27033" ["01088"]
        chooseMainScheme
        pass
        (.playerIdentityTough) <$> playerAttrs p `shouldReturn` False
        Villain.villainConfused . toAttrs <$> getVillain `shouldReturn` False

  describe "Web-Shot" do
    it "deals four attack damage then separately selects the target for Venom Blast" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        asMiles p
        minion <- addMinion p Minions.weaponsRunner
        _ <- playPaying p "27034" ["01088"]
        choose "attack the minion" (isTargetChoice minion)
        chooseVillain
        pass
        minionDamageOf minion `shouldReturn` Nothing
        villainDamageNow `shouldReturn` 2
        Villain.villainStunned . toAttrs <$> getVillain `shouldReturn` True

    it "does not resolve Venom Blast without an energy resource" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        asMiles p
        _ <- playPaying p "27034" ["01089"]
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 4
        Villain.villainStunned . toAttrs <$> getVillain `shouldReturn` False

    it "can apply Venom Blast to the same enemy after the attack" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        asMiles p
        _ <- playPaying p "27034" ["01088"]
        chooseVillain
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 6
        Villain.villainStunned . toAttrs <$> getVillain `shouldReturn` True
