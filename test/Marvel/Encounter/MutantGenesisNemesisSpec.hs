module Marvel.Encounter.MutantGenesisNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.SideScheme.Types qualified as SideScheme
import Marvel.Status
import TestImport

spec :: Spec
spec = describe "Mutant Genesis nemeses" do
  it "Juggernaut enters tough and ignores stun because of stalwart" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Minions.juggernaut
      resolve [GiveStatus (MinionRef mid) StunnedStatus]
      a <- getsGame (toAttrs . fromMaybe (error "Juggernaut missing") . HashMap.lookup mid . gameMinions)
      minionTough a `shouldBe` True
      minionStunned a `shouldBe` False

  it "White Queen continuously confuses her engaged player" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      _ <- addMinion p Minions.whiteQueenMutantGenesis
      (.playerIdentityConfused) <$> playerAttrs p `shouldReturn` True
      runNow [RemoveStatus (IdentityRef p) ConfusedStatus]
      (.playerIdentityConfused) <$> playerAttrs p `shouldReturn` True

  it "Telepathic Restraint stuns the attached identity" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Attachments.telepathicRestraint
      pass
      (.playerIdentityStunned) <$> playerAttrs p `shouldReturn` True

  it "Slammed stuns a player who was not already stunned" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Treacheries.slammed
      pass
      (.playerIdentityStunned) <$> playerAttrs p `shouldReturn` True
      damageOf p `shouldReturn` 0

  it "Slammed instead deals two damage to an already stunned player" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      runNow [GiveStatus (IdentityRef p) StunnedStatus]
      _ <- revealNow p Treacheries.slammed
      pass
      damageOf p `shouldReturn` 2

  it "Rampaging Juggernaut counts each tough status discarded from a character" do
    tg <- testGame ["colossus"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      runNow [GiveStatus (IdentityRef p) ToughStatus, GiveStatus (IdentityRef p) ToughStatus]
      _ <- revealNow p SideSchemes.rampagingJuggernaut
      pass
      (.playerIdentityTough) <$> playerAttrs p `shouldReturn` False
      (.playerIdentityAdditionalTough) <$> playerAttrs p `shouldReturn` 0
      threat <- getsGame $ sum . map (SideScheme.getSideSchemeThreat) . filter ((== "32027") . toCardCode . getCardDef) . HashMap.elems . gameSideSchemes
      threat `shouldBe` 6

  it "White Queen's confused card remains when she leaves play" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Minions.whiteQueenMutantGenesis
      resolve [RemoveFromPlay (MinionRef mid)]
      (.playerIdentityConfused) <$> playerAttrs p `shouldReturn` True

  it "Hellfire Pawn's guard stops Solid Shadowcat from attacking the villain" do
    tg <- testGame ["shadowcat"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      _ <- addMinion p Minions.hellfirePawn
      choose "basic attack" (isHeroAttack p)
      vid <- villainIdNow
      offers (isTargetChoice vid) `shouldReturn` False
